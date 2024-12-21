-- | Define a file which can be used to cache tuples for arbitrary, flat tuple sets.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProjectM36.Cache.Tuple where
import ProjectM36.Base
import ProjectM36.Serialise.Base ()
import ProjectM36.RelExprSize
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Network.ByteOrder
import Data.Time.Clock
import Codec.Winery as W
import System.IO
import GHC.Generics
import Control.Monad (when, foldM)
import Control.Exception
import qualified Streamly.Data.Stream.Prelude as SP
import qualified Streamly.Internal.Data.Stream as SD
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Bifunctor
import Data.Proxy

{- file format for tuple cache
-bytecount for info at end of file
TupleCacheInfo

-use blockSizes to get the next block of bytes
-}
type FileMagic = BS.ByteString

newtype InvalidFileMagicException = InvalidFileMagicException BS.ByteString deriving (Show)
newtype TruncatedFileException = TruncatedFileException ByteCount deriving (Show)
data SerialisationSchemaMismatch = SerialisationSchemaMismatch deriving (Show)

instance Exception InvalidFileMagicException
instance Exception TruncatedFileException
instance Exception SerialisationSchemaMismatch

fileMagic :: FileMagic
fileMagic = "PM36CacheTuple_v000"

type ByteOffset = ByteCount

data TupleCacheInfo =
  TupleCacheInfo { blockSizes :: V.Vector (ByteOffset, ByteCount),
                   representing :: PinnedRelationalExpr,
                   created :: UTCTime,
                   tupleSchema :: W.Schema -- the schema is shared amongst all tuples
                 }
  deriving (Generic, Show)
  deriving Serialise via WineryRecord TupleCacheInfo

blockCount :: TupleCacheInfo -> Int
blockCount i = V.length (blockSizes i)

readByteCount :: Handle -> IO Int
readByteCount h = do
  bytes <- BS.hGet h 8
  when (BS.length bytes /= 8) $ throw (TruncatedFileException 8)
  pure (fromIntegral $ word64 bytes)

-- | Runs checks such as confirming that the file size can accommodate the blockSizes in the cache info, but without reading the entire file so that this can be done routinely.
validateQuickly :: Handle -> IO ()
validateQuickly = undefined

--read and deserialize block of bytes prefixed by a byte count
readBlock :: Serialise a => Handle -> IO a
readBlock h = do
  bc <- readByteCount h
  bytes <- BS.hGetNonBlocking h bc
  when (BS.length bytes /= bc) $ throw (InvalidFileMagicException bytes)
  case deserialise bytes of
    Left err -> throw err
    Right val -> pure val
  
-- | get the cache info from a tuple cache file, may throw IO exception or InvalidFileMagicException
readCacheInfo :: Handle -> IO TupleCacheInfo
readCacheInfo h = do
  --look for magic
  potentialMagic <- BS.hGetNonBlocking h (BS.length fileMagic)
  when (potentialMagic /= fileMagic) $ throw (InvalidFileMagicException potentialMagic)
  readBlock h 

-- since this is just for caching, IO errors can be logged and ignored due to cache creation failure
writeTupleStream :: Handle -> PinnedRelationalExpr -> ByteCount -> [RelationTuple] -> IO ()
writeTupleStream h expr groupSize tuples = do
  --how can I write the tuple info if I only have a stream of the tuples :/ should the tuple info be at the back of the file then?
  --create slices and record sizes for metadata
  --write tuple info
  --write metadata plus metadata size at end
  now <- getCurrentTime
  BS.hPutStr h fileMagic
  let writeTupleBlock tuples' = do
          let tupleListBytes = serialiseOnly tuples'
              bytesCount = fromIntegral (BS.length tupleListBytes)
          BS.hPutStr h tupleListBytes
          pure bytesCount
      tupleBlockWriter (accsize, offset', accTuples, metadata) nextTuple = do
        -- add a new grouping, if we go over the size limit
        let estimatedTupleSize = size nextTuple
        if accsize + estimatedTupleSize >= groupSize then do
          tupleListByteCount <- writeTupleBlock (accTuples <> [nextTuple])
          pure (0,
                offset' + tupleListByteCount,
                [],
                metadata <> [(offset', tupleListByteCount)])
          else do
          pure (accsize + estimatedTupleSize,
                offset',
                accTuples <> [nextTuple],
                metadata)
      writeFinalBlock x@(_, _, [], _) = pure x
      writeFinalBlock (acc, offset', accTuples, metadata) = do
        bytesWritten <- writeTupleBlock accTuples
        pure (acc, offset', [], metadata <> [(offset', bytesWritten)])
        
  (_, _, _, blockSizeMetadata) <- foldM tupleBlockWriter (0,0,mempty,mempty) tuples >>= writeFinalBlock
  --write remaining tuples which didn't get get us over the last threshold
  --create block metadata at end of file, offset by file magic at beginning of file
  let tupleCacheInfo = TupleCacheInfo { blockSizes = V.fromList (map (first offsetByMagic) blockSizeMetadata),
                                        representing = expr,
                                        created = now,
                                        tupleSchema = schema (Proxy :: Proxy RelationTuple)
                                        }
      offsetByMagic = (+) (fromIntegral (BS.length fileMagic))
      tInfoData = serialise tupleCacheInfo
  BS.hPutStr h tInfoData
  BS.hPutStr h (bytestring64 (fromIntegral (BS.length tInfoData)))
  pure ()

readTupleStream :: MonadIO m => Handle -> SP.Stream m RelationTuple
readTupleStream h = SD.unCross $ do
  --skip filemagic
  --jump to end to read tuple cache info
  --deserialise blocks from metadata
  tupleCacheInfo <- liftIO $ do
    potentialMagic <- BS.hGetNonBlocking h (BS.length fileMagic)
    when (potentialMagic /= fileMagic) $ throw (InvalidFileMagicException potentialMagic)
    --get size of tuple cache info
    hSeek h SeekFromEnd (-8)
    infoSize <- readByteCount h
    hSeek h SeekFromEnd (-(fromIntegral (8 + infoSize)))
    tcacheInfo <- deserialise <$> BS.hGet h infoSize
    case tcacheInfo of
      Left err -> throw err
      Right info -> do
        when (tupleSchema info /= schema (Proxy :: Proxy RelationTuple)) $ throw SerialisationSchemaMismatch
        --print (blockSizes info)
        pure info
  let readTupleBlock (offset', byteLength) = liftIO $ do
--        print ("readTupleBlock", offset', byteLength)
        hSeek h AbsoluteSeek (fromIntegral offset')
        tuples <- deserialiseOnly' <$> BS.hGet h (fromIntegral byteLength)
        case tuples of
          Left err -> throw err
          Right tuples' -> do
--            print ("readTupleBlock", tuples')
            pure $ SP.fromList tuples'
  SD.mkCross $ SP.concatMapM readTupleBlock (SP.fromList (V.toList (blockSizes tupleCacheInfo)))
  
deserialiseOnly' :: forall s. Serialise s => BS.ByteString -> Either WineryException s
deserialiseOnly' bytes = do
  dec <- getDecoder (schema (Proxy :: Proxy s))
  pure (evalDecoder dec bytes)

    
          
