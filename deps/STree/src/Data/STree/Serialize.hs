{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Reading and writing a tree to disk.
--
-- A tree is one flat array plus a handful of numbers that all follow from its
-- size, so the file is a fixed header followed by the array verbatim — there is
-- no structure to encode, nothing to traverse, and no per-element work beyond
-- the copy. Loading does not redo the @O(n log_B n)@ permutation; it validates
-- the header, reads the payload and recomputes the geometry.
--
-- == The file
--
-- > offset  size  field
-- > 0       8     magic "STREE\0\0\0"
-- > 8       2     format version (u16 LE)
-- > 10      1     key type tag ('keyTag')
-- > 11      1     node width l
-- > 12      4     flags (u32 LE; bit 0: payload is little-endian)
-- > 16      8     number of keys, n (u64 LE)
-- > 24      8     payload length in bytes (u64 LE)
-- > 32      8     payload checksum, 0 if absent (u64 LE)
-- > 40      24    reserved, zero
-- > 64      ...   the layout array verbatim, sentinel padding included
--
-- The header fields are little-endian regardless of host. The /payload/ is
-- copied raw, so it is host-endian, and the flag records which that was: a
-- loader on a host of the other endianness refuses the file rather than
-- silently reinterpreting it. Every machine you are likely to run this on is
-- little-endian, so this is a guard, not a feature.
--
-- The sentinel padding is part of the payload because the descent reads it.
--
-- == Loading is eager
--
-- 'readSTree' returns only once every key is in memory: it allocates the whole
-- array up front and fills it with a chunked read through a small staging
-- buffer. Peak residency is the payload plus a megabyte or so. Nothing is
-- deferred, so a truncated or unreadable file fails at load time rather than
-- surfacing an exception from inside a pure query later.
module Data.STree.Serialize
  ( -- * Writing
    writeSTree
  , hPutSTree

    -- * Reading
  , readSTree
  , hGetSTree

    -- * Errors
  , SerializeError (..)

    -- * Key types
  , SerializableKey (..)

    -- * Format
  , formatVersion
  , headerSize
  ) where

import Control.Exception (bracketOnError)
import Data.Bits (shiftL, testBit, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import Data.Int (Int32, Int64)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, peek, sizeOf)
import System.Directory (removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import Data.STree.BTree
import Data.STree.Key (Key)

-- | Key types that can be persisted, with the tag that identifies them in the
-- header so that loading a file at the wrong type fails loudly instead of
-- reinterpreting the bits.
--
-- 'Storable' is required for the raw copy; it is not a superclass of 'Key'
-- because nothing else in the library needs it.
class (Key a, Storable a) => SerializableKey a where
  keyTag :: Word8

-- | Note that @Int@ and @Word@ files are not portable across word sizes; the
-- payload length check rejects the mismatch.
instance SerializableKey Int where keyTag = 1

instance SerializableKey Int32 where keyTag = 2

instance SerializableKey Int64 where keyTag = 3

instance SerializableKey Word where keyTag = 4

instance SerializableKey Word32 where keyTag = 5

instance SerializableKey Word64 where keyTag = 6

instance SerializableKey Float where keyTag = 7

instance SerializableKey Double where keyTag = 8

-- | Why a file cannot be loaded. Genuine IO failures (missing file, permission
-- denied) are thrown as @IOException@ instead, as usual.
data SerializeError
  = -- | Not an S-tree file.
    BadMagic
  | -- | Written by an incompatible version of this module.
    UnsupportedVersion Word16
  | -- | Written at a different key type: expected tag, found tag.
    KeyTypeMismatch Word8 Word8
  | -- | Written with a different node width: expected, found.
    LineWidthMismatch Int Int
  | -- | Payload written on a host of the other endianness.
    EndiannessMismatch
  | -- | Header fields contradict each other, or the size is absurd.
    InconsistentHeader String
  | -- | The file is smaller than the header says: needed, available.
    ShortFile Integer Integer
  | -- | The file is larger than the header says, by this many bytes.
    UnexpectedTrailingBytes Integer
  | -- | The payload ended early, after this many bytes.
    Truncated Int
  deriving (Eq, Show)

-- | The format version this module reads and writes.
formatVersion :: Word16
formatVersion = 1

-- | Size of the header in bytes. The payload starts here.
headerSize :: Int
headerSize = 64

-- | Bytes moved per read or write call. Large enough to amortise the syscall,
-- small enough that the staging buffer stays in cache.
chunkBytes :: Int
chunkBytes = 1024 * 1024

magic :: B.ByteString
magic = B.pack [0x53, 0x54, 0x52, 0x45, 0x45, 0x00, 0x00, 0x00] -- "STREE\0\0\0"

hostIsLittleEndian :: Bool
hostIsLittleEndian = unsafePerformIO $
  with (0x01020304 :: Word32) $ \p ->
    (== 4) <$> peek (castPtr p :: Ptr Word8)
{-# NOINLINE hostIsLittleEndian #-}

-- ---------------------------------------------------------------------------
-- Writing
-- ---------------------------------------------------------------------------

-- | Write a tree to a file, replacing it atomically: the tree goes to a
-- temporary file in the same directory which is then renamed over the target,
-- so an interrupted write cannot leave a file that passes validation but has
-- garbage in its tail.
--
-- Atomic replacement is not the same as durability — if you need the data to
-- survive power loss, @fsync@ the directory afterwards.
{-# INLINABLE writeSTree #-}
writeSTree :: forall l a. (LineWidth l, SerializableKey a) => FilePath -> BTree l a -> IO ()
writeSTree path t =
  bracketOnError
    (openBinaryTempFile (takeDirectory path) (takeFileName path ++ ".tmp"))
    (\(tmp, h) -> hClose h >> removeFile tmp)
    ( \(tmp, h) -> do
        hPutSTree h t
        hClose h
        renameFile tmp path
    )

-- | Write a tree to a handle at its current position. The handle must be in
-- binary mode.
{-# INLINABLE hPutSTree #-}
hPutSTree :: forall l a. (LineWidth l, SerializableKey a) => Handle -> BTree l a -> IO ()
hPutSTree h t = do
  B.hPut h (encodeHeader (keyTag @a) (lineWidth @l) n payloadBytes)
  go 0
  where
    arr = layout t
    n = size t
    esize = sizeOf (undefined :: a)
    total = U.length arr
    payloadBytes = total * esize
    chunkElems = max 1 (chunkBytes `quot` esize)

    -- Unboxed vectors do not expose a pointer, so each chunk is copied into a
    -- fresh Storable vector — pinned, hence safe to hand to the syscall.
    go !off
      | off >= total = return ()
      | otherwise = do
          let len = min chunkElems (total - off)
              src = U.convert (U.unsafeSlice off len arr) :: S.Vector a
          S.unsafeWith src $ \p -> hPutBuf h p (len * esize)
          go (off + len)

encodeHeader :: Word8 -> Int -> Int -> Int -> B.ByteString
encodeHeader tag l n payloadBytes =
  BL.toStrict . BB.toLazyByteString $
    BB.byteString magic
      <> BB.word16LE formatVersion
      <> BB.word8 tag
      <> BB.word8 (fromIntegral l)
      <> BB.word32LE flags
      <> BB.word64LE (fromIntegral n)
      <> BB.word64LE (fromIntegral payloadBytes)
      <> BB.word64LE 0 -- checksum: absent
      <> BB.byteString (B.replicate 24 0)
  where
    flags = if hostIsLittleEndian then 1 else 0

-- ---------------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------------

-- | Read a tree written by 'writeSTree'. The file must contain exactly one
-- tree and nothing else.
{-# INLINABLE readSTree #-}
readSTree ::
  forall l a.
  (LineWidth l, SerializableKey a) =>
  FilePath ->
  IO (Either SerializeError (BTree l a))
readSTree path = withBinaryFile path ReadMode $ \h -> do
  available <- hFileSize h
  readFrom @l @a True h available

-- | Read a tree from a handle at its current position, given the number of
-- bytes available to it there. Anything left over after the tree is ignored,
-- so this can read a tree embedded in a larger file.
--
-- The budget is not optional because the header is untrusted: without a known
-- bound, a corrupt size field turns into an absurd allocation before anything
-- has had a chance to notice.
{-# INLINABLE hGetSTree #-}
hGetSTree ::
  forall l a.
  (LineWidth l, SerializableKey a) =>
  Handle ->
  Integer ->
  IO (Either SerializeError (BTree l a))
hGetSTree = readFrom @l @a False

{-# INLINABLE readFrom #-}
readFrom ::
  forall l a.
  (LineWidth l, SerializableKey a) =>
  Bool ->
  Handle ->
  Integer ->
  IO (Either SerializeError (BTree l a))
readFrom exact h available = do
  hdr <- B.hGet h headerSize
  case decodeHeader @l @a exact hdr available of
    Left err -> return (Left err)
    Right n -> do
      let total = layoutLength @l n
          esize = sizeOf (undefined :: a)
          chunkElems = max 1 (chunkBytes `quot` esize)
      mv <- UM.new total
      staging <- SM.new chunkElems
      let go !off
            | off >= total = return Nothing
            | otherwise = do
                let len = min chunkElems (total - off)
                    bytes = len * esize
                got <- SM.unsafeWith staging $ \p -> hGetBuf h p bytes
                if got /= bytes
                  then return (Just (Truncated (off * esize + got)))
                  else do
                    -- The frozen slice is consumed by the copy below before the
                    -- buffer is written again, so reusing it is safe.
                    src <- S.unsafeFreeze (SM.unsafeSlice 0 len staging)
                    U.unsafeCopy (UM.unsafeSlice off len mv) (U.convert src)
                    go (off + len)
      outcome <- go 0
      case outcome of
        Just err -> return (Left err)
        Nothing -> Right . unsafeFromLayout @l n <$> U.unsafeFreeze mv

-- | Validate a header and return the number of keys.
--
-- The order of the checks matters: the size field is untrusted, so it is bounded
-- by the bytes actually available before it is used to compute anything, and the
-- consistency check is done in 'Integer' so that a hostile value cannot overflow
-- its way past it. Once the payload length is known to match, every index the
-- descent can compute is in bounds — which is what makes the unchecked indexing
-- of the query path safe on a foreign file.
decodeHeader ::
  forall l a.
  (LineWidth l, SerializableKey a) =>
  Bool ->
  B.ByteString ->
  Integer ->
  Either SerializeError Int
decodeHeader exact hdr available = do
  let need = fromIntegral headerSize
  if B.length hdr < headerSize || available < need
    then Left (ShortFile need available)
    else Right ()

  if B.take 8 hdr /= magic then Left BadMagic else Right ()

  let version = fromIntegral (leWord hdr 8 2) :: Word16
  if version /= formatVersion then Left (UnsupportedVersion version) else Right ()

  let tag = fromIntegral (leWord hdr 10 1) :: Word8
  if tag /= keyTag @a then Left (KeyTypeMismatch (keyTag @a) tag) else Right ()

  let width = fromIntegral (leWord hdr 11 1) :: Int
  if width /= lineWidth @l then Left (LineWidthMismatch (lineWidth @l) width) else Right ()

  let flags = fromIntegral (leWord hdr 12 4) :: Word32
  if testBit flags 0 /= hostIsLittleEndian then Left EndiannessMismatch else Right ()

  let rawSize = toInteger (leWord hdr 16 8)
      payload = toInteger (leWord hdr 24 8)
      esize = toInteger (sizeOf (undefined :: a))
      total = fromIntegral headerSize + payload

  -- Bound the payload by the file before believing anything derived from it.
  if total > available then Left (ShortFile total available) else Right ()
  if exact && total < available
    then Left (UnexpectedTrailingBytes (available - total))
    else Right ()

  -- With the payload bounded, so is the key count.
  if rawSize > payload `quot` esize
    then
      Left . InconsistentHeader $
        "size " ++ show rawSize ++ " does not fit in a payload of " ++ show payload ++ " bytes"
    else Right ()

  let n = fromIntegral rawSize :: Int
      expected = toInteger (layoutLength @l n) * esize
  if expected /= payload
    then
      Left . InconsistentHeader $
        "payload is " ++ show payload ++ " bytes, expected " ++ show expected ++ " for " ++ show n ++ " keys"
    else Right ()

  return n

-- | Decode a little-endian unsigned field of @width@ bytes at @off@. Done byte
-- by byte so that header parsing does not depend on the host's byte order or on
-- alignment.
leWord :: B.ByteString -> Int -> Int -> Word64
leWord bs off width = go 0 0
  where
    go !i !acc
      | i >= width = acc
      | otherwise = go (i + 1) (acc .|. (fromIntegral (BU.unsafeIndex bs (off + i)) `shiftL` (8 * i)))
