#if defined(mingw32_HOST_OS)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
module ProjectM36.SystemMemory where
import Data.Int (Int64)
#if defined(darwin_HOST_OS)  
import System.Process (readProcess)
import Control.Exception (catch, SomeException)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
#endif
#if defined(linux_HOST_OS)
import System.Linux.Proc.MemInfo
import System.Linux.Proc.Errors
import qualified Data.Text as T
#endif 

#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import System.Win32.Types
import System.Win32.Memory

-- Define the MEMORYSTATUSEX structure
data MEMORYSTATUSEX = MEMORYSTATUSEX
    { dwLength       :: DWORD
    , dwMemoryLoad   :: DWORD
    , ullTotalPhys   :: DWORD64
    , ullAvailPhys   :: DWORD64
    , ullTotalPageFile :: DWORD64
    , ullAvailPageFile :: DWORD64
    , ullTotalVirtual :: DWORD64
    , ullAvailVirtual :: DWORD64
    , ullExtendedVirtual :: DWORD64
    }

-- Foreign function interface to call GlobalMemoryStatusEx
foreign import ccall "GlobalMemoryStatusEx" c_GlobalMemoryStatusEx :: Ptr MEMORYSTATUSEX -> IO ()
#endif

type FreeMemBytes = Int64
type TotalMemBytes = Int64
type MemoryStats = (FreeMemBytes, TotalMemBytes)

-- | Get total physical memory in a platform-specific way.
getMemoryStats :: IO (Either String MemoryStats)
getMemoryStats = do
#if defined(mingw32_HOST_OS)
    allocaBytes (sizeOf (undefined :: MEMORYSTATUSEX)) $ \ptr -> do
        let memStatus = MEMORYSTATUSEX { dwLength = fromIntegral (sizeOf (undefined :: MEMORYSTATUSEX)), .. }
        poke ptr memStatus
        c_GlobalMemoryStatusEx ptr
        memStatus' <- peek ptr
        return $ Right (fromIntegral (ullAvailPhys memStatus'), fromIntegral (ullTotalPhys memStatus'))
#elif defined(darwin_HOST_OS)
  eres <- try (readProcess "memory_pressure" [] "") :: IO (Either IOException String)
  case eres of
    Left err -> pure (Left (displayException err))
    Right memPressureText -> do
      let parseMemPressure = do
           _ <- manyTill anySingle (try (string "The system has "))
           totalMemBytes <- L.decimal
           _ <- manyTill anySingle (try (string "Pages free:"))
           space1
           -- parse integer pages
           freePages <- L.decimal
           _ <- char '%'
           pure (freePages * 4096, totalMemBytes) -- 4096 default page size on macOS
      case parse parseMemPressure "" memPressureText of
        Left err -> pure (Left (errorBundlePretty err))
        Right memvals -> pure (Just memvals)
#elif defined(linux_HOST_OS)
  eMemInfo <- readProcMemInfo
  pure $ case eMemInfo of
    Left err -> Left (T.unpack (renderProcError err))
    Right memInfo -> Right (fromIntegral (memFree memInfo), fromIntegral (memTotal memInfo))
#endif

-- | Estimate the memory pressure as a number between 0 and 1 which is a ratio of free to actively used memory.
getMemoryPressure :: IO (Maybe Double)
getMemoryPressure = do
#if defined(mingw32_HOST_OS)
  allocaBytes (sizeOf (undefined :: MEMORYSTATUSEX)) $ \ptr -> do
    let memStatus = MEMORYSTATUSEX { dwLength = fromIntegral (sizeOf (undefined :: MEMORYSTATUSEX)), .. }
    poke ptr memStatus
    c_GlobalMemoryStatusEx ptr
    memStatus' <- peek ptr
    return $ Just (fromIntegral (ullAvailPhys memStatus') / fromIntegral (ullTotalPhys memStatus'))
#elif defined(darwin_HOST_OS)
  eres <- try (readProcess "memory_pressure" [] "") :: IO (Either IOException String)
  case eres of
    Left err -> pure Nothing
    Right memPressureText -> do
      let parseMemPressure = do
           _ <- manyTill anySingle (try (string "System-wide memory free percentage"))
           space1
           _ <- char ':'
           space1
           -- parse integer percentage
           n <- L.decimal
           _ <- char '%'
           pure n
      case parseMaybe parseMemPressure memPressureText of
        Nothing -> pure Nothing
        Just percentage -> pure (Just (percentage / 100.0))
#elif defined(linux_HOST_OS)
  eMemInfo <- readProcMemInfo
  pure $ case eMemInfo of
    Left _err -> Nothing
    Right memInfo -> Just (fromIntegral (memFree memInfo))
#endif

-- on linux, /proc/pressure/(memory/cpu/io) track metrics for determining if useful work is being delayed due to pressure on those subsystems- however we want to prevent getting to this state at all, so it's not that useful
