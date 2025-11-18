#if defined(mingw32_HOST_OS)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
module ProjectM36.SystemMemory where
import Data.Int (Int64)
#if defined(darwin_HOST_OS)  
import System.Process (readProcess)
import Control.Exception (catch, SomeException)
#endif
#if defined(linux_HOST_OS)
import Data.List (isPrefixOf)
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

-- | Get total physical memory in a platform-specific way.
getTotalMemory :: IO (Maybe Int64)
getTotalMemory = do
#if defined(mingw32_HOST_OS)
    allocaBytes (sizeOf (undefined :: MEMORYSTATUSEX)) $ \ptr -> do
        let memStatus = MEMORYSTATUSEX { dwLength = fromIntegral (sizeOf (undefined :: MEMORYSTATUSEX)), .. }
        poke ptr memStatus
        c_GlobalMemoryStatusEx ptr
        memStatus' <- peek ptr
        return $ Just (fromIntegral (ullTotalPhys memStatus'))
#elif defined(darwin_HOST_OS)
      result <- catch (readProcess "sysctl" ["-n", "hw.memsize"] "") handleError
      return $ readMaybe result
        where
          handleError :: SomeException -> IO (Maybe Int64)
          handleError _ = return Nothing
          readMaybe :: String -> Maybe Int64
          readMaybe s = case reads s of
            [(val, "")] -> Just val
            _           -> Nothing
#elif defined(linux_HOST_OS)      
      contents <- readFile "/proc/meminfo"
      let totalRAMLine = filter (isPrefixOf "MemTotal:") (lines contents)
      case totalRAMLine of
        (line:_) -> return $ parseTotalRAM line
        []       -> return Nothing
  where
    parseTotalRAM :: String -> Maybe Int64
    parseTotalRAM line = 
          let wordsInLine = words line
          in if length wordsInLine > 1
          then Just (1024 * read (wordsInLine !! 1) :: Int64)  -- The second word is the total RAM in kB
       else Nothing
#endif
