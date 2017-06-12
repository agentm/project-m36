{-# LANGUAGE CPP #-}
--cross-platform file locking utilizing POSIX file locking on Unix/Linux and Windows file locking
--hackage's System.FileLock doesn't support POSIX advisory locks nor locking file based on file descriptors, hence this needless rewrite
module ProjectM36.FileLock where
import System.IO


#if defined(mingw32_HOST_OS)
import ProjectM36.Win32Handle
import System.Win32.Types
import Foreign.Marshal.Alloc
import System.Win32.File
import System.Win32.Mem
import Data.Bits

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif
foreign import WINDOWS_CCONV "LockFileEx" c_lockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED -> IO BOOL

foreign import WINDOWS_CCONV "UnlockFileEx" c_unlockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED -> IO BOOL

--swiped from System.FileLock package
lockFile :: Handle -> LockType -> IO ()
lockFile handle lock = withHandleToHANDLE handle $ \winHandle -> do
  let exFlag = case lock of
                 WriteLock -> 2
                 ReadLock -> 0
      blockFlag = 0 --always block
      sizeof_OVERLAPPED = 32

  allocaBytes sizeof_OVERLAPPED $ \op -> do
    zeroMemory op $ fromIntegral sizeof_OVERLAPPED
    res <- c_lockFileEx winHandle (exFlag .|. blockFlag) 0 1 0 op
    if res then
       pure ()
    else
       error "failed to wait for database lock"

unlockFile :: Handle -> IO ()
unlockFile handle = withHandleToHANDLE handle $ \winHandle -> do
  let sizeof_OVERLAPPED = 32
  allocaBytes sizeof_OVERLAPPED $ \op -> do
    zeroMemory op $ fromIntegral sizeof_OVERLAPPED
    res <- c_unlockFileEx winHandle 0 1 0 op
    if res then
      pure ()
    else
      error ("failed to unlock database lock: " ++ show res)

#else
import qualified System.Posix.IO as P

lockStruct :: P.LockRequest -> P.FileLock
lockStruct req = (req, AbsoluteSeek, 0, 0)

--blocks on lock, if necessary
lockFile :: Handle -> LockType -> IO ()    
lockFile file lock = do
  fd <- P.handleToFd file
  let lockt = case lock of
        WriteLock -> P.WriteLock
        ReadLock -> P.ReadLock
  P.waitToSetLock fd (lockStruct lockt)
  
unlockFile :: Handle -> IO ()  
unlockFile file = do 
  fd <- P.handleToFd file
  P.waitToSetLock fd (lockStruct P.Unlock)
#endif

data LockType = ReadLock | WriteLock

{-
lockFileSTM :: Handle -> LockType -> STM ()
lockFileSTM file lock = unsafeIOToSTM $ onException (lockFile file lock) (unlockFile file)

unlockFileSTM :: Handle -> STM ()
unlockFileSTM file = unsafeIOToSTM $ unlockFile file
-}
  
