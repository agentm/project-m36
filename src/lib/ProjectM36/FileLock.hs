{-# LANGUAGE CPP, NamedFieldPuns #-}
--cross-platform file locking utilizing POSIX file locking on Unix/Linux and Windows file locking
--hackage's System.FileLock doesn't support POSIX advisory locks nor locking file based on file descriptors, hence this needless rewrite
module ProjectM36.FileLock where


#if defined(mingw32_HOST_OS)
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

type LockFile = HANDLE

openLockFile :: FilePath -> IO LockFile
openLockFile path = createFile path 
                    (gENERIC_READ .|. gENERIC_WRITE)
                    (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
                    Nothing
                    oPEN_ALWAYS
                    fILE_ATTRIBUTE_NORMAL
                    Nothing

closeLockFile :: LockFile -> IO ()
closeLockFile file = do
   unlockFile file
   closeHandle file

--swiped from System.FileLock package
lockFile :: HANDLE -> LockType -> IO ()
lockFile winHandle lock = do
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

unlockFile :: HANDLE -> IO ()
unlockFile winHandle = do
  let sizeof_OVERLAPPED = 32
  allocaBytes sizeof_OVERLAPPED $ \op -> do
    zeroMemory op $ fromIntegral sizeof_OVERLAPPED
    res <- c_unlockFileEx winHandle 0 1 0 op
    if res then
      pure ()
    else
      error ("failed to unlock database lock: " ++ show res)

#else
--all of this complicated nonsense is fixed if we switch to GHC 8.2 which includes native flock support on handles
import qualified System.Posix.IO as P
import System.Posix.Types
import System.Posix.Files
import System.IO

lockStruct :: P.LockRequest -> P.FileLock
lockStruct req = (req, AbsoluteSeek, 0, 0)

newtype LockFile = LockFile Fd

--we cannot use openFile from System.IO because it implements complicated locking which prevents opening the same file twice in write mode in the same process with no way to bypass the check.
openLockFile :: FilePath -> IO LockFile
openLockFile path =
  LockFile <$> P.createFile path ownerWriteMode
  
closeLockFile :: LockFile -> IO ()
closeLockFile l@(LockFile fd) = do
  unlockFile l
  P.closeFd fd
  
--blocks on lock, if necessary
lockFile :: LockFile -> LockType -> IO ()    
lockFile (LockFile fd) lock = do
  let lockt = case lock of
        WriteLock -> P.WriteLock
        ReadLock -> P.ReadLock
  P.waitToSetLock fd (lockStruct lockt)
  
unlockFile :: LockFile -> IO ()  
unlockFile (LockFile fd) = 
  P.waitToSetLock fd (lockStruct P.Unlock)
#endif

data LockType = ReadLock | WriteLock

  
