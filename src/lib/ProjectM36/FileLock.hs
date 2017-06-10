--cross-platform file locking utilizing POSIX file locking on Unix/Linux and Windows file locking
--hackage's System.FileLock doesn't support POSIX advisory locks nor locking file based on file descriptors, hence this needless rewrite
module ProjectM36.FileLock where

#if defined(linux_HOST_OS)
import qualified System.Posix.IO as P
import System.IO

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
  
