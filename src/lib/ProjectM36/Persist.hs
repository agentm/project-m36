{-# LANGUAGE ForeignFunctionInterface, CPP #-}
--this module is related to persisting Project:M36 structures to disk and not related to the persistent library
module ProjectM36.Persist (writeFileSync, 
                           writeBSFileSync,
                           renameSync,
                           printFdCount,
                           DiskSync(..)) where
-- on Windows, use FlushFileBuffers and MoveFileEx
import qualified Data.Text as T

#if defined(linux_HOST_OS)
# define FDCOUNTSUPPORTED 1
# define FDDIR "/proc/self/fd"
#elif defined(darwin_HOST_OS)
# define FDCOUNTSUPPORTED 1
# define FDDIR "/dev/fd"
#else
# define FDCOUNTSUPPORTED 0
#endif

#if FDCOUNTSUPPORTED
import System.Directory
#endif

#if defined(mingw32_HOST_OS)
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
#if defined(linux_HOST_OS)
import System.Posix.Unistd (fileSynchroniseDataOnly)
#else
import System.Posix.Unistd (fileSynchronise)
#endif
import System.Posix.IO (handleToFd, closeFd)
import Foreign.C
#endif

import System.IO (withFile, IOMode(WriteMode), Handle)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BS'
import qualified Data.Text.Encoding as TE

#if defined(mingw32_HOST_OS)
import ProjectM36.Win32Handle
#else
foreign import ccall unsafe "cDirectoryFsync" cHSDirectoryFsync :: CString -> IO CInt
#endif

data DiskSync = NoDiskSync | FsyncDiskSync

--using withFile here is OK because we use a WORM strategy- the file is written but not read until after the handle is synced, closed, and unhidden (moved from ".X" to "X") at the top level in the transaction directory 
writeFileSync :: DiskSync -> FilePath -> T.Text -> IO()
writeFileSync sync path strOut = withFile path WriteMode handler
  where
    handler handle = do
      BS'.hPut handle (TE.encodeUtf8 strOut)
      syncHandle sync handle

renameSync :: DiskSync -> FilePath -> FilePath -> IO ()
renameSync sync srcPath dstPath = do
  atomicRename srcPath dstPath
  syncDirectory sync dstPath

-- System.Directory's renameFile/renameDirectory almost do exactly what we want except that it needlessly differentiates between directories and files
{-# ANN atomicRename ("HLint: ignore Eta reduce" :: String) #-}
atomicRename :: FilePath -> FilePath -> IO ()
atomicRename srcPath dstPath = 
#if defined(mingw32_HOST_OS)
#if MIN_VERSION_Win32(2,6,0)
  let dst = Just dstPath
#else
  let dst = dstPath
#endif
  in
  Win32.moveFileEx srcPath dst Win32.mOVEFILE_REPLACE_EXISTING
#else
  Posix.rename srcPath dstPath
#endif

syncHandle :: DiskSync -> Handle -> IO ()
syncHandle FsyncDiskSync handle =
#if defined(mingw32_HOST_OS)
  withHandleToHANDLE handle (\h -> Win32.flushFileBuffers h)
#elif defined(linux_HOST_OS)
 do
  --fdatasync doesn't exist on macOS
  fd <- handleToFd handle 
  fileSynchroniseDataOnly fd
  closeFd fd  
#else
 do
  fd <- handleToFd handle
  fileSynchronise fd
  closeFd fd
#endif

syncHandle NoDiskSync _ = pure ()

syncDirectory :: DiskSync -> FilePath -> IO ()
syncDirectory FsyncDiskSync path = directoryFsync path 
syncDirectory NoDiskSync _ = pure ()

writeBSFileSync :: DiskSync -> FilePath -> BS.ByteString -> IO ()
writeBSFileSync sync path bstring =
  withFile path WriteMode $ \handle -> do
    BS.hPut handle bstring
    syncHandle sync handle
  
directoryFsync :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
directoryFsync _ = pure () -- on Windows directory metadata cannot be synchronized- perhaps there is a workaround? resync a file in the directory? 
#else
directoryFsync path = throwErrnoIfMinus1Retry_ "directoryFsync" (withCString path $ \cpath -> cHSDirectoryFsync cpath)
#endif

--prints out number of consumed file descriptors      
printFdCount :: IO ()
printFdCount =
#if FDCOUNTSUPPORTED
 do
  fdc <- fdCount
  putStrLn ("Fd count: " ++ show fdc)
  --getLine >> pure ()
#else
  putStrLn "Fd count not supported on this OS."
#endif

#if FDCOUNTSUPPORTED
fdCount :: IO Int
fdCount = do
  fds <- getDirectoryContents FDDIR
  pure ((length fds) - 2)
--not supported on non-linux
#endif