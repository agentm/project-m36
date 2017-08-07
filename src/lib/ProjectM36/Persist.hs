{-# LANGUAGE ForeignFunctionInterface, CPP #-}
--this module is related to persisting Project:M36 structures to disk and not related to the persistent library
module ProjectM36.Persist (writeFileSync, 
                           writeBSFileSync,
                           renameSync, 
                           DiskSync(..)) where
-- on Windows, use FlushFileBuffers and MoveFileEx
import qualified Data.Text.IO as TIO
import Data.Text
#if defined(mingw32_HOST_OS)
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
#if defined(linux_HOST_OS)
import System.Posix.Unistd (fileSynchroniseDataOnly)
#else
import System.Posix.Unistd (fileSynchronise)
#endif
import System.Posix.IO (handleToFd)
import Foreign.C
#endif

import System.IO (withFile, IOMode(WriteMode), Handle)
import qualified Data.ByteString.Lazy as BS

#if defined(mingw32_HOST_OS)
import ProjectM36.Win32Handle
#else
foreign import ccall unsafe "cDirectoryFsync" cHSDirectoryFsync :: CString -> IO CInt
#endif

data DiskSync = NoDiskSync | FsyncDiskSync

writeFileSync :: DiskSync -> FilePath -> Text -> IO()
writeFileSync sync path strOut = withFile path WriteMode handler
  where
    handler handle = do
      TIO.hPutStr handle strOut
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
  Win32.moveFileEx srcPath dstPath Win32.mOVEFILE_REPLACE_EXISTING
#else
  Posix.rename srcPath dstPath
#endif

syncHandle :: DiskSync -> Handle -> IO ()
syncHandle FsyncDiskSync handle =
#if defined(mingw32_HOST_OS)
  withHandleToHANDLE handle (\h -> Win32.flushFileBuffers h)
#elif defined(linux_HOST_OS)
  --fdatasync doesn't exist on macOS
  handleToFd handle >>= fileSynchroniseDataOnly
#else 
  handleToFd handle >>= fileSynchronise
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

