{-# LANGUAGE ForeignFunctionInterface, CPP #-}
--this module is related to persisting Project:M36 structures to disk and not related to the persistent library
module ProjectM36.Persist (writeFileSync, 
                           writeBSFileSync,
                           renameSync, 
                           DiskSync(..)) where
-- on Windows, use FlushFileBuffers and System.Directory's rename (which uses MoveFileEx)
import System.Directory (renameDirectory)

#if defined(mingw32_HOST_OS)
import System.Win32.File (flushFileBuffers)
--import ProjectM36.Win32Handle
#else
import System.Posix.Unistd (fileSynchroniseDataOnly)
import System.Posix.IO (handleToFd)
import Foreign.C
#endif

import System.IO (withFile, IOMode(WriteMode), hPutStr, Handle)
import qualified Data.ByteString.Lazy as BS

#if defined(mingw32_HOST_OS)
import ProjectM36.Win32Handle
#else
foreign import ccall unsafe "cDirectoryFsync" cHSDirectoryFsync :: CString -> IO CInt
#endif

data DiskSync = NoDiskSync | FsyncDiskSync

writeFileSync :: DiskSync -> FilePath -> String -> IO()
writeFileSync sync path strOut = withFile path WriteMode handler
  where
    handler handle = do
      hPutStr handle strOut
      syncHandle sync handle

renameSync :: DiskSync -> FilePath -> FilePath -> IO ()
renameSync sync srcPath dstPath = do
  renameDirectory srcPath dstPath
  syncDirectory sync dstPath

syncHandle :: DiskSync -> Handle -> IO ()
syncHandle FsyncDiskSync handle = do
#if defined(mingw32_HOST_OS)
  withHandleToHANDLE handle (\h -> flushFileBuffers h)
#else
  handleToFd handle >>= fileSynchroniseDataOnly
#endif
syncHandle NoDiskSync _ = pure ()

syncDirectory :: DiskSync -> FilePath -> IO ()
syncDirectory FsyncDiskSync path = directoryFsync path 
syncDirectory NoDiskSync _ = pure ()

writeBSFileSync :: DiskSync -> FilePath -> BS.ByteString -> IO ()
writeBSFileSync sync path bstring = do
  withFile path WriteMode $ \handle -> do
    BS.hPut handle bstring
    syncHandle sync handle
  
directoryFsync :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
directoryFsync _ = pure () -- on Windows directory metadata cannot be synchronized- perhaps there is a workaround? resync a file in the directory? 
#else
directoryFsync path = throwErrnoIfMinus1Retry_ "directoryFsync" (withCString path $ \cpath -> cHSDirectoryFsync cpath)
#endif

