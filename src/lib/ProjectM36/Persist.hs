{-# LANGUAGE ForeignFunctionInterface, CPP #-}
--this module is related to persisting Project:M36 structures to disk and not related to the persistent library
module ProjectM36.Persist (writeFileSync, 
                           writeBSFileSync,
                           renameSync, 
                           DiskSync(..)) where
-- on Windows, use FlushFileBuffers and MoveFileEx

#if defined(mingw32_HOST_OS)
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
  #ifdef HAVE_FDATASYNC
import System.Posix.Unistd (fileSynchroniseDataOnly)
  #endif
import System.Posix.Unistd (fileSynchronise)
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
  atomicRename srcPath dstPath
  syncDirectory sync dstPath

-- System.Directory's renameFile/renameDirectory almost do exactly what we want except that it needlessly differentiates between directories and files
atomicRename :: FilePath -> FilePath -> IO ()
atomicRename srcPath dstPath = do
#if defined(mingw32_HOST_OS)
  Win32.moveFileEx srcPath dstPath Win32.mOVEFILE_REPLACE_EXISTING
#else
  Posix.rename srcPath dstPath
#endif

syncHandle :: DiskSync -> Handle -> IO ()
syncHandle FsyncDiskSync handle = do
#if defined(mingw32_HOST_OS)
  withHandleToHANDLE handle (\h -> Win32.flushFileBuffers h)
#elif HAVE_FDATASYNC
  handleToFd handle >>= fileSynchroniseDataOnly
#else
  handleToFd handle >>= fileSynchronise
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

