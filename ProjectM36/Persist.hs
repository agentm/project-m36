{-# LANGUAGE ForeignFunctionInterface #-}
--this module is related to persisting Project:M36 structures to disk and not related to the persistent library
module ProjectM36.Persist (writeFileSync, 
                           writeBSFileSync,
                           renameSync, 
                           DiskSync(..)) where
--import ProjectM36.DirectoryFsync (directoryFsync)
import System.Posix.Files (rename)
import System.Posix.Unistd (fileSynchroniseDataOnly)
import System.Posix.IO (handleToFd)
import System.IO (withFile, IOMode(WriteMode), hPutStr, Handle)
import qualified Data.ByteString.Lazy as BS
import Foreign.C

foreign import ccall unsafe "cDirectoryFsync" cHSDirectoryFsync :: CString -> IO (CInt)

data DiskSync = NoDiskSync | FsyncDiskSync

writeFileSync :: DiskSync -> FilePath -> String -> IO()
writeFileSync sync path strOut = withFile path WriteMode handler
  where
    handler handle = do
      hPutStr handle strOut
      syncHandle sync handle

renameSync :: DiskSync -> FilePath -> FilePath -> IO ()
renameSync sync srcPath dstPath = do
  rename srcPath dstPath
  syncDirectory sync dstPath
  
syncHandle :: DiskSync -> Handle -> IO ()    
syncHandle FsyncDiskSync handle = handleToFd handle >>= fileSynchroniseDataOnly
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
directoryFsync path = throwErrnoIfMinus1Retry_ "directoryFsync" (withCString path $ \cpath -> cHSDirectoryFsync cpath)
  