-- confirm that the filesystem type is a journaled FS type expected by Project:M36
-- use statfs on Linux and macOS and GetVolumeInformation on Windows
-- this could still be fooled with symlinks or by disabling journaling on filesystems that support that
module ProjectM36.FSType where
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error

#ifdef mingw32_HOST_OS
import System.Win32.String 
import System.Win32.File

foreign import WINDOWS_CCONV unsafe "windows.h GetVolumeInformationW"
  c_GetVolumeInformation :: LPCTSTR -> LPTSTR -> DWORD -> LPDWORD -> LPDWORD -> LPDWORD -> LPTSTR -> DWORD -> IO BOOL

data VolumeInformation = VolumeInformation
                         { volumeName         :: String
                         , volumeSerialNumber :: DWORD
                         , maximumComponentLength :: DWORD
                         , fileSystemFlags    :: DWORD
                         , fileSystemName     :: String
                         } deriving Show

-- from https://github.com/haskell/win32/blob/master/System/Win32/HardLink.hs
getVolumeInformation :: Maybe String -> IO VolumeInformation
getVolumeInformation mPath =
   maybeWith withTString mPath $ \c_path ->
   withTStringBufferLen 256    $ \(vnBuf, vnLen) ->
   alloca $ \serialNum ->
   alloca $ \maxLen ->
   alloca $ \fsFlags ->
   withTStringBufferLen 256 $ \(fsBuf, fsLen) -> do
       failIfFalse_ (unwords ["GetVolumeInformationW", c_path]) $
         c_GetVolumeInformation c_path vnBuf (fromIntegral vnLen)
                                serialNum maxLen fsFlags
                                fsBuf (fromIntegral fsLen)
       return VolumeInformation
         <*> peekTString vnBuf
         <*> peek serialNum
         <*> peek maxLen
         <*> peek fsFlags
         <*> peekTString fsBuf

fsTypeSupportsJournaling :: FilePath -> IO Bool
fsTypeSupportJournaling path = do
  info <- getVolumeInformation
#define FILE_SUPPORTS_USN_JOURNAL 0x02000000
  pure (fileSystemFlags info .&. FILE_SUPPORTS_USN_JOURNAL)

#elif darwin_HOST_OS
--Darwin reports journaling directly in the fs flags

type CStatFS = ()
foreign import ccall unsafe "cDarwinFSJournaled" 
  c_DarwinFSJournaled :: CString -> IO CInt

fsTypeSupportsJournaling :: FilePath -> IO Bool
fsTypeSupportsJournaling path = 
  withCString path $ \c_path -> do
    ret <- throwErrnoIfMinus1 "statfs" (c_DarwinFSJournaled c_path)
    pure (ret > (0 :: CInt))
      
#elif linux_HOST_OS
import Foreign
--Linux cannot report journaling, so we just check the filesystem type as a proxy
type CStatFS = ()
foreign import ccall unsafe "sys/vfs.h statfs" 
  c_statfs :: CString -> Ptr CStatFS -> IO CInt
  
#if sizeof(int) == 8
type CFSType = Word64
#else
type CFSType = Word32
#endif

fsTypeSupportsJournaling :: FilePath -> IO Bool
fsTypeSupportsJournaling path = do
  struct_statfs <- mallocForeignPtrBytes #{size statfs}
  withCString path $ \c_path -> do
    withForeignPtr struct_statfs $ \ptr_statfs -> do
      throwErrnoIfMinus1_ "statfs" (c_statfs c_path ptr_statfs)
      cfstype <- peekByteOff ptr_statfs 0 :: IO CFSType
      let journaledFS = [0xEF53, --EXT3+4
                         0x5346544e, --NTFS
                         0x52654973, --REISERFS
                         0x58465342, --XFS
                         0x3153464a --JFS
                         ]
      pure (elem cfstype journaledFS)
#endif

