{-# LANGUAGE CPP #-}
-- confirm that the filesystem type is a journaled FS type expected by Project:M36
-- use statfs on Linux and macOS and GetVolumeInformation on Windows
-- this could still be fooled with symlinks or by disabling journaling on filesystems that support that
module ProjectM36.FSType where

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# endif
import System.Win32.Types
import Foreign.ForeignPtr
import Data.Word
import Data.Bits
import Foreign.Storable

foreign import WINDOWS_CCONV unsafe "windows.h GetVolumePathNameW"
  c_GetVolumePathName :: LPCTSTR -> LPTSTR -> DWORD -> IO BOOL
  
foreign import WINDOWS_CCONV unsafe "windows.h GetVolumeInformationW"
  c_GetVolumeInformation :: LPCTSTR -> LPTSTR -> DWORD -> LPDWORD -> LPDWORD -> LPDWORD -> LPTSTR -> DWORD -> IO BOOL

#define FILE_SUPPORTS_USN_JOURNAL 0x02000000

getVolumePathName :: FilePath -> IO String
getVolumePathName path = do
  let maxpathlen = 260 --ANSI MAX_PATH- we only care about the drive name anyway
  withTString path $ \c_path -> do
    fp_pathout <- mallocForeignPtrBytes maxpathlen
    withForeignPtr fp_pathout $ \pathout -> do
      failIfFalse_ ("GetVolumePathNameW " ++ path) (c_GetVolumePathName c_path pathout (fromIntegral maxpathlen))
      peekTString pathout

fsTypeSupportsJournaling :: FilePath -> IO Bool
fsTypeSupportsJournaling path = do
    -- get the drive path of the incoming path
    drive <- getVolumePathName path
    withTString drive $ \c_drive -> do
        foreign_flags <- mallocForeignPtrBytes 8
        withForeignPtr foreign_flags $ \ptr_fsFlags -> do
            failIfFalse_ (unwords ["GetVolumeInformationW", path]) (c_GetVolumeInformation c_drive nullPtr 0 nullPtr nullPtr ptr_fsFlags nullPtr 0)
            fsFlags <- peekByteOff ptr_fsFlags 0 :: IO Word64
            pure (fsFlags .&. FILE_SUPPORTS_USN_JOURNAL /= 0)

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
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

#include "MachDeps.h"
--Linux cannot report journaling, so we just check the filesystem type as a proxy
type CStatFS = ()
foreign import ccall unsafe "sys/vfs.h statfs" 
  c_statfs :: CString -> Ptr CStatFS -> IO CInt
  
#if WORD_SIZE_IN_BITS == 64
type CFSType = Word64
sizeofStructStatFS :: Int
sizeofStructStatFS = 120
#else
#error 32-bit not supported due to sizeof struct statfs missing
type CFSType = Word32
sizeofStructStatFS :: Int
sizeofStructStatFS = undefined
#endif

fsTypeSupportsJournaling :: FilePath -> IO Bool
fsTypeSupportsJournaling path = do
  struct_statfs <- mallocForeignPtrBytes sizeofStructStatFS
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

