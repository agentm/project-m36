{-# LANGUAGE CPP #-}
module ProjectM36.Win32Handle where
import System.Win32.Types
import Control.Exception (bracket)
import Foreign.StablePtr
import Foreign.C.Types
import Control.Concurrent.MVar

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle.Types (Handle(..), Handle__(..))
import GHC.IO.FD (FD(..)) -- A wrapper around an Int32
import Data.Typeable
#else
import GHC.IOBase (Handle(..), Handle__(..))
import qualified GHC.IOBase as IOBase (FD) -- Just an Int32
#endif

-- This essential function comes from the C runtime system. It is certainly provided by msvcrt, and also seems to be provided by the mingw C library - hurrah!
#if __GLASGOW_HASKELL__ >= 612
foreign import ccall unsafe "_get_osfhandle" cget_osfhandle :: CInt -> IO HANDLE
#else
foreign import ccall unsafe "_get_osfhandle" cget_osfhandle :: IOBase.FD -> IO HANDLE
#endif

-- copied from ansi-terminal package
-- | This bit is all highly dubious.  The problem is that we want to output ANSI to arbitrary Handles rather than forcing
-- people to use stdout.  However, the Windows ANSI emulator needs a Windows HANDLE to work it's magic, so we need to be able
-- to extract one of those from the Haskell Handle.
--
-- This code accomplishes this, albeit at the cost of only being compatible with GHC.
withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLE haskell_handle action = 
    -- Create a stable pointer to the Handle. This prevents the garbage collector
    -- getting to it while we are doing horrible manipulations with it, and hence
    -- stops it being finalized (and closed).
    withStablePtr haskell_handle $ const $ do
        -- Grab the write handle variable from the Handle
        let write_handle_mvar = case haskell_handle of
                FileHandle _ handle_mvar     -> handle_mvar
                DuplexHandle _ _ handle_mvar -> handle_mvar -- This is "write" MVar, we could also take the "read" one
        
        -- Get the FD from the algebraic data type
#if __GLASGOW_HASKELL__ < 612
        fd <- haFD <$> readMVar write_handle_mvar
#else
        --readMVar write_handle_mvar >>= \(Handle__ { haDevice = dev }) -> print (typeOf dev)
        Just fd <- fmap (\(Handle__ { haDevice = dev }) -> fmap fdFD (cast dev)) $ readMVar write_handle_mvar
#endif

        -- Finally, turn that (C-land) FD into a HANDLE using msvcrt
        windows_handle <- cget_osfhandle fd
        
        -- Do what the user originally wanted
        action windows_handle

#if !MIN_VERSION_win32(2,5,1)
withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr
#endif