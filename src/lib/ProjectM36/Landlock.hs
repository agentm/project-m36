module ProjectM36.Landlock where
import System.Landlock

setup :: IO Bool
setup = do
  supported <- isSupported
  if supported then
    setup'
    else
    pure False

setup' :: IO Bool
setup' = do
  version <- abiVersion
  allFlags <- lookupAccessFsFlags version
