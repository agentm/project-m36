--build distributions for various platforms using shake
import Development.Shake
import Development.Shake.FilePath
import ProjectM36.AtomFunctionBodyDeps
import Control.Monad
import Data.List
 
binary_src :: FilePath -> FilePath  
binary_src bin_name = "dist" </> "build" </> bin_name </> bin_name

binary_dest :: FilePath -> FilePath
binary_dest bin_name  = "dist_build" </> "bin" </> bin_name

lib_dest :: FilePath
lib_dest = "dist_build" </> "lib"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_shake", shakeVerbosity=Chatty} $ do
  let build_zip = "project-m36.zip"
      script_packages = ["base", "project-m36"]
      binaries = ["tutd"]
      -- libraries required by AtomFunctionBody scripting
      libs = atomFunctionBodyDeps
  want $ "libs" : map binary_dest binaries
  {-build_zip %> \out -> do
    need (
    cmd "zip" out dist_binaries-}
  --create a directory containing all the components for the distribution
  (map binary_src binaries) |%> \out -> do
      let bin_name = takeFileName out
      cmd "cabal build" bin_name
  (map binary_dest binaries) |%> \out -> do
      let bin_name = takeFileName out
      need [binary_src bin_name]
      liftIO $ putStrLn ("spam" ++ show out)
      copyFileChanged (binary_src bin_name) out
  "libs" ~> do
    --find the ghc pkg
    forM_ libs $ \pkg -> do
      Stdout cmd_result <- cmd ["cabal", "sandbox", "hc-pkg", "field", pkg, "import-dirs"]
      let expected_import_dirs = "import-dirs: "
          pkg_dir = case cmd_result of 
            _ | expected_import_dirs `isPrefixOf` cmd_result -> init (drop (length expected_import_dirs) cmd_result)
            _ -> error ("Failed to find " ++ pkg)
          dest_dir = lib_dest </> last (splitPath pkg_dir)

      object_files <- getDirectoryFiles pkg_dir ["*.so", "*.hi", "*.dyn_hi"]
      mapM (\obj -> copyFileChanged (pkg_dir </> obj) (dest_dir </> obj)) object_files

  
--ghc-pkg field containers import-dirs --find the package