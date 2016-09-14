{-# LANGUAGE CPP #-}
--build distributions for various platforms using shake
import Development.Shake
import Development.Shake.FilePath
--du -hs /biimport ProjectM36.AtomFunctionBodyDeps
import System.Directory
 
bundle_name :: FilePath
bundle_name = "project-m36-" ++ PROJECTM36_VERSION

binary_dest :: FilePath -> FilePath
binary_dest bin_name  = dist_build </> bundle_name </> "bin" </> bin_name

dist_build :: FilePath
dist_build = "dist_build"

--lib_dest :: FilePath
--lib_dest = "dist_build" </> "lib"

--current system: copy stack-built binaries into "dist_build", then create a zip file
main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_shake", shakeVerbosity=Chatty} $ do
  let binaries = ["tutd", 
                  "project-m36-server",
                  "project-m36-websocket-server"]
      bin_zip = dist_build </> bundle_name ++ ".zip"
      -- libraries required by AtomFunctionBody scripting
      --libs = atomFunctionBodyDeps
      dist_bins = map binary_dest binaries
  want [bin_zip]
  bin_zip %> \out -> do
    --check where the binaries are located
    Stdout res <- cmd "stack path --dist-dir"
    let dist_dir = init res
    --copy binaries to dist_build
    mapM_ (\bin -> copyFileChanged (dist_dir </> "build" </> bin </> bin) (binary_dest bin)) binaries
    zip_abs <- liftIO $ makeAbsolute out
    let stripInitPath = joinPath . tail . splitPath 
    cmd (Cwd dist_build) "zip" zip_abs (map stripInitPath dist_bins)
   
-- ghc-pkgs are NOT relocatable, so we need a different strategy if we want it to be relocatable      
{-  "libs" ~> do
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
-}
  
