module ProjectM36.ScriptSession where

import ProjectM36.Error

import Control.Exception
import Control.Monad
import System.IO.Error
import System.Directory
import Control.Monad.IO.Class
import System.FilePath.Glob
import System.FilePath
import Data.Text (Text, unpack)
import Data.Maybe

import GHC
import GHC.Paths (libdir)
#if __GLASGOW_HASKELL__ >= 800
import GHC.LanguageExtensions
#endif
import DynFlags
import Panic
import Outputable --hiding ((<>))
import PprTyThing
import Unsafe.Coerce
import Type hiding (pprTyThing)  

data ScriptSession = ScriptSession {
  hscEnv :: HscEnv, 
  atomFunctionBodyType :: Type,
  dbcFunctionBodyType :: Type
  }
                     
data ScriptSessionError = ScriptSessionLoadError GhcException
                          deriving (Show)

-- | Configure a GHC environment/session which we will use for all script compilation.
initScriptSession :: [String] -> IO (Either ScriptSessionError ScriptSession)
initScriptSession ghcPkgPaths = do
    --for the sake of convenience, for developers' builds, include the local cabal sandbox package database and the cabal new-build package database
  eHomeDir <- tryJust (guard . isDoesNotExistError) getHomeDirectory
  let homeDir = either (const "/") id eHomeDir
  let excHandler exc = pure $ Left (ScriptSessionLoadError exc)
  handleGhcException excHandler $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let ghcVersion = projectVersion dflags

    sandboxPkgPaths <- liftIO $ liftM concat $ mapM glob [
      "./dist-newstyle/packagedb/ghc-" ++ ghcVersion,
      ".cabal-sandbox/*ghc-" ++ ghcVersion ++ "-packages.conf.d", 
      homeDir </> ".cabal/store/ghc-" ++ ghcVersion ++ "/package.db"
      ]
    
    let localPkgPaths = map PkgConfFile (ghcPkgPaths ++ sandboxPkgPaths)
      
    let dflags' = applyGopts . applyXopts $ dflags { hscTarget = HscInterpreted , 
                           ghcLink = LinkInMemory, 
                           safeHaskell = Sf_Trustworthy,
                           safeInfer = True,
                           safeInferred = True,
                           --verbosity = 3,
#if __GLASGOW_HASKELL__ >= 800                           
                           trustFlags = map TrustPackage required_packages,
#endif                                        
                           packageFlags = (packageFlags dflags) ++ packages,
                           extraPkgConfs = const (localPkgPaths ++ [UserPkgConf, GlobalPkgConf])
                         }
        applyGopts flags = foldl gopt_set flags gopts
        applyXopts flags = foldl xopt_set flags xopts
#if __GLASGOW_HASKELL__ >= 800
        xopts = [OverloadedStrings, ExtendedDefaultRules, ImplicitPrelude, ScopedTypeVariables]
#else               
        xopts = [Opt_OverloadedStrings, Opt_ExtendedDefaultRules, Opt_ImplicitPrelude,  Opt_ScopedTypeVariables]
#endif
        gopts = [] --[Opt_DistrustAllPackages, Opt_PackageTrust]
        required_packages = ["base", 
                             "containers",
                             "Glob",
                             "directory",
                             "unordered-containers",
                             "hashable",
                             "uuid",
                             "vector",
                             "text",
                             "binary",
                             "vector-binary-instances",
                             "time",
                             "project-m36",
                             "bytestring"]
#if __GLASGOW_HASKELL__ >= 800
        packages = map (\m -> ExposePackage ("-package " ++ m) (PackageArg m) (ModRenaming True [])) required_packages
#else
        packages = map TrustPackage required_packages
#endif
  --liftIO $ traceShowM (showSDoc dflags' (ppr packages))
    _ <- setSessionDynFlags dflags'
    let safeImportDecl mn mQual = ImportDecl {
          ideclSourceSrc = Nothing,
          ideclName      = noLoc mn,
          ideclPkgQual   = Nothing,
          ideclSource    = False,
          ideclSafe      = True,
          ideclImplicit  = False,
          ideclQualified = (isJust mQual),
          ideclAs        = mQual,
          ideclHiding    = Nothing
          }
        unqualifiedModules = map (\modn -> IIDecl $ safeImportDecl (mkModuleName modn) Nothing) [
          "Prelude",
          "Data.Map",
          "Data.Either",
          "Data.Time.Calendar",
          "Control.Monad.State",
          "ProjectM36.Base",
          "ProjectM36.Relation",
          "ProjectM36.AtomFunctionError",
          "ProjectM36.DatabaseContextFunctionError",
          "ProjectM36.RelationalExpression"]
        qualifiedModules = map (\(modn, qualNam) -> IIDecl $ safeImportDecl (mkModuleName modn) (Just (mkModuleName qualNam))) [
          ("Data.Text", "T")
          ]
    setContext (unqualifiedModules ++ qualifiedModules)
    env <- getSession
    atomFuncType <- mkTypeForName "AtomFunctionBodyType"
    dbcFuncType <- mkTypeForName "DatabaseContextFunctionBodyType"
    pure (Right (ScriptSession env atomFuncType dbcFuncType))

addImport :: String -> Ghc ()
addImport moduleNam = do
  ctx <- getContext
  setContext ( (IIDecl $ simpleImportDecl (mkModuleName moduleNam)) : ctx )
  
showType :: DynFlags -> Type -> String
showType dflags ty = showSDocForUser dflags alwaysQualify (pprTypeForUser ty)

mkTypeForName :: String -> Ghc Type
mkTypeForName name = do
  lBodyName <- parseName name
  case lBodyName of
    [] -> error ("failed to parse " ++ name)
    _:_:_ -> error "too many name matches"
    bodyName:[] -> do
      mThing <- lookupName bodyName
      case mThing of
        Nothing -> error ("failed to find " ++ name)
        Just (ATyCon tyCon) -> case synTyConRhs_maybe tyCon of
          Just typ -> pure typ
          Nothing -> error (name ++ " is not a type synonym")
        Just _ -> error ("failed to find type synonym " ++ name)

compileScript :: Type -> Text -> Ghc (Either ScriptCompilationError a)
compileScript funcType script = do
  let sScript = unpack script
  mErr <- typeCheckScript funcType script
  case mErr of
    Just err -> pure (Left err)
    Nothing -> do
      --catch exception here
      --we could potentially wrap the script with Atom pattern matching so that the script doesn't have to do it, but the change to an Atom ADT should make it easier. Still, it would be nice if the script didn't have to handle a list of arguments, for example.
      -- we can't use dynCompileExpr here because
      func <- compileExpr sScript
      pure $ Right (unsafeCoerce func)
      
typeCheckScript :: Type -> Text -> Ghc (Maybe ScriptCompilationError)    
typeCheckScript expectedType inp = do
  dflags <- getSessionDynFlags  
  --catch exception for SyntaxError
  funcType <- GHC.exprType (unpack inp)
  --liftIO $ putStrLn $ showType dflags expectedType ++ ":::" ++ showType dflags funcType 
  if eqType funcType expectedType then
    pure Nothing
    else
    pure (Just (TypeCheckCompilationError (showType dflags expectedType) (showType dflags funcType)))
      