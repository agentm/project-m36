{-# LANGUAGE ScopedTypeVariables #-}
--tools to execute an atom function body
module ProjectM36.AtomFunctionBody where
import ProjectM36.Base
import ProjectM36.Error

import Control.Monad.IO.Class
import Control.Exception
import Data.Text hiding (map, foldl)

import Unsafe.Coerce
import GHC
import GHC.Paths (libdir)
import GHC.LanguageExtensions
import DynFlags
import Panic
import Outputable --hiding ((<>))
import PprTyThing
import Type hiding (pprTyThing)
import System.FilePath.Glob
import Debug.Trace

data ScriptSession = ScriptSession {
  hscEnv :: HscEnv, 
  atomFunctionBodyType :: Type
  }
                     
data ScriptSessionError = ScriptSessionLoadError GhcException
                          deriving (Show)

-- | Configure a GHC environment/session which we will use for all script compilation.
initScriptSession :: [String] -> IO (Either ScriptSessionError ScriptSession)
initScriptSession ghcPkgPaths = do
    --for the sake of convenience, for developers' builds, include the local cabal sandbox pacakge database
  sandboxPkgPaths <- liftIO (glob ".cabal-sandbox/*packages.conf.d")
  let excHandler exc = pure $ Left (ScriptSessionLoadError exc)
  handleGhcException excHandler $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
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
    let safeImportDecl mn = ImportDecl {
          ideclSourceSrc = Nothing,
          ideclName      = noLoc mn,
          ideclPkgQual   = Nothing,
          ideclSource    = False,
          ideclSafe      = True,
          ideclImplicit  = False,
          ideclQualified = False,
          ideclAs        = Nothing,
          ideclHiding    = Nothing
          }
    setContext (map (\modn -> IIDecl $ safeImportDecl (mkModuleName modn))
                ["Prelude",
                 "Data.Map",
                 "ProjectM36.Base"])
    env <- getSession
    fType <- mkAtomFunctionBodyType
    pure (Right (ScriptSession env fType))
      
mkAtomFunctionBodyType :: Ghc Type      
mkAtomFunctionBodyType = do
  lBodyName <- parseName "AtomFunctionBodyType"
  case lBodyName of
    [] -> error "failed to parse AtomFunctionBodyType"
    _:_:_ -> error "too many name matches"
    bodyName:[] -> do
      mThing <- lookupName bodyName
      case mThing of
        Nothing -> error "failed to find AtomFunctionBodyType"
        Just (ATyCon tyCon) -> case synTyConRhs_maybe tyCon of
          Just typ -> pure typ
          Nothing -> error "AtomFunctionBodyType is not a type synonym"
        Just _ -> error "failed to find type synonym AtomFunctionBodyType"
  
addImport :: String -> Ghc ()
addImport moduleNam = do
  ctx <- getContext
  setContext ( (IIDecl $ simpleImportDecl (mkModuleName moduleNam)) : ctx )
  
showType :: DynFlags -> Type -> String
showType dflags ty = showSDocForUser dflags alwaysQualify (pprTypeForUser ty)  

-- | Typecheck and validate the 
typeCheckAtomFunctionScript :: Type -> AtomFunctionBodyScript -> Ghc (Maybe AtomFunctionBodyCompilationError)    
typeCheckAtomFunctionScript expectedType inp = do
  dflags <- getSessionDynFlags  
  --catch exception for SyntaxError
  funcType <- GHC.exprType (unpack inp)

  if eqType funcType expectedType then
    pure Nothing
    else
    pure (Just (TypeCheckCompilationError (showType dflags expectedType) (showType dflags funcType)))
    
-- | After compiling the script, it must accept a list of Atoms and return an Atom, otherwise an AtomFunctionBodyCompilationError is returned
compileAtomFunctionScript :: ScriptSession -> AtomFunctionBodyScript -> Ghc (Either AtomFunctionBodyCompilationError AtomFunctionBodyType)
compileAtomFunctionScript (ScriptSession _ funcType) script = do
  let sScript = unpack script
  mErr <- typeCheckAtomFunctionScript funcType script
  case mErr of
    Just err -> pure (Left err)
    Nothing -> do
      --catch exception here
      --we could potentially wrap the script with Atom pattern matching so that the script doesn't have to do it, but the change to an Atom ADT should make it easier. Still, it would be nice if the script didn't have to handle a list of arguments, for example.
      -- we can't use dynCompileExpr here because
      func <- compileExpr sScript
      pure $ Right (unsafeCoerce func)
        
catchCompileException :: MonadIO m => IO a -> m (Either AtomFunctionBodyCompilationError a)
catchCompileException m = liftIO $ do
    mres <- try m
    case mres of
      Left (err :: SomeException) -> do
        pure (Left (OtherScriptCompilationError (show err)))
      Right res -> pure (Right res)

compiledAtomFunctionBody :: AtomFunctionBodyType -> AtomFunctionBody  
compiledAtomFunctionBody func = AtomFunctionBody Nothing func
