{-# LANGUAGE UnboxedTuples, KindSignatures, DataKinds #-}
#ifdef PM36_HASKELL_SCRIPTING
{-# LANGUAGE MagicHash #-}
#endif
module ProjectM36.ScriptSession where

#ifdef PM36_HASKELL_SCRIPTING
import ProjectM36.Error
import GHC
import Control.Exception
import Control.Monad
import System.IO.Error
import System.Directory
import Control.Monad.IO.Class
import System.FilePath.Glob
import System.FilePath
import System.Info (os, arch)
import Data.Text (Text, unpack)
import Data.Maybe
import GHC.Paths (libdir)
import System.Environment
#if __GLASGOW_HASKELL__ >= 800
import GHC.LanguageExtensions
import GHCi.ObjLink
#else
import ObjLink
#endif
#if __GLASGOW_HASKELL__ >= 802
import BasicTypes
#endif
import DynFlags
import Panic
import Outputable --hiding ((<>))
import PprTyThing
import Unsafe.Coerce
#if __GLASGOW_HASKELL__ >= 802
import Type
#elif __GLASGOW_HASKELL__ >= 710
import Type hiding (pprTyThing)
#else
#endif
import GHC.Exts (addrToAny#)
import GHC.Ptr (Ptr(..))
import Encoding
#endif


data ScriptSession = ScriptSession {
#ifdef PM36_HASKELL_SCRIPTING
  hscEnv :: HscEnv,
  atomFunctionBodyType :: Type,
  dbcFunctionBodyType :: Type
#endif
  }

#ifdef PM36_HASKELL_SCRIPTING
data ScriptSessionError = ScriptSessionLoadError GhcException
                        | ScriptingDisabled
                          deriving (Show)
#else
data ScriptSessionError = ScriptingDisabled
  deriving (Show)
#endif

data LoadSymbolError = LoadSymbolError
type ModName = String
type FuncName = String

-- | Configure a GHC environment/session which we will use for all script compilation.
initScriptSession :: [String] -> IO (Either ScriptSessionError ScriptSession)
#if !defined(PM36_HASKELL_SCRIPTING)
initScriptSession _ = pure (Left ScriptingDisabled)
#else
initScriptSession ghcPkgPaths = do
    --for the sake of convenience, for developers' builds, include the local cabal sandbox package database and the cabal new-build package database
  eHomeDir <- tryJust (guard . isDoesNotExistError) getHomeDirectory
  let homeDir = either (const "/") id eHomeDir
  let excHandler exc = pure $ Left (ScriptSessionLoadError exc)
  handleGhcException excHandler $ runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let ghcVersion = projectVersion dflags
    -- get nix packages dir, if available
    mNixLibDir <- liftIO $ lookupEnv "NIX_GHC_LIBDIR"

    sandboxPkgPaths <- liftIO $ concat <$> mapM glob [
      --"./dist-newstyle/packagedb/ghc-" ++ ghcVersion, --rely on cabal 3's ghc.environment install: "cabal install --lib project-m36"
      ".cabal-sandbox/*ghc-" ++ ghcVersion ++ "-packages.conf.d",
      ".stack-work/install/*/*/" ++ ghcVersion ++ "/pkgdb",
      ".stack-work/install/*/pkgdb/", --windows stack build
      "C:/sr/snapshots/b201cfe6/pkgdb", --windows stack build- ideally, we could run `stack path --snapshot-pkg-db, but this is sufficient to pass CI
      homeDir </> ".stack/snapshots/*/*/" ++ ghcVersion ++ "/pkgdb"
      --homeDir </> ".cabal/store/ghc-" ++ ghcVersion ++ "/package.db"
      ]

    let localPkgPaths = map PkgConfFile (ghcPkgPaths ++ sandboxPkgPaths ++ maybeToList mNixLibDir)

    let dflags' = applyGopts . applyXopts $ dflags { hscTarget = HscInterpreted ,
                           ghcLink = LinkInMemory,
                           safeHaskell = Sf_Trustworthy,
                           safeInfer = True,
                           safeInferred = True,
                           --verbosity = 3,
#if __GLASGOW_HASKELL__ >= 800
                           trustFlags = map TrustPackage required_packages,
#endif
                           packageFlags = packageFlags dflags ++ packages,
#if __GLASGOW_HASKELL__ >= 802
                           packageDBFlags = map PackageDB localPkgPaths
#else
                           extraPkgConfs = const (localPkgPaths ++ [UserPkgConf, GlobalPkgConf])
#endif

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
                             "mtl",
                             "uuid",
                             "vector",
                             "text",
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
#if __GLASGOW_HASKELL__ >= 802
          ideclSourceSrc = NoSourceText,
#else
          ideclSourceSrc = Nothing,
#endif

#if __GLASGOW_HASKELL__ >= 810
          ideclExt = noExtField,
#elif __GLASGOW_HASKELL__ >= 806
          ideclExt = NoExt,
#endif
          ideclName      = noLoc mn,
          ideclPkgQual   = Nothing,
          ideclSource    = False,
          ideclSafe      = True,
          ideclImplicit  = False,
#if __GLASGOW_HASKELL__ >= 810
          ideclQualified = if isJust mQual then QualifiedPre else NotQualified,
#else
          ideclQualified = isJust mQual,
#endif
          ideclAs        = mQual,
          ideclHiding    = Nothing
          }
#if __GLASGOW_HASKELL__ >= 806
          :: ImportDecl (GhcPass (c :: Pass))
#endif
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
          "ProjectM36.DatabaseContextFunctionUtils",
          "ProjectM36.RelationalExpression"]
#if __GLASGOW_HASKELL__ >= 802
        mkModName = noLoc . mkModuleName
#else
        mkModName = mkModuleName
#endif
        qualifiedModules = map (\(modn, qualNam) -> IIDecl $ safeImportDecl (mkModuleName modn) (Just (mkModName qualNam))) [
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
  setContext (IIDecl (simpleImportDecl (mkModuleName moduleNam)) : ctx)

showType :: DynFlags -> Type -> String
showType dflags ty = showSDocForUser dflags alwaysQualify (pprTypeForUser ty)

mkTypeForName :: String -> Ghc Type
mkTypeForName name = do
  lBodyName <- parseName name
  case lBodyName of
    [] -> error ("failed to parse " ++ name)
    _:_:_ -> error "too many name matches"
    [bodyName] -> do
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
    Nothing ->
      --catch exception here
      --we could potentially wrap the script with Atom pattern matching so that the script doesn't have to do it, but the change to an Atom ADT should make it easier. Still, it would be nice if the script didn't have to handle a list of arguments, for example.
      -- we can't use dynCompileExpr here because
       Right . unsafeCoerce <$> compileExpr sScript

typeCheckScript :: Type -> Text -> Ghc (Maybe ScriptCompilationError)
typeCheckScript expectedType inp = do
  dflags <- getSessionDynFlags
  --catch exception for SyntaxError
#if __GLASGOW_HASKELL__ >= 802
  funcType <- GHC.exprType TM_Inst (unpack inp)
#else
  funcType <- GHC.exprType (unpack inp)
#endif
  --liftIO $ putStrLn $ showType dflags expectedType ++ ":::" ++ showType dflags funcType
  if eqType funcType expectedType then
    pure Nothing
    else
    pure (Just (TypeCheckCompilationError (showType dflags expectedType) (showType dflags funcType)))

mangleSymbol :: Maybe String -> String -> String -> String
mangleSymbol pkg module' valsym =
    prefixUnderscore ++
      maybe "" (\p -> zEncodeString p ++ "_") pkg ++
      zEncodeString module' ++ "_" ++ zEncodeString valsym ++ "_closure"

loadFunction :: ModName -> FuncName -> FilePath -> IO (Either LoadSymbolError a)
loadFunction modName funcName objPath = do
#if __GLASGOW_HASKELL__ >= 802
  initObjLinker RetainCAFs
#else
  initObjLinker
#endif
  loadObj objPath
  _ <- resolveObjs
  ptr <- lookupSymbol (mangleSymbol Nothing modName funcName)
  case ptr of
    Nothing -> pure (Left LoadSymbolError)
    Just (Ptr addr) -> case addrToAny# addr of
      (# hval #) -> pure (Right hval)

prefixUnderscore :: String
prefixUnderscore =
    case (os,arch) of
      ("mingw32","x86_64") -> ""
      ("cygwin","x86_64") -> ""
      ("mingw32",_) -> "_"
      ("darwin",_) -> "_"
      ("cygwin",_) -> "_"
      _ -> ""
#endif
