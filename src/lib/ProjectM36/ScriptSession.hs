{-# LANGUAGE UnboxedTuples, KindSignatures, DataKinds #-}
#ifdef PM36_HASKELL_SCRIPTING
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif
module ProjectM36.ScriptSession where

#ifdef PM36_HASKELL_SCRIPTING
import ProjectM36.Error
import ProjectM36.Base
import GHC
import GHC.Core.TyCo.Rep
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

import Unsafe.Coerce
import GHC.LanguageExtensions (Extension(OverloadedStrings,ExtendedDefaultRules,ImplicitPrelude,ScopedTypeVariables))
-- GHC 9.4+
import Data.List.NonEmpty(NonEmpty(..))
import GHC.Utils.Panic (handleGhcException)
import GHC.Driver.Session (projectVersion, PackageDBFlag(PackageDB), PkgDbRef(PkgDbPath), TrustFlag(TrustPackage), gopt_set, xopt_set, PackageFlag(ExposePackage), PackageArg(PackageArg), ModRenaming(ModRenaming))
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Driver.Ppr (showSDocForUser)
import GHC.Utils.Outputable (ppr, Outputable)
--import GHC.Unit.Module.Graph (showModMsg, mgModSummaries')
import GHC.Core.TyCo.Ppr (pprType)
import GHC.Utils.Encoding (zEncodeString)
import GHC.Unit.State (emptyUnitState)
import GHC.Types.PkgQual (RawPkgQual(NoRawPkgQual))
import GHC.Exts (addrToAny#)
import GHC.Ptr (Ptr(..))
import GHCi.ObjLink (initObjLinker, ShouldRetainCAFs(RetainCAFs), resolveObjs, lookupSymbol, loadDLL, loadObj)
--import GHC.Builtin.Types (integerTyCon, intTyCon, doubleTyCon, eqTyCon)
--import qualified Data.Map as M
import Data.List (find)
import qualified Control.Monad.Catch as CMC

#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Unit.Types (UnitId)
import GHC.Driver.Env (hsc_home_unit)
import GHC.Unit.Home (homeUnitId)
#endif
#endif
-- endif for SCRIPTING FLAG

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

data LoadSymbolError = LoadSymbolError | SecurityLoadSymbolError
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
      homeDir </> ".local/state/cabal/store/ghc-" ++ ghcVersion ++ "/package.db",
      ".cabal-sandbox/*ghc-" ++ ghcVersion ++ "-packages.conf.d",
      ".stack-work/install/*/*/" ++ ghcVersion ++ "/pkgdb",
      ".stack-work/install/*/pkgdb/", --windows stack build
      "C:/sr/snapshots/b201cfe6/pkgdb", --windows stack build- ideally, we could run `stack path --snapshot-pkg-db, but this is sufficient to pass CI
      homeDir </> ".stack/snapshots/*/*/" ++ ghcVersion ++ "/pkgdb",
      --github CI path
      homeDir </> (".cabal/store/ghc-" ++ ghcVersion) </> "package.db"
      ]
#if MIN_VERSION_ghc(9,0,0)
    let pkgConf = PkgDbPath
#else
    let pkgConf = PkgConfFile
#endif
    let localPkgPaths = map pkgConf (ghcPkgPaths ++ sandboxPkgPaths ++ maybeToList mNixLibDir)

    let dflags' = applyGopts . applyXopts $ dflags {
#if MIN_VERSION_ghc(9,6,0)
                           backend = interpreterBackend,
#elif MIN_VERSION_ghc(9,2,0)
                           backend = Interpreter,
#else  
                           hscTarget = HscInterpreted ,
#endif
                           ghcLink = LinkInMemory,
                           safeHaskell = Sf_Trustworthy,
                           safeInfer = True,
                           safeInferred = True,
                           --verbosity = 3,

                           trustFlags = map TrustPackage required_packages,
                           packageFlags = packageFlags dflags ++ packages,
                           packageDBFlags = map PackageDB localPkgPaths

--                           extraPkgConfs = const (localPkgPaths ++ [UserPkgConf, GlobalPkgConf])
        }

        applyGopts flags = foldl gopt_set flags gopts
        applyXopts flags = foldl xopt_set flags xopts
        xopts = [OverloadedStrings, ExtendedDefaultRules, ImplicitPrelude, ScopedTypeVariables]
        gopts = [] --[Opt_DistrustAllPackages, Opt_PackageTrust]
        required_packages = ["base",
                             "containers",
                             "Glob",
                             "directory",
                             "unordered-containers",
                             "hashable",
                             "uuid",
                             "mtl",
                             "vector",
                             "text",
                             "time",
                             "project-m36",
                             "bytestring"]
        packages = map (\m -> ExposePackage ("-package " ++ m) (PackageArg m) (ModRenaming True [])) required_packages
  --liftIO $ traceShowM (showSDoc dflags' (ppr packages))
    _ <- setSessionDynFlags dflags'
    let unqualifiedModules = map (\modn -> IIDecl $ safeImportDecl modn Nothing) [
          "Prelude",
          "Data.Map",
          "Data.Either",
          "Data.Time.Calendar",
--          "Data.Scientific",
          "Control.Monad.State",
          "ProjectM36.Base",
          "ProjectM36.Error",
          "ProjectM36.Relation",
          "ProjectM36.DatabaseContext",
          "ProjectM36.DatabaseContext.Types",
          "ProjectM36.AtomFunctionError",
          "ProjectM36.RelationalExpression"]
        qualifiedModules = map (\(modn, qualNam) -> IIDecl $ safeImportDecl modn (Just qualNam)) [
          ("Data.Text", "T")
          ]
    setContext (unqualifiedModules ++ qualifiedModules)
    env <- getSession
{-    case mgLookupModule (mkModuleName "ProjectM36") moduleInfos of
       Nothing -> error "failed to load project-m36 module"
       Just moduleInfo ->
  -}        
    atomFuncType <- mkTypeForName "AtomFunctionBodyType"
    dbcFuncType <- mkTypeForName "DatabaseContextFunctionBodyType"

    pure (Right (ScriptSession env atomFuncType dbcFuncType))

safeImportDecl :: String -> Maybe String -> ImportDecl (GhcPass 'Parsed)
safeImportDecl fullModuleName _mQualifiedName = ImportDecl {
#if MIN_VERSION_ghc(9,6,0)
#else
          ideclSourceSrc = NoSourceText,
#endif
#if MIN_VERSION_ghc(9,6,0)
          ideclImportList = Nothing,
#endif
#if MIN_VERSION_ghc(9,10,0)
          ideclExt = XImportDeclPass
            { ideclAnn = noAnn
            , ideclSourceText = NoSourceText
            , ideclImplicit = False
            },
#elif MIN_VERSION_ghc(9,6,0)
          ideclExt = XImportDeclPass
            { ideclAnn = EpAnnNotUsed
            , ideclSourceText = NoSourceText
            , ideclImplicit = False
            },
#elif MIN_VERSION_ghc(9,2,0)
          ideclExt = noAnn,
          ideclImplicit  = False,
#else
          ideclExt = noExtField,
          ideclImplicit  = False,
#endif
#if MIN_VERSION_ghc(9,2,0)
          --GenLocated SrcSpanAnnA ModuleName
          ideclName      = noLocA (mkModuleName fullModuleName),
#else
          ideclName      = noLoc (mkModuleName fullModuleName),
#endif
#if MIN_VERSION_ghc(9,4,0)
          ideclPkgQual   = NoRawPkgQual,
#else
          ideclPkgQual   = Nothing,
#endif
#if MIN_VERSION_ghc(9,0,0)
          ideclSource    = NotBoot,
#else
          ideclSource    = False,
#endif
          ideclQualified = if isJust _mQualifiedName then QualifiedPre else NotQualified,
#if MIN_VERSION_ghc(9,6,0)
          ideclAs        = Nothing,
#elif MIN_VERSION_ghc(9,2,0)
          ideclAs        = Just (noLocA (mkModuleName fullModuleName)),
#else
          ideclAs        = noLoc . mkModuleName <$> _mQualifiedName,
#endif
#if MIN_VERSION_ghc(9,6,0)
#else
          ideclHiding    = Nothing,
#endif
          ideclSafe      = True
          }


addImport :: String -> Ghc ()
addImport moduleNam = do
  ctx <- getContext
  setContext (IIDecl (simpleImportDecl (mkModuleName moduleNam)) : ctx)

showType :: DynFlags -> Type -> String
#if MIN_VERSION_ghc(9,4,0)
showType dflags ty = showSDocForUser dflags emptyUnitState alwaysQualify (pprType ty)  
#elif MIN_VERSION_ghc(9,2,0)
showType dflags ty = showSDocForUser dflags emptyUnitState alwaysQualify (pprTypeForUser ty)
#else
showType dflags ty = showSDocForUser dflags alwaysQualify (pprTypeForUser ty)
#endif

mkTypeForName :: String -> Ghc Type
mkTypeForName name = do
  lBodyName <- parseName name
  case lBodyName of
#if MIN_VERSION_ghc(9,6,0)
    _ :| (_:_) -> error "too many name matches"
    bodyName :| _ -> do
#else
    [] -> error ("failed to parse " ++ name)
    _:_:_ -> error "too many name matches"
    [bodyName] -> do
#endif
      mThing <- lookupName bodyName
      case mThing of
        Nothing -> error ("failed to find " ++ name)
        Just (ATyCon tyCon) -> case synTyConRhs_maybe tyCon of
          Just typ -> pure typ
          Nothing -> error (name ++ " is not a type synonym")
        Just _ -> error ("failed to find type synonym " ++ name)

compileScript :: Type -> Text -> Ghc (Either ScriptCompilationError a)
compileScript funcType' script = do
  let sScript = unpack script
  mErr <- typeCheckScript funcType' script
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
  funcType' <- GHC.exprType TM_Inst (unpack inp)
  --liftIO $ putStrLn $ showType dflags expectedType ++ ":::" ++ showType dflags funcType
  if eqType funcType' expectedType then
    pure Nothing
    else
    pure (Just (TypeCheckCompilationError (showType dflags expectedType) (showType dflags funcType')))

mangleSymbol :: Maybe String -> String -> String -> String
mangleSymbol pkg module' valsym =
    prefixUnderscore ++
      maybe "" (\p -> zEncodeString p ++ "_") pkg ++
      zEncodeString module' ++ "_" ++ zEncodeString valsym ++ "_closure"

data ObjectLoadMode = LoadObjectFile | -- ^ load .o files only
                      LoadDLLFile | -- ^ load .so .dynlib .dll files only
                      LoadAutoObjectFile -- ^ determine which object mode to use based on the file name's extension

-- | Load either a .o or dynamic library based on the file name's extension.


-- | Load a function from an relocatable object file (.o or .so)
--   If a modulesDir is specified, only load a path relative to the modulesDir (no ..)

type ModuleDirectory = FilePath

loadFunctionFromDirectory :: ObjectLoadMode -> ModName -> FuncName -> FilePath -> FilePath -> IO (Either LoadSymbolError a)
loadFunctionFromDirectory mode modName funcName' modDir objPath =
  if takeFileName objPath /= objPath then
    pure (Left SecurityLoadSymbolError)
  else
    let fullObjPath = modDir </> objPath in
      loadFunction mode modName funcName' fullObjPath


loadFunction :: ObjectLoadMode -> ModName -> FuncName -> FilePath -> IO (Either LoadSymbolError a)
loadFunction loadMode modName funcName' objPath = do
  initObjLinker RetainCAFs
  let loadFuncForSymbol = do    
        _ <- resolveObjs
        ptr <- lookupSymbol (mangleSymbol Nothing modName funcName')
        case ptr of
          Nothing -> pure (Left LoadSymbolError)
          Just (Ptr addr) -> case addrToAny# addr of
            (# hval #) -> pure (Right hval)
  case loadMode of
    LoadAutoObjectFile ->
      if takeExtension objPath == ".o" then
          loadFunction LoadObjectFile modName funcName' objPath
        else
          loadFunction LoadDLLFile modName funcName' objPath
    LoadObjectFile -> do
      loadObj objPath
      loadFuncForSymbol
    LoadDLLFile -> do
#if MIN_VERSION_ghc(9,10,0)
      eErr <- loadDLL objPath
      case eErr of
        Left _err -> pure (Left LoadSymbolError)
        Right _ptr -> loadFuncForSymbol
#else
      mErr <- loadDLL objPath
      case mErr of
        Just _ -> pure (Left LoadSymbolError)
        Nothing -> loadFuncForSymbol
#endif

prefixUnderscore :: String
prefixUnderscore =
    case (os,arch) of
      ("mingw32","x86_64") -> ""
      ("cygwin","x86_64") -> ""
      ("mingw32",_) -> "_"
      ("darwin",_) -> "_"
      ("cygwin",_) -> "_"
      _ -> ""

getHomeUnitId :: ScriptSession -> UnitId
getHomeUnitId sSession =
  homeUnitId (hsc_home_unit (hscEnv sSession))

data TypeConversionError = UnsupportedTypeConversionError String
  deriving Show

mkTypeConversions :: GhcMonad m => m [(Type, AtomType)]
mkTypeConversions = do
  let extTypes = [("Integer", IntegerAtomType),
                  ("Int", IntAtomType),
                  ("Double", DoubleAtomType),
                  ("Scientific", ScientificAtomType), -- found in multiple modules?
                  ("Text", TextAtomType),
                  ("Day", DayAtomType),
                  ("UTCTime", DateTimeAtomType),
                  ("ByteString", ByteStringAtomType),
                  ("Bool", BoolAtomType),
                  ("UUID", UUIDAtomType)
                 ]
  res <- forM extTypes $ \(nam, typ) -> do
    -- extract type from bottom value
    ghcType <- (Just <$> exprType TM_Default ("undefined :: " <> nam)) `CMC.catch` (\(_ :: SomeException) -> pure Nothing)
    pure (ghcType, typ)
  let mapFilter (Nothing, _) = Nothing
      mapFilter (Just t, at) = Just (t, at)
  pure (mapMaybe mapFilter res)
                                
convertGhcTypeToFunctionAtomType :: DynFlags -> [(Type, AtomType)] -> Type -> Either TypeConversionError [AtomType]
convertGhcTypeToFunctionAtomType dflags tyConv typ =
  case typ of
    TyConApp _tycon _args -> do
      aType <- findTypeConv dflags typ tyConv
      pure [aType]
    fun@FunTy{} -> do
      arg <- convertGhcTypeToFunctionAtomType dflags tyConv (ft_arg fun)
      rest <- convertGhcTypeToFunctionAtomType dflags tyConv (ft_res fun)
      pure (arg <> rest)
    other -> Left (UnsupportedTypeConversionError (pprShow other dflags))

    
pprShow :: Outputable a => a -> DynFlags -> String
pprShow x dflags = showSDocForUser dflags emptyUnitState alwaysQualify (ppr x)

findTypeConv :: DynFlags -> Type -> [(Type, AtomType)] -> Either TypeConversionError AtomType
findTypeConv dflags typ tyConv =
  case find (\(k,_v) -> k `eqType` typ) tyConv of
    Nothing -> Left (UnsupportedTypeConversionError (pprShow typ dflags))
    Just (_,match') -> 
      pure match'

-- | Last argument should be `DatabaseContextFunctionMonad ()`
convertGhcTypeToDatabaseContextFunctionAtomType :: DynFlags -> [(Type, AtomType)] -> Type -> Type -> Either TypeConversionError [AtomType]
convertGhcTypeToDatabaseContextFunctionAtomType dflags tyConv dbcFuncMonadType typ =
  case typ of
    TyConApp{} |
      typ `eqType` dbcFuncMonadType -> pure []
    TyConApp{} -> do
      aType <- findTypeConv dflags typ tyConv
      pure [aType]
    fun@FunTy{} -> do
      arg <- convertGhcTypeToDatabaseContextFunctionAtomType dflags tyConv dbcFuncMonadType (ft_arg fun)
      rest <- convertGhcTypeToDatabaseContextFunctionAtomType dflags tyConv dbcFuncMonadType (ft_res fun)
      pure (arg <> rest)
    other -> Left (UnsupportedTypeConversionError (pprShow other dflags))
#endif
