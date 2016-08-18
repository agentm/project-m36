{-# LANGUAGE ScopedTypeVariables #-}
--tools to execute an atom function body
module ProjectM36.AtomFunctionBody where
import ProjectM36.Base
import ProjectM36.Error

import Control.Monad.IO.Class
import Control.Exception
import Data.Dynamic
import Data.Text

import GHC
import GHC.Paths (libdir)
import DynFlags
import Outputable hiding ((<>))
import PprTyThing
import Type hiding (pprTyThing)
import TysWiredIn

type ScriptSession = HscEnv

-- | Configure a GHC environment/session which we will use for all script compilation.
initScriptSession :: IO ScriptSession
initScriptSession = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let dflags' = dflags { hscTarget = HscInterpreted , 
                         ghcLink = LinkInMemory, 
                         safeHaskell = Sf_Safe,
                         safeInfer = True,
                         safeInferred = True,
                         --trustFlags = [TrustPackage "base"] -- new in 8
                         packageFlags = (packageFlags dflags) ++ [TrustPackage "base"]
                         }
                `xopt_set` Opt_ExtendedDefaultRules
                `xopt_set` Opt_ImplicitPrelude
                `gopt_set` Opt_DistrustAllPackages 
                `xopt_set` Opt_ScopedTypeVariables
                `gopt_set` Opt_PackageTrust
                --`gopt_set` Opt_ImplicitImportQualified
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

  baseTarget <- guessTarget "ProjectM36.ScriptBase" Nothing
  addTarget baseTarget
  flag <- load LoadAllTargets
  case flag of
    Failed -> error "failed to load"
    Succeeded -> do
      setContext [ IIDecl $ safeImportDecl (mkModuleName "Prelude"),
                   IIDecl $ safeImportDecl (mkModuleName "ProjectM36.ScriptBase")]
      env <- getSession
      pure env
  
addImport :: String -> Ghc ()
addImport moduleNam = do
  ctx <- getContext
  setContext ( (IIDecl $ simpleImportDecl (mkModuleName moduleNam)) : ctx )
  
showType :: DynFlags -> Type -> String
showType dflags ty = showSDocForUser dflags alwaysQualify (pprTypeForUser ty)  

-- | Typecheck and validate the 
typeCheckAtomFunctionScript :: AtomFunctionBodyScript -> Ghc (Maybe AtomFunctionBodyCompilationError)    
typeCheckAtomFunctionScript inp = do
  dflags <- getSessionDynFlags  
  --catch exception for SyntaxError
  funcType <- GHC.exprType (unpack inp)

  let expectedType = atomFunctionBodyGhcType

  if eqType funcType expectedType then
    pure Nothing
    else
    pure (Just (TypeCheckCompilationError (showType dflags expectedType) (showType dflags funcType)))
    
atomFunctionBodyGhcType :: Type    
atomFunctionBodyGhcType = Type.mkFunTy intTy intTy

-- | After compiling the script, it must accept a list of Atoms and return an Atom, otherwise an AtomFunctionBodyCompilationError is returned
compileAtomFunctionScript :: AtomFunctionBodyScript -> Ghc (Either AtomFunctionBodyCompilationError AtomFunctionBodyType)
compileAtomFunctionScript script = do
  let sScript = unpack script
  mErr <- typeCheckAtomFunctionScript script
  case mErr of
    Just err -> pure (Left err)
    Nothing -> do
      --catch exception here
      --we could potentially wrap the script with Atom pattern matching so that the script doesn't have to do it, but the change to an Atom ADT should make it easier. Still, it would be nice if the script didn't have to handle a list of arguments, for example.
      mDyn <- fromDynamic <$> dynCompileExpr sScript
      case mDyn of
        Nothing -> pure $ Left (TypeCheckCompilationError "" "") --this really shouldn't happen
        Just funcBody -> pure $ Right funcBody
        
catchCompileException :: MonadIO m => IO a -> m (Either AtomFunctionBodyCompilationError a)
catchCompileException m = liftIO $ do
    mres <- try m
    case mres of
      Left (err :: SomeException) -> do
        pure (Left (OtherScriptCompilationError (show err)))
      Right res -> pure (Right res)

compiledAtomFunctionBody :: AtomFunctionBodyType -> AtomFunctionBody  
compiledAtomFunctionBody func = AtomFunctionBody Nothing func
