module ProjectM36.AtomType where
import ProjectM36.Base
import qualified ProjectM36.TypeConstructorDef as TCD
import qualified ProjectM36.TypeConstructor as TC
import qualified ProjectM36.DataConstructorDef as DCD
import ProjectM36.MiscUtils
import ProjectM36.Error
import ProjectM36.DataTypes.Primitive
import ProjectM36.AttributeExpr as AE
import qualified ProjectM36.Attribute as A
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Either (rights, lefts)
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Text as T

findDataConstructor :: DataConstructorName -> TypeConstructorMapping -> Maybe (TypeConstructorDef, DataConstructorDef)
findDataConstructor dName = foldr tConsFolder Nothing
  where
    tConsFolder (tCons, dConsList) accum = if isJust accum then
                                accum
                              else
                                case findDCons dConsList of
                                  Just dCons -> Just (tCons, dCons)
                                  Nothing -> Nothing
    findDCons dConsList = case filter (\dCons -> DCD.name dCons == dName) dConsList of
      [] -> Nothing
      [dCons] -> Just dCons
      _ -> error "More than one data constructor with the same name found"
  
{-
-- | Scan the atom types and return the resultant ConstructedAtomType or error.
-- Used in typeFromAtomExpr to validate argument types.
atomTypeForDataConstructorName :: DataConstructorName -> [AtomType] -> TypeConstructorMapping -> Either RelationalError AtomType
-- search for the data constructor and resolve the types' names
atomTypeForDataConstructorName dConsName atomTypesIn tConsList =
  case findDataConstructor dConsName tConsList of
    Nothing -> Left (NoSuchDataConstructorError dConsName)
    Just (tCons, dCons) -> do
      dConsVars <- resolveDataConstructorTypeVars dCons atomTypesIn tConsList      
      let tConsVars = M.fromList (map TypeVariableType (TCD.typeVars tCons))
          allVars = M.union dConsVars tConsVars
      ConstructedAtomType (TCD.name tCons) <$> allVars
-}
        
atomTypeForDataConstructorDefArg :: DataConstructorDefArg -> AtomType -> TypeConstructorMapping ->  Either RelationalError AtomType
atomTypeForDataConstructorDefArg (DataConstructorDefTypeConstructorArg tCons) aType tConss = do
  isValidAtomTypeForTypeConstructor aType tCons tConss
  pure aType

atomTypeForDataConstructorDefArg (DataConstructorDefTypeVarNameArg _) aType _ = Right aType --any type is OK
        
-- | Used to determine if the atom arguments can be used with the data constructor.  
-- | This is the entry point for type-checking from RelationalExpression.hs
atomTypeForDataConstructor :: TypeConstructorMapping -> DataConstructorName -> [AtomType] -> Either RelationalError AtomType
atomTypeForDataConstructor tConss dConsName atomArgTypes =
  --lookup the data constructor
  case findDataConstructor dConsName tConss of
    Nothing -> Left (NoSuchDataConstructorError dConsName)
    Just (tCons, dCons) -> do
      --validate that the type constructor arguments are fulfilled in the data constructor
      dConsVars <- resolveDataConstructorTypeVars dCons atomArgTypes tConss
      let tConsVars = M.fromList (map (\v -> (v, TypeVariableType v)) (TCD.typeVars tCons))
          allVars = M.union dConsVars tConsVars
          unresolvedType = ConstructedAtomType (TCD.name tCons) allVars
      --validateAtomType unresolvedType tConss -- do not validate here because the type may not be fully resolved at this point
      pure unresolvedType
      
-- | Walks the data and type constructors to extract the type variable map.
resolveDataConstructorTypeVars :: DataConstructorDef -> [AtomType] -> TypeConstructorMapping -> Either RelationalError TypeVarMap
resolveDataConstructorTypeVars dCons@(DataConstructorDef _ defArgs) aTypeArgs tConss = do
  let defCount = length defArgs
      argCount = length aTypeArgs
  if defCount /= argCount then
    Left (ConstructedAtomArgumentCountMismatchError defCount argCount)
    else do
    maps <- mapM (\(dCons',aTypeArg) -> resolveDataConstructorArgTypeVars dCons' aTypeArg tConss) (zip (DCD.fields dCons) aTypeArgs)
  --if any two maps have the same key and different values, this indicates a type arg mismatch
    let typeVarMapFolder valMap acc = case acc of
          Left err -> Left err
          Right accMap -> 
            case resolveAtomTypesInTypeVarMap valMap accMap of
              Left (TypeConstructorTypeVarMissing _) -> Left (DataConstructorTypeVarsMismatch (DCD.name dCons) accMap valMap)
              Left err -> Left err
              Right ok -> pure ok
    case foldr typeVarMapFolder (Right M.empty) maps of
      Left err -> Left err
      Right typeVarMaps -> pure typeVarMaps
  --if the data constructor cannot complete a type constructor variables (ex. "Nothing" could be Maybe Int or Maybe Text, etc.), then fill that space with TypeVar which is resolved when the relation is constructed- the relation must contain all resolved atom types.


-- | Attempt to match the data constructor argument to a type constructor type variable.
resolveDataConstructorArgTypeVars :: DataConstructorDefArg -> AtomType -> TypeConstructorMapping -> Either RelationalError TypeVarMap
resolveDataConstructorArgTypeVars (DataConstructorDefTypeConstructorArg tCons) aType tConss = resolveTypeConstructorTypeVars tCons aType tConss
  
resolveDataConstructorArgTypeVars (DataConstructorDefTypeVarNameArg pVarName) aType _ = Right (M.singleton pVarName aType)

resolveTypeConstructorTypeVars :: TypeConstructor -> AtomType -> TypeConstructorMapping -> Either RelationalError TypeVarMap
resolveTypeConstructorTypeVars (PrimitiveTypeConstructor _ pType) aType _ = 
  if aType /= pType then
    Left (AtomTypeMismatchError pType aType)
  else
    Right M.empty

resolveTypeConstructorTypeVars (ADTypeConstructor tConsName _) (ConstructedAtomType tConsName' pVarMap') tConss = 
  if tConsName /= tConsName' then
    Left (TypeConstructorNameMismatch tConsName tConsName')
  else
    case findTypeConstructor tConsName tConss of
      Nothing -> Left (NoSuchTypeConstructorName tConsName)
      Just (tConsDef, _) -> let expectedPVarNames = S.fromList (TCD.typeVars tConsDef) in
        if M.keysSet pVarMap' `S.isSubsetOf` expectedPVarNames then
          Right pVarMap' 
        else
          Left (TypeConstructorTypeVarsMismatch expectedPVarNames (M.keysSet pVarMap'))
resolveTypeConstructorTypeVars (TypeVariable tvName) typ _ = case typ of
  TypeVariableType nam -> Left (TypeConstructorTypeVarMissing nam)
  _ -> Right (M.singleton tvName typ)
resolveTypeConstructorTypeVars (ADTypeConstructor tConsName []) typ tConss = case findTypeConstructor tConsName tConss of
  Just (PrimitiveTypeConstructorDef _ _, _) -> Right M.empty
  _ -> Left (TypeConstructorAtomTypeMismatch tConsName typ)

resolveTypeConstructorTypeVars (ADTypeConstructor tConsName _) typ _ = Left (TypeConstructorAtomTypeMismatch tConsName typ)

resolveTypeConstructorTypeVars (RelationAtomTypeConstructor attrExprs) typ tConsMap = 
  case typ of
    RelationAtomType attrs -> 
      --resolve any typevars in the attrExprs 
      M.unions <$> mapM (\(expectedAtomType, attrExpr) -> resolveAttributeExprTypeVars attrExpr expectedAtomType tConsMap) (zip (A.atomTypesList attrs) attrExprs)
    otherType -> Left (AtomTypeMismatchError typ otherType)
      
--used when recursing down sub-relation type definition
resolveAttributeExprTypeVars :: AttributeExprBase a -> AtomType -> TypeConstructorMapping -> Either RelationalError TypeVarMap
resolveAttributeExprTypeVars (NakedAttributeExpr attr) aType _ = pure $ resolveTypeVariable (A.atomType attr) aType
resolveAttributeExprTypeVars (AttributeAndTypeNameExpr _ tCons _) aType tConsMap = resolveTypeConstructorTypeVars tCons aType tConsMap
    
-- check that type vars on the right also appear on the left
-- check that the data constructor names are unique      
validateTypeConstructorDef :: TypeConstructorDef -> [DataConstructorDef] -> [RelationalError]
validateTypeConstructorDef tConsDef dConsList = execWriter $ do
  let duplicateDConsNames = dupes (L.sort (map DCD.name dConsList))
  mapM_ tell [map DataConstructorNameInUseError duplicateDConsNames]
  let leftSideVars = S.fromList (TCD.typeVars tConsDef)
      rightSideVars = S.unions (map DCD.typeVars dConsList)
      varsDiff = S.difference leftSideVars rightSideVars
  mapM_ tell [map DataConstructorUsesUndeclaredTypeVariable (S.toList varsDiff)]

atomTypeForTypeConstructor :: TypeConstructor -> TypeConstructorMapping -> TypeVarMap -> Either RelationalError AtomType
atomTypeForTypeConstructor = atomTypeForTypeConstructorValidate False

-- | Create an atom type iff all type variables are provided.
-- Either Int Text -> ConstructedAtomType "Either" {Int , Text}
atomTypeForTypeConstructorValidate :: Bool -> TypeConstructor -> TypeConstructorMapping -> TypeVarMap -> Either RelationalError AtomType
atomTypeForTypeConstructorValidate _ (PrimitiveTypeConstructor _ aType) _ _ = Right aType
atomTypeForTypeConstructorValidate validate (TypeVariable tvname) _ tvMap = case M.lookup tvname tvMap of
  Nothing -> if validate then
                Left (TypeConstructorTypeVarMissing tvname)
             else
               pure (TypeVariableType tvname)
  Just typ -> Right typ
atomTypeForTypeConstructorValidate _ (RelationAtomTypeConstructor attrExprs) tConsMap tvMap = do
  resolvedAtomTypes <- mapM (\expr -> atomTypeForAttributeExpr expr tConsMap tvMap) attrExprs
  let attrs = zipWith Attribute (map AE.attributeName attrExprs) resolvedAtomTypes
  pure (RelationAtomType (A.attributesFromList attrs))
atomTypeForTypeConstructorValidate val tCons tConss tvMap = case findTypeConstructor (TC.name tCons) tConss of
  Nothing -> Left (NoSuchTypeConstructorError (TC.name tCons))
  Just (PrimitiveTypeConstructorDef _ aType, _) -> Right aType
  Just (tConsDef, _) -> do
          tConsArgTypes <- mapM (\tConsArg -> atomTypeForTypeConstructorValidate val tConsArg tConss tvMap) (TC.arguments tCons)    
          let pVarNames = TCD.typeVars tConsDef
              tConsArgs = M.fromList (zip pVarNames tConsArgTypes)
          Right (ConstructedAtomType (TC.name tCons) tConsArgs)      

      
atomTypeForAttributeExpr :: AttributeExprBase a -> TypeConstructorMapping -> TypeVarMap -> Either RelationalError AtomType      
atomTypeForAttributeExpr (NakedAttributeExpr attr) _ _ = pure (A.atomType attr)
atomTypeForAttributeExpr (AttributeAndTypeNameExpr _ tCons _) tConsMap tvMap = atomTypeForTypeConstructorValidate True tCons tConsMap tvMap

--reconcile the atom-in types with the type constructors
isValidAtomTypeForTypeConstructor :: AtomType -> TypeConstructor -> TypeConstructorMapping -> Either RelationalError ()
isValidAtomTypeForTypeConstructor aType (PrimitiveTypeConstructor _ expectedAType) _ = if expectedAType /= aType then Left (AtomTypeMismatchError expectedAType aType) else pure ()

--lookup constructor name and check if the incoming atom types are valid
isValidAtomTypeForTypeConstructor (ConstructedAtomType tConsName _) (ADTypeConstructor expectedTConsName _) _ =  if tConsName /= expectedTConsName then Left (TypeConstructorNameMismatch expectedTConsName tConsName) else pure ()

isValidAtomTypeForTypeConstructor (RelationAtomType attrs) (RelationAtomTypeConstructor attrExprs) tConsMap = do
  evaldAtomTypes <- mapM (\expr -> atomTypeForAttributeExpr expr tConsMap M.empty) attrExprs
  mapM_ (uncurry resolveAtomType) (zip (map A.atomType (V.toList attrs)) evaldAtomTypes)
isValidAtomTypeForTypeConstructor aType tCons _ = Left (AtomTypeTypeConstructorReconciliationError aType (TC.name tCons))

findTypeConstructor :: TypeConstructorName -> TypeConstructorMapping -> Maybe (TypeConstructorDef, [DataConstructorDef])
findTypeConstructor name = foldr tConsFolder Nothing
  where
    tConsFolder (tCons, dConsList) accum = if TCD.name tCons == name then
                                     Just (tCons, dConsList)
                                   else
                                     accum

resolveAttributes :: Attribute -> Attribute -> Either RelationalError Attribute
resolveAttributes attrA attrB =
  if A.attributeName attrA /= A.attributeName attrB then
    Left $ AttributeNamesMismatchError (S.fromList (map A.attributeName [attrA, attrB]))
  else
    Attribute (A.attributeName attrA) <$> resolveAtomType (A.atomType attrA) (A.atomType attrB)
    
--given two atom types, try to resolve type variables                                     
resolveAtomType :: AtomType -> AtomType -> Either RelationalError AtomType  
resolveAtomType (ConstructedAtomType tConsName resolvedTypeVarMap) (ConstructedAtomType _ unresolvedTypeVarMap) =
  ConstructedAtomType tConsName <$> resolveAtomTypesInTypeVarMap resolvedTypeVarMap unresolvedTypeVarMap
resolveAtomType typeFromRelation unresolvedType = 
  if typeFromRelation == unresolvedType then
    Right typeFromRelation
  else
    Left (AtomTypeMismatchError typeFromRelation unresolvedType)
                                                    
{-
--walk an `AtomType` and apply the type variables in the map
resolveAtomTypeVars :: TypeVarMap -> AtomType -> AtomType   
resolveAtomTypeVars tvMap typ@(TypeVariableType nam) = fromMaybe typ (M.lookup nam tvMap)
resolveAtomTypeVars tvMap (RelationAtomType relAttrs) = 
-}                                                    
-- this could be optimized to reduce new tuple creation
resolveAtomTypesInTypeVarMap :: TypeVarMap -> TypeVarMap -> Either RelationalError TypeVarMap
resolveAtomTypesInTypeVarMap resolvedTypeMap unresolvedTypeMap = do
  {-
  let resKeySet = traceShowId $ M.keysSet resolvedTypeMap
      unresKeySet = traceShowId $ M.keysSet unresolvedTypeMap
  when (resKeySet /= unresKeySet) (Left $ TypeConstructorTypeVarsMismatch resKeySet unresKeySet)
  

  let lookupOrDef key tMap = case M.lookup key tMap of
        Nothing -> Left (TypeConstructorTypeVarMissing key)
        Just val -> Right val
  -}
  let resolveTypePair resKey resType =
        -- if the key is missing in the unresolved type map, then fill it in with the value from the resolved map
        case M.lookup resKey unresolvedTypeMap of
          Just unresType -> case unresType of
            --do we need to recurse for RelationAtomType?
            subType@(ConstructedAtomType _ _) -> do
                resSubType <- resolveAtomType resType subType
                pure (resKey, resSubType)
            typ ->
              let typ' = if isResolvedType typ then typ else resType in
                pure (resKey, typ')
          Nothing ->
            pure (resKey, resType) --swipe the missing type var from the expected map
  tVarList <- mapM (uncurry resolveTypePair) (M.toList resolvedTypeMap)
  pure (M.fromList tVarList)
  
-- | See notes at `resolveTypesInTuple`. The typeFromRelation must not include any wildcards.
resolveTypeInAtom :: AtomType -> Atom -> TypeConstructorMapping -> Either RelationalError Atom
resolveTypeInAtom typeFromRelation@(ConstructedAtomType _ tvMap) atomIn@(ConstructedAtom dConsName _ args) tConss = do
  newType <- resolveAtomType typeFromRelation (atomTypeForAtom atomIn)
  case findDataConstructor dConsName tConss of
    Nothing -> -- the atom may have been constructed using a constructor function without a public data constructor, no further resolution is possible
      pure atomIn
    Just (_, dConsDef) -> do
      atomArgTypes <- resolvedAtomTypesForDataConstructorDefArgs tConss tvMap dConsDef
      newArgs <- mapM (\(atom, atomTyp) -> resolveTypeInAtom atomTyp atom tConss) (zip args atomArgTypes)
      pure (ConstructedAtom dConsName newType newArgs)
resolveTypeInAtom (RelationAtomType attrs) (RelationAtom (Relation _ tupSet)) tConss = do
  let newTups = mapM (resolveTypesInTuple attrs tConss) (asList tupSet)
  RelationAtom . Relation attrs . RelationTupleSet <$> newTups
resolveTypeInAtom _ atom _ = Right atom
  
-- | When creating a tuple, the data constructor may not complete the type constructor arguments, so the wildcard "TypeVar x" fills in the type constructor's argument. The tuple type must be resolved before it can be part of a relation, however.
-- Example: "Nothing" does not specify the the argument in "Maybe a", so allow delayed resolution in the tuple before it is added to the relation. Note that this resolution could cause a type error. Hardly a Hindley-Milner system.
resolveTypesInTuple :: Attributes -> TypeConstructorMapping -> RelationTuple -> Either RelationalError RelationTuple
resolveTypesInTuple resolvedAttrs tConss (RelationTuple _ tupAtoms) = do
  newAtoms <- mapM (\(atom, resolvedType) -> resolveTypeInAtom resolvedType atom tConss) (zip (V.toList tupAtoms) $ map A.atomType (V.toList resolvedAttrs))
  Right (RelationTuple resolvedAttrs (V.fromList newAtoms))
                           
-- | Validate that the type is provided with complete type variables for type constructors.
validateAtomType :: AtomType -> TypeConstructorMapping -> Either RelationalError ()
validateAtomType typ@(ConstructedAtomType tConsName tVarMap) tConss =
  case findTypeConstructor tConsName tConss of 
    Nothing -> Left (TypeConstructorAtomTypeMismatch tConsName typ)
    Just (tConsDef, _) -> case tConsDef of
      ADTypeConstructorDef _ tVarNames -> let expectedTyVarNames = S.fromList tVarNames
                                              actualTyVarNames = M.keysSet tVarMap
                                              diff = S.difference expectedTyVarNames actualTyVarNames in
                                          if not (S.null diff) then
                                            Left $ TypeConstructorTypeVarsMismatch expectedTyVarNames actualTyVarNames
                                          else
                                            validateTypeVarMap tVarMap tConss
      _ -> Right ()
validateAtomType (RelationAtomType attrs) tConss =
  mapM_ (\attr ->
         validateAtomType (A.atomType attr) tConss) (V.toList attrs)
validateAtomType (TypeVariableType x) _ = Left (TypeConstructorTypeVarMissing x)  
validateAtomType _ _ = pure ()

validateAttributes :: TypeConstructorMapping -> Attributes -> Either RelationalError ()
validateAttributes tConss attrs =
  if length errs > 0 then
    Left (someErrors errs)
  else
    pure ()
  where
    errs = lefts $ map ((`validateAtomType` tConss) . A.atomType) (V.toList attrs)

--ensure that all type vars are fully resolved
validateTypeVarMap :: TypeVarMap -> TypeConstructorMapping -> Either RelationalError ()
validateTypeVarMap tvMap tConss = mapM_ (`validateAtomType` tConss) $ M.elems tvMap

validateTuple :: RelationTuple -> TypeConstructorMapping -> Either RelationalError ()
validateTuple (RelationTuple _ atoms) tConss = mapM_ (`validateAtom` tConss) atoms

--ensure that all types are fully resolved
validateAtom :: Atom -> TypeConstructorMapping -> Either RelationalError ()
validateAtom (RelationAtom (Relation _ tupSet)) tConss = mapM_ (`validateTuple` tConss) (asList tupSet)
validateAtom (ConstructedAtom _ dConsType atomArgs) tConss = do
  validateAtomType dConsType tConss
  mapM_ (`validateAtom` tConss) atomArgs
validateAtom _ _ = pure ()
  

-- | Determine if two types are equal or compatible (including special handling for TypeVar x).
atomTypeVerify :: AtomType -> AtomType -> Either RelationalError AtomType
atomTypeVerify (TypeVariableType _) x = Right x
atomTypeVerify x (TypeVariableType _) = Right x
atomTypeVerify x@(ConstructedAtomType tConsNameA tVarMapA) (ConstructedAtomType tConsNameB tVarMapB) 
  | tConsNameA /= tConsNameB = Left (TypeConstructorNameMismatch tConsNameA tConsNameB)
  | not (typeVarMapsVerify tVarMapA tVarMapB) = Left (TypeConstructorTypeVarsTypesMismatch tConsNameA tVarMapA tVarMapB)
  | otherwise = Right x

atomTypeVerify x@(RelationAtomType attrs1) y@(RelationAtomType attrs2) = do
  mapM_ (\(attr1,attr2) -> let name1 = A.attributeName attr1
                               name2 = A.attributeName attr2 in
                               if notElem "_" [name1, name2] && name1 /= name2 then 
                                 Left $ AtomTypeMismatchError x y
                               else
                                 atomTypeVerify (A.atomType attr1) (A.atomType attr2)) $ V.toList (V.zip attrs1 attrs2)
  return x
atomTypeVerify x y = if x == y then
                       Right x
                     else
                       Left $ AtomTypeMismatchError x y

-- | Determine if two typeVars are logically compatible.
typeVarMapsVerify :: TypeVarMap -> TypeVarMap -> Bool
typeVarMapsVerify a b = M.keysSet a == M.keysSet b && (length . rights) (map (\((_,v1),(_,v2)) -> atomTypeVerify v1 v2) (zip (M.toAscList a) (M.toAscList b))) == M.size a

prettyAtomType :: AtomType -> T.Text
prettyAtomType (RelationAtomType attrs) = "relation {" `T.append` T.intercalate "," (map prettyAttribute (V.toList attrs)) `T.append` "}"
prettyAtomType (ConstructedAtomType tConsName typeVarMap) = tConsName `T.append` T.concat (map showTypeVars (M.toList typeVarMap))
  where
    showTypeVars (_, TypeVariableType x) = " " <> x
    showTypeVars (tyVarName, aType) = " (" `T.append` tyVarName `T.append` "::" `T.append` prettyAtomType aType `T.append` ")"
-- it would be nice to have the original ordering, but we don't have access to the type constructor here- maybe the typevarmap should be also positional (ordered map?)
prettyAtomType (TypeVariableType x) = x
prettyAtomType aType = T.take (T.length fullName - T.length "AtomType") fullName
  where fullName = (T.pack . show) aType

prettyAttribute :: Attribute -> T.Text
prettyAttribute (Attribute _ (TypeVariableType x)) = x
prettyAttribute attr = A.attributeName attr `T.append` "::" `T.append` prettyAtomType (A.atomType attr)

resolveTypeVariables :: [AtomType] -> [AtomType] -> Either RelationalError TypeVarMap  
resolveTypeVariables expectedArgTypes actualArgTypes = do
  let tvmaps = zipWith resolveTypeVariable expectedArgTypes actualArgTypes
  --if there are any new keys which don't have equal values then we have a conflict!
  foldM (\acc tvmap -> do
            let inter = M.intersectionWithKey (\tvName vala valb -> 
                                                if vala /= valb then
                                                  Left (AtomFunctionTypeVariableMismatch tvName vala valb)
                                                else
                                                  Right vala) acc tvmap
                errs = lefts (M.elems inter)
            case errs of
              [] -> pure (M.unions tvmaps)
              errs' -> Left (someErrors errs')) M.empty tvmaps
  
resolveTypeVariable :: AtomType -> AtomType -> TypeVarMap
resolveTypeVariable (TypeVariableType tv) typ = M.singleton tv typ
resolveTypeVariable (ConstructedAtomType _ _) (ConstructedAtomType _ actualTvMap) = actualTvMap
resolveTypeVariable _ _ = M.empty

resolveFunctionReturnValue :: AtomFunctionName -> TypeVarMap -> AtomType -> Either RelationalError AtomType
resolveFunctionReturnValue funcName tvMap (ConstructedAtomType tCons retMap) = do
  let diff = M.difference retMap tvMap
  if M.null diff then
    pure (ConstructedAtomType tCons (M.intersection tvMap retMap))
    else
    Left (AtomFunctionTypeVariableResolutionError funcName (fst (head (M.toList diff))))
resolveFunctionReturnValue funcName tvMap (TypeVariableType tvName) = case M.lookup tvName tvMap of
  Nothing -> Left (AtomFunctionTypeVariableResolutionError funcName tvName)
  Just typ -> pure typ
resolveFunctionReturnValue _ _ typ = pure typ

-- convert a typevarmap and data constructor definition into a list of atomtypes which represent the arguments-- no type variables are allowed to remain
resolvedAtomTypesForDataConstructorDefArgs :: TypeConstructorMapping -> TypeVarMap -> DataConstructorDef -> Either RelationalError [AtomType] 
resolvedAtomTypesForDataConstructorDefArgs tConsMap tvMap (DataConstructorDef _ args) = mapM (resolvedAtomTypeForDataConstructorDefArg tConsMap tvMap) args

resolvedAtomTypeForDataConstructorDefArg :: TypeConstructorMapping -> TypeVarMap -> DataConstructorDefArg -> Either RelationalError AtomType
resolvedAtomTypeForDataConstructorDefArg tConsMap tvMap (DataConstructorDefTypeConstructorArg typCons) = atomTypeForTypeConstructor typCons tConsMap tvMap
resolvedAtomTypeForDataConstructorDefArg _ tvMap (DataConstructorDefTypeVarNameArg tvName) = case M.lookup tvName tvMap of
  Nothing -> Left (DataConstructorUsesUndeclaredTypeVariable tvName)
  Just typ -> Right typ

isResolvedType :: AtomType -> Bool
isResolvedType typ =
  case typ of
    IntAtomType -> True
    IntegerAtomType -> True
    DoubleAtomType -> True
    TextAtomType -> True
    DayAtomType -> True
    DateTimeAtomType -> True
    ByteStringAtomType -> True
    BoolAtomType -> True
    RelationAtomType attrs -> isResolvedAttributes attrs
    ConstructedAtomType _ tvMap -> all isResolvedType (M.elems tvMap)
    TypeVariableType _ -> False

isResolvedAttributes :: Attributes -> Bool
isResolvedAttributes attrs = all isResolvedAttribute (V.toList attrs)

isResolvedAttribute :: Attribute -> Bool
isResolvedAttribute = isResolvedType . A.atomType

--given two AtomTypes x,y
