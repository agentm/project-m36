{-# LANGUAGE OverloadedStrings #-}
module ProjectM36.AtomType where
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.DataTypes.Primitive
import qualified ProjectM36.TypeConstructor as TC
import qualified ProjectM36.DataConstructor as DC
import qualified Data.Text as T
import ProjectM36.Error
import ProjectM36.Attribute
import ProjectM36.Atom
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe (isJust, catMaybes)
import Control.Monad.Writer

typesAsRelation :: TypeConstructors -> Either RelationalError Relation
typesAsRelation types = mkRelationFromTuples attrs tuples
  where
    attrs = attributesFromList [Attribute "TypeConstructor" textAtomType,
                                Attribute "DataConstructors" dConsType]
    subAttrs = attributesFromList [Attribute "DataConstructor" textAtomType]
    dConsType = RelationAtomType subAttrs
    tuples = map mkTypeConsDescription types
    
    mkTypeConsDescription (tCons, dConsList) = RelationTuple attrs (V.fromList [textAtom (TC.name tCons), mkDataConsRelation dConsList])
    
    mkDataConsRelation dConsList = case mkRelationFromTuples subAttrs $ map (\dCons -> RelationTuple subAttrs (V.singleton $ textAtom $ T.intercalate " " ((DC.name dCons):(map (T.pack . show) (DC.fields dCons))))) dConsList of
      Left err -> error ("mkRelationFromTuples pooped " ++ show err)
      Right rel -> Atom rel
      
      
{-      
-- | Scan all type constructors for the proper data constructor      
dataConstructorTypeNamesForName :: DataConstructorName -> AtomTypes -> Either RelationalError [AtomTypeName]
dataConstructorTypeNamesForName dConsName aTypes = M.foldrWithKey typeFolder defaultErr aTypes
  where
    defaultErr = Left (NoSuchDataConstructorError dConsName)
    typeFolder _ (_, AtomConstructor dConsMap) accum = if isRight accum then
                                                                            accum
                                                                         else 
                                                                           case M.lookup dConsName dConsMap of
      Nothing -> accum
      Just aTypeNames -> Right (dConsName, aTypeNames)
-}
-- fix mostly-duplicated fold code
      
      
findDataConstructor :: DataConstructorName -> TypeConstructors -> Maybe (TypeConstructor, DataConstructor)
findDataConstructor dName tConsList = foldr tConsFolder Nothing tConsList
  where
    tConsFolder (tCons, dConsList) accum = if isJust accum then
                                accum
                              else
                                case findDCons dConsList of
                                  Just dCons -> Just (tCons, dCons)
                                  Nothing -> Nothing
    findDCons dConsList = case filter (\dCons -> DC.name dCons == dName) dConsList of
      [] -> Nothing
      [dCons] -> Just dCons
      _ -> error "More than one data constructor with the same name found"
  
-- | Scan the atom types and return the resultant ConstructedAtomType or error.
-- Used in typeFromAtomExpr to validate argument types.
atomTypeForDataConstructorName :: DataConstructorName -> [AtomType] -> TypeConstructors -> Either RelationalError AtomType
-- search for the data constructor and resolve the types' names
atomTypeForDataConstructorName dConsName atomTypesIn tConsList = do
  case findDataConstructor dConsName tConsList of
    Nothing -> Left (NoSuchDataConstructorError dConsName)
    Just (tCons, dCons) -> do
      let validation = map (\(aTypeIn, tConsArgIn) -> validAtomTypeWithTypeConstructorArg aTypeIn tConsArgIn tConsList) (zip atomTypesIn (DC.fields dCons))
          errors = catMaybes validation
      if length errors /= 0 then
        Left (head errors)
        else
        Right (ConstructedAtomType (TC.name tCons) atomTypesIn)
        
validAtomTypeWithTypeConstructorArg :: AtomType -> TypeConstructorArg -> TypeConstructors -> Maybe RelationalError
validAtomTypeWithTypeConstructorArg aType (TypeConstructorArg tCons) tConsList = validAtomTypeWithTypeConstructor aType tCons tConsList
validAtomTypeWithTypeConstructorArg _ (TypeConstructorPolymorphicArg _) _ = Just IncompletelyDefinedAtomTypeWithConstructorError
    
--reconcile the atom-in types with the type constructors
validAtomTypeWithTypeConstructor :: AtomType -> TypeConstructor -> TypeConstructors -> Maybe RelationalError
validAtomTypeWithTypeConstructor aType (PrimitiveTypeConstructor _ expectedAType) _ = if expectedAType /= aType then Just (AtomTypeMismatchError expectedAType aType) else Nothing

--lookup constructor name and check if the incoming atom types are valid
validAtomTypeWithTypeConstructor (ConstructedAtomType tConsName _) (ADTypeConstructor expectedTConsName _) _ =  if tConsName /= expectedTConsName then Just (TypeConstructorNameMismatch expectedTConsName tConsName) else Nothing

validAtomTypeWithTypeConstructor aType tCons _ = Just (AtomTypeTypeConstructorReconciliationError aType tCons)

-- | Used to determine if the atom arguments can be used with the data constructor.  
atomTypeWithDataConstructor :: TypeConstructors -> DataConstructorName -> [AtomType] -> Either RelationalError AtomType
atomTypeWithDataConstructor tConss dConsName atomArgTypes = do
  --lookup the data constructor
  case findDataConstructor dConsName tConss of
    Nothing -> Left (NoSuchDataConstructorError dConsName)
    Just (tCons, dCons) -> do
      --match the data constructor arguments to the atom args
      let validator (tConsArg', aType) = maybe (Right ()) Left (validAtomTypeWithTypeConstructorArg aType tConsArg' tConss)
      mapM_ validator (zip (DC.fields dCons) atomArgTypes)  
      pure (ConstructedAtomType (TC.name tCons) atomArgTypes)
      
-- check that polymorphic vars on the right also appear on the left
-- check that the data constructor names are unique      
validateTypeConstructor :: TypeConstructor -> [DataConstructor] -> [RelationalError]
validateTypeConstructor tCons dConsList = execWriter $ do
  let duplicateDConsNames = dupes (L.sort (map DC.name dConsList))
  mapM_ tell [map DataConstructorNameInUseError duplicateDConsNames]
  let leftSideVars = TC.polymorphicVariables tCons
      rightSideVars = concatMap DC.polymorphicVariables dConsList
      varsDiff = S.difference (S.fromList leftSideVars) (S.fromList rightSideVars)
  mapM_ tell [map DataConstructorUsesUndeclaredTypeVariable (S.toList varsDiff)]
  pure ()
    
--returns duplicates of a pre-sorted list
dupes :: Eq a => [a] -> [a]    
dupes [] = []
dupes (_:[]) = []
dupes (x:y:[]) = if x == y then [x] else []
dupes (x:y:xs) = dupes(x:[y]) ++ dupes(xs)

atomTypeForTypeConstructor :: TypeConstructor -> TypeConstructors -> Either RelationalError AtomType
atomTypeForTypeConstructor tCons tConsList = foldr tConsFolder defaultErr (map fst tConsList)
  where
    tConsFolder tCons' accum = if TC.name tCons == TC.name tCons' then do
      tConsArgTypes <- mapM ((flip atomTypeForTypeConstructorArg) tConsList) (TC.arguments tCons')
      Right (ConstructedAtomType (TC.name tCons') tConsArgTypes)
                               else 
                                 accum
    defaultErr = Left (NoSuchTypeConstructorName (TC.name tCons))
                                     
atomTypeForTypeConstructorArg :: TypeConstructorArg -> TypeConstructors -> Either RelationalError AtomType
atomTypeForTypeConstructorArg (TypeConstructorPolymorphicArg _) _ = Left IncompletelyDefinedAtomTypeWithConstructorError
atomTypeForTypeConstructorArg (TypeConstructorArg tCons) tConsList = atomTypeForTypeConstructor tCons tConsList

findTypeConstructor :: TypeConstructorName -> TypeConstructors -> Maybe (TypeConstructor, [DataConstructor])
findTypeConstructor name tConsList = foldr tConsFolder Nothing tConsList
  where
    tConsFolder (tCons, dConsList) accum = if TC.name tCons == name then
                                     Just (tCons, dConsList)
                                   else
                                     accum
                                    