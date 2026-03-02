-- | Implements client-side TutorialD templating for use in Haskell code including parameterized replacement.

-- if I want users to use tutoriald from client libraries then it needs to be bundled in the project-m36 library OR I need to make a separate client library (less appealing)
{-# LANGUAGE FlexibleInstances, TypeApplications, StandaloneDeriving, DeriveLift #-}
module TutorialD.Interpreter.Template where
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Interpreter.TransGraphRelationalOperator
import Text.Megaparsec
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import ProjectM36.Base
import ProjectM36.Relation
import ProjectM36.Tuple
import ProjectM36.TransactionGraph
import ProjectM36.DataTypes.Primitive
import ProjectM36.AccessControlList
import Instances.TH.Lift ()
import qualified Data.Vector as V
import qualified Data.Map as M

relationalExpr :: QuasiQuoter
relationalExpr =
    QuasiQuoter
    { quoteExp = parseRelationalExprQ
    , quotePat = notHandled "pattern"
    , quoteType = notHandled "type"
    , quoteDec = notHandled "declaration"
    }
    where
      parseRelationalExprQ tutdString =
        case parse (relExprP @()) "" (T.pack tutdString) of
          Left err -> fail (show err)
          Right parsed -> lift parsed
      

databaseContextExpr :: QuasiQuoter
databaseContextExpr =
    QuasiQuoter
    { quoteExp = \strIn ->
        case parse databaseContextExprP "" (T.pack strIn) of
          Left err -> fail (show err)
          Right parsed -> lift parsed
    , quotePat = notHandled "pattern"
    , quoteType = notHandled "type"
    , quoteDec = notHandled "declaration"
    }

transGraphRelationalExpr :: QuasiQuoter
transGraphRelationalExpr = 
    QuasiQuoter
    { quoteExp = \strIn ->
        case parse (relExprP @TransactionIdLookup) "" (T.pack strIn) of
          Left err -> fail (show err)
          Right parsed -> lift parsed
    , quotePat = notHandled "pattern"
    , quoteType = notHandled "type"
    , quoteDec = notHandled "declaration"
    }

deriving instance Lift a => Lift (RelationalExprBase a)
deriving instance Lift a => Lift (TupleExprsBase a)
deriving instance Lift a => Lift (TupleExprBase a)
deriving instance Lift a => Lift (AttributeExprBase a)
deriving instance Lift a => Lift (AttributeNamesBase a)
deriving instance Lift a => Lift (RestrictionPredicateExprBase a)
deriving instance Lift a => Lift (ExtendTupleExprBase a)
deriving instance Lift a => Lift (WithNameExprBase a)
deriving instance Lift a => Lift (AtomExprBase a)
deriving instance Lift RelationTupleSet
deriving instance Lift Attributes
deriving instance Lift Relation
deriving instance Lift TypeConstructor
deriving instance Lift Attribute
deriving instance Lift Atom
deriving instance Lift RelationTuple
deriving instance Lift AtomType

deriving instance (Lift a, Lift r) => Lift (DatabaseContextExprBase a r)
deriving instance Lift r => Lift (AlterDBCACLExprBase r)
deriving instance Lift TypeConstructorDef
deriving instance Lift DataConstructorDef
deriving instance Lift DataConstructorDefArg
deriving instance Lift SomePermission
deriving instance Lift RelVarPermission
deriving instance Lift DBCFunctionPermission
deriving instance Lift FunctionPermission
deriving instance Lift AlterSchemaPermission
deriving instance Lift AlterTransGraphPermission
deriving instance Lift ACLPermission
deriving instance Lift InclusionDependency

deriving instance Lift TransactionIdLookup
deriving instance Lift TransactionIdHeadBacktrack

notHandled :: String -> a
notHandled e = error $  e <> "not supported by TutorialD Quasiquoter"

class AtomReplacer a where
  replace :: Atom -> Atom -> a -> a

instance AtomReplacer Atom where
  replace needle replacement haystack =
    case haystack of
      h | h == needle -> replacement
      IntegerAtom{} -> haystack
      IntAtom{} -> haystack
      ScientificAtom{} -> haystack
      DoubleAtom{} -> haystack
      TextAtom{} -> haystack
      DayAtom{} -> haystack
      DateTimeAtom{} -> haystack
      ByteStringAtom{} -> haystack
      BoolAtom{} -> haystack
      UUIDAtom{} -> haystack
      RelationAtom rel -> RelationAtom (replace needle replacement rel)
      SubrelationFoldAtom rel attr -> SubrelationFoldAtom (replace needle replacement rel) attr
      ConstructedAtom dConsName aType args ->
        ConstructedAtom dConsName aType (fmap (replace needle replacement) args)

instance AtomReplacer (AtomExprBase a) where
  replace needle replacement haystack =
    case haystack of
      AttributeAtomExpr{} -> haystack
      SubrelationAttributeAtomExpr{} -> haystack
      NakedAtomExpr atom -> NakedAtomExpr (replace needle replacement atom)
      FunctionAtomExpr fname args marker ->
        FunctionAtomExpr fname (fmap (replace needle replacement) args) marker
      RelationAtomExpr relExpr ->
        RelationAtomExpr (replace needle replacement relExpr)
      IfThenAtomExpr if' then' else' ->
        IfThenAtomExpr (replace needle replacement if') (replace needle replacement then') (replace needle replacement else')
      ConstructedAtomExpr dConsName args marker ->
        ConstructedAtomExpr dConsName (fmap (replace needle replacement) args) marker

-- only allow a replacement if the type does *not* change
instance AtomReplacer Relation where
  replace needle replacement haystack =
    if atomTypeForAtom needle /= atomTypeForAtom replacement then
      error "atoms in Relation can only replaced with one of same type"
      else
      case relMap tupMapper haystack of
        Left err -> error ("replacement rejected: " <> show err)
        Right rel -> rel
    where
      tupMapper tup =
        pure $ replace needle replacement tup

instance AtomReplacer (RelationalExprBase a) where
  replace needle replacement haystack =
    case haystack of
      MakeRelationFromExprs attrExprs tupExprs ->
        MakeRelationFromExprs attrExprs (replace needle replacement tupExprs)
      MakeStaticRelation attrs tupSet ->
        MakeStaticRelation attrs (replace needle replacement tupSet)
      ExistingRelation rel ->
        ExistingRelation (replace needle replacement rel)
      RelationVariable{} -> haystack
      RelationValuedAttribute{} -> haystack
      Project attrNames relExpr ->
        Project attrNames (replace needle replacement relExpr)
      Union attrNames relExpr ->
        Union attrNames (replace needle replacement relExpr)
      Join relExprA relExprB ->
        Join (replace needle replacement relExprA) (replace needle replacement relExprB)
      Rename renames relExpr ->
        Rename renames (replace needle replacement relExpr)
      Difference relExprA relExprB ->
        Difference (replace needle replacement relExprA) (replace needle replacement relExprB)
      Group attrNames groupAttr relExpr ->
        Group attrNames groupAttr (replace needle replacement relExpr)
      Ungroup attrName relExpr ->
        Ungroup attrName (replace needle replacement relExpr)
      Restrict predExpr relExpr ->
        Restrict (replace needle replacement predExpr) (replace needle replacement relExpr)
      Equals relExprA relExprB ->
        Equals (replace needle replacement relExprA) (replace needle replacement relExprB)
      NotEquals relExprA relExprB ->
        NotEquals (replace needle replacement relExprA) (replace needle replacement relExprB)
      Extend extender relExpr ->
        Extend (replace needle replacement extender) (replace needle replacement relExpr)
      With withs relExpr ->
        With (replace needle replacement withs) (replace needle replacement relExpr)
      
instance AtomReplacer RelationTuple where
  replace needle replacement haystack =
    if atomTypeForAtom needle /= atomTypeForAtom replacement then
      error "atoms in Relation can only replaced with one of same type"
    else
      RelationTuple (tupleAttributes haystack) (V.map (replace needle replacement) (tupleAtoms haystack))

instance AtomReplacer (TupleExprsBase a) where
  replace needle replacement (TupleExprs marker tupExprs) =
    TupleExprs marker (fmap (replace needle replacement) tupExprs)

instance AtomReplacer RelationTupleSet where
  replace needle replacement (RelationTupleSet tups) =
    RelationTupleSet (fmap (replace needle replacement) tups)

instance AtomReplacer (RestrictionPredicateExprBase a) where
  replace needle replacement expr =
    case expr of
      TruePredicate -> expr
      AndPredicate a b ->
        AndPredicate (replace needle replacement a) (replace needle replacement b)
      OrPredicate a b ->
        OrPredicate (replace needle replacement a) (replace needle replacement b)
      NotPredicate a ->
        NotPredicate (replace needle replacement a)
      AtomExprPredicate expr ->
        AtomExprPredicate (replace needle replacement expr)
      AttributeEqualityPredicate attrName atomExpr ->
        AttributeEqualityPredicate attrName (replace needle replacement atomExpr)
      
instance AtomReplacer (ExtendTupleExprBase a) where
  replace needle replacement (AttributeExtendTupleExpr attrName expr) =
    AttributeExtendTupleExpr attrName (replace needle replacement expr)

instance AtomReplacer (WithNamesAssocsBase a) where
  replace needle replacement assocs =
    fmap (\(name, relExpr) -> (name, replace needle replacement relExpr)) assocs

instance AtomReplacer (TupleExprBase a) where
  replace needle replacement (TupleExpr tupMap) =
    TupleExpr (M.map (\atomExpr -> replace needle replacement atomExpr) tupMap)

instance AtomReplacer (DatabaseContextExprBase a r) where
  replace needle replacement expr =
    case expr of
      NoOperation -> expr
      Define{} -> expr
      Undefine{} -> expr
      Assign rv relExpr ->
        Assign rv (replace needle replacement relExpr)
      Insert rv relExpr ->
        Insert rv (replace needle replacement relExpr)
      Delete rv predExpr ->
        Delete rv (replace needle replacement predExpr)
      Update rv attrAtomMap predExpr ->
        Update rv (M.map (replace needle replacement) attrAtomMap) (replace needle replacement predExpr)
      AddInclusionDependency nam incDep ->
        AddInclusionDependency nam (replace needle replacement incDep)
      RemoveInclusionDependency{} -> expr
      AddNotification nam expr1 expr2 expr3 ->
        AddNotification nam (replace needle replacement expr1) (replace needle replacement expr2) (replace needle replacement expr3)
      RemoveNotification{} -> expr
      AddTypeConstructor{} -> expr
      RemoveTypeConstructor{} -> expr
      RemoveAtomFunction{} -> expr
      RemoveDatabaseContextFunction{} -> expr
      ExecuteDatabaseContextFunction fname args ->
        ExecuteDatabaseContextFunction fname (map (replace needle replacement) args)
      AddRegisteredQuery qName relExpr ->
        AddRegisteredQuery qName (replace needle replacement relExpr)
      RemoveRegisteredQuery{} -> expr
      AlterACL{} -> expr
      MultipleExpr exprs ->
        MultipleExpr (map (replace needle replacement) exprs)

instance AtomReplacer InclusionDependency where
  replace needle replacement (InclusionDependency exprA exprB) =
    InclusionDependency (replace needle replacement exprA) (replace needle replacement exprB)

replaceTextAtom :: AtomReplacer a => T.Text -> T.Text -> a -> a
replaceTextAtom needle replacement haystack =
  replace (TextAtom needle) (TextAtom replacement) haystack
    
replaceIntegerAtom :: AtomReplacer a => Integer -> Integer -> a -> a
replaceIntegerAtom needle replacement haystack =
  replace (IntegerAtom needle) (IntegerAtom replacement) haystack

replaceAtoms :: AtomReplacer a => [(Atom, Atom)] -> a -> a
replaceAtoms assocs haystack =
  foldr folder haystack assocs
  where
    folder (needle, replacement) acc =
      replace needle replacement acc
