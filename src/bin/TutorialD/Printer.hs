{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module TutorialD.Printer where
import ProjectM36.Base
import ProjectM36.Attribute hiding (null)
import Data.Text.Prettyprint.Doc 
import qualified Data.Set as S hiding (fromList)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE

instance Pretty Atom where
  pretty (IntegerAtom x) = pretty x
  pretty (IntAtom x) = "int" <> parensList [pretty x]
  pretty (DoubleAtom x) = pretty x
  pretty (TextAtom x) = dquotes (pretty x)
  pretty (DayAtom x) = "fromGregorian" <> parensList [pretty a, pretty b, pretty c]
    where
      (a,b,c) = toGregorian x
  pretty (DateTimeAtom time) = "dateTimeFromEpochSeconds" <> parensList [pretty @Integer (round (utcTimeToPOSIXSeconds time))]
  pretty (ByteStringAtom bs) = "bytestring" <> parensList [dquotes (pretty (TE.decodeUtf8 (B64.encode bs)))]
  pretty (BoolAtom x) = if x then "t" else "f"
  pretty (RelationAtom x) = pretty x
  pretty (RelationalExprAtom re) = pretty re
  pretty (ConstructedAtom n _ as) = pretty n <+> prettyList as

instance Pretty AtomExpr where
  pretty (AttributeAtomExpr attrName) = pretty attrName
  pretty (NakedAtomExpr atom)         = pretty atom
  pretty (FunctionAtomExpr atomFuncName' atomExprs _) = pretty atomFuncName' <> prettyAtomExprsAsArguments atomExprs
  pretty (RelationAtomExpr relExpr) = pretty relExpr
  pretty (ConstructedAtomExpr dName [] _) = pretty dName
  pretty (ConstructedAtomExpr dName atomExprs _) = pretty dName <+> hsep (map prettyAtomExpr atomExprs)
                                           
prettyAtomExpr :: AtomExpr -> Doc ann
prettyAtomExpr atomExpr =
  case atomExpr of
    AttributeAtomExpr attrName -> "@" <> pretty attrName
    ConstructedAtomExpr dConsName [] () -> pretty dConsName
    ConstructedAtomExpr dConsName atomExprs () -> parens (pretty dConsName <+> hsep (map prettyAtomExpr atomExprs))
    _ -> pretty atomExpr
    
prettyAtomExprsAsArguments :: [AtomExpr] -> Doc ann
prettyAtomExprsAsArguments = align . parensList . map addAt
  where addAt (atomExpr :: AtomExpr) =
          case atomExpr of
            AttributeAtomExpr attrName -> "@" <> pretty attrName
            _ -> pretty atomExpr

instance Pretty TupleExpr where
  pretty (TupleExpr map') = "tuple" <> bracesList (Prelude.map (\(attrName,atom)-> pretty attrName <+> pretty atom) (M.toList map'))

instance Pretty RelationTuple where
  pretty (RelationTuple attrs atoms) = "tuple" <> bracesList (zipWith (\x y-> pretty x <+> pretty y) (V.toList (attributeNames attrs)) (V.toList atoms))

instance Pretty Relation where
  pretty (Relation attrs tupSet) | attrs == V.empty && null (asList tupSet) = "false"
  pretty (Relation attrs tupSet) | attrs == V.empty && asList tupSet == [RelationTuple V.empty V.empty] = "true"
  pretty (Relation attrs tupSet) = "relation" <> prettyBracesList (V.toList attrs) <> prettyBracesList (asList tupSet)

instance Pretty Attribute where
  pretty (Attribute n aTy) = pretty n <+> pretty (show aTy)  -- workaround

instance Pretty RelationalExpr where
  pretty (RelationVariable n _) = pretty n
  pretty (ExistingRelation r) = pretty r
  pretty (NotEquals a b) = pretty' a <+> "!=" <+> pretty' b
  pretty (Equals a b) = pretty' a <+> "==" <+> pretty' b
  pretty (Project ns r) = pretty' (ignoreProjects r) <> pretty ns
  pretty (Extend ext r) = collectExtends r <> pretty ext <> "}"
  pretty (MakeRelationFromExprs Nothing (TupleExprs () tupExprs)) = "relation" <> prettyBracesList tupExprs
  pretty (MakeRelationFromExprs (Just attrExprs) (TupleExprs () tupExprs)) = "relation" <> prettyBracesList attrExprs <> prettyBracesList tupExprs
  pretty (MakeStaticRelation attrs tupSet) = "relation" <> prettyBracesList (V.toList attrs) <> prettyBracesList (asList tupSet)
  pretty (Union a b) = parens $ pretty' a <+> "union" <+> pretty' b
  pretty (Join a b) = parens $ pretty' a <+> "join" <+> pretty' b
  pretty (Rename n1 n2 relExpr) = parens $ pretty relExpr <+> "rename" <+> braces (pretty n1 <+> "as" <+> pretty n2)
  pretty (Difference a b) = parens $ pretty' a <+> "minus" <+> pretty' b
  pretty (Group attrNames attrName relExpr) = parens $ pretty relExpr <+> "group" <+> parens (pretty attrNames <+> "as" <+> pretty attrName)
  pretty (Ungroup attrName relExpr) = parens $ pretty' relExpr <+> "ungroup" <+> pretty attrName
  pretty (Restrict resPreExpr relExpr) = parens $ pretty' relExpr <+> "where" <+> pretty resPreExpr 
  pretty (With pairs a) = "with" <+> parensList (map (\(name,expr)-> pretty name <+> "as" <+> pretty expr) pairs) <+> pretty' a

-- relvar:{a:=?}:{b:=?}:... in ADTs => relvar:{a:=?, b:=?, ...} in Doc ann
collectExtends :: RelationalExpr -> Doc ann
collectExtends (Extend ext r) = collectExtends r <> pretty ext <> ", "
collectExtends r = pretty r <> ":{" 

--relvar{a,b,c,...}{a,b,...}..{a} => relvar{a}
ignoreProjects :: RelationalExpr -> RelationalExpr
ignoreProjects (Project _ r) = ignoreProjects r
ignoreProjects r = r

prettyRelationalExpr :: RelationalExpr -> Doc n
prettyRelationalExpr (RelationVariable n _) = pretty n
prettyRelationalExpr r = parens (pretty r)

pretty' :: RelationalExpr -> Doc n
pretty' = prettyRelationalExpr

instance Pretty AttributeNames where
  pretty (AttributeNames attrNames) = prettyBracesList (S.toList attrNames)
  pretty (InvertedAttributeNames attrNames) = braces $ "all but" <+> concatWith (surround ", ") (map pretty (S.toList attrNames))
  pretty (RelationalExprAttributeNames relExpr) = braces $ "all from" <+> pretty relExpr
  pretty a = error $ show a ++ " is not implemented yet."
--  YuMing L.: I guess these two are used in evaluating relational expressions. So we won't be using them in pretty printer.
--  pretty (UnionAttributeNames aAttrNames bAttrNames) = prettyBracesList (S.toList (aAttrNames `union` bAttrNames))
--  pretty (IntersectAttributeNames aAttrNames bAttrNames) = prettyBracesList (S.toList (aAttrNames `intersection` bAttrNames))

instance Pretty AttributeExpr where
  pretty (NakedAttributeExpr attr) = pretty attr
  pretty (AttributeAndTypeNameExpr name typeCons _) = pretty name <+> pretty typeCons

instance Pretty TypeConstructor where
  pretty (ADTypeConstructor tcName []) = pretty tcName
  pretty (ADTypeConstructor tcName tConsArgs) = pretty tcName <+> hsep (map pretty tConsArgs)
  pretty (PrimitiveTypeConstructor tcName atomType') = pretty tcName <+> pretty atomType'
  pretty (RelationAtomTypeConstructor attrExprs) = "relation" <> prettyBracesList attrExprs
  pretty (TypeVariable x) = pretty x

instance Pretty AtomType where
  pretty IntAtomType = "Int"
  pretty IntegerAtomType = "Integer"
  pretty DoubleAtomType = "Double"
  pretty TextAtomType = "Text"
  pretty DayAtomType = "Day"
  pretty DateTimeAtomType = "DateTime"
  pretty ByteStringAtomType = "ByteString"
  pretty BoolAtomType = "Bool"
  pretty (RelationAtomType attrs) = "relation " <+> prettyBracesList (V.toList attrs)
  pretty (ConstructedAtomType tcName tvMap) = pretty tcName <+> hsep (map pretty (M.toList tvMap)) --order matters
  pretty RelationalExprAtomType = "RelationalExpr"
  pretty (TypeVariableType x) = pretty x
    
instance Pretty ExtendTupleExpr where
  pretty (AttributeExtendTupleExpr attrName atomExpr) = pretty attrName <> ":=" <> pretty atomExpr


instance Pretty RestrictionPredicateExpr where
  pretty TruePredicate = "true"
  pretty (AndPredicate a b) = pretty a <+> "and" <+> pretty b 
  pretty (OrPredicate a b) = pretty a <+> "or" <+> pretty b 
  pretty (NotPredicate a) = "not" <+> pretty a 
  pretty (RelationalExprPredicate relExpr) = pretty relExpr
  pretty (AtomExprPredicate atomExpr) = pretty atomExpr
  pretty (AttributeEqualityPredicate attrName atomExpr) = pretty attrName <> "=" <> pretty atomExpr

instance Pretty WithNameExpr where
  pretty (WithNameExpr name _) = pretty name

bracesList :: [Doc ann] -> Doc ann
bracesList = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "

prettyBracesList :: Pretty a => [a] -> Doc ann
prettyBracesList = align . bracesList . map pretty

parensList :: [Doc ann] -> Doc ann
parensList = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") ", "

prettyParensList :: Pretty a => [a] -> Doc ann
prettyParensList = align . parensList . map pretty

