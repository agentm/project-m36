{-# LANGUAGE MagicHash #-}
module ProjectM36.RelExprSize where
import ProjectM36.Base
import Data.Int
import GHC.Integer.GMP.Internals
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.ByteString as BS
import Data.Time.Calendar
import GHC.Exts
import qualified Data.Map as M

type ByteCount = Int64

-- | Provide reasonably accurate but fast estimates for size of the data.
class Size a where
  size :: a -> ByteCount

instance Size (RelationalExprBase a) where
  size expr =   
    case expr of
      MakeRelationFromExprs _ tupleExprs -> size tupleExprs
      MakeStaticRelation _ tupSet -> size tupSet
      ExistingRelation (Relation _ tupSet) -> size tupSet
      RelationVariable _ _ -> 0
      Project _ expr' -> size expr'
      Union exprA exprB -> size exprA + size exprB
      Join exprA exprB -> size exprA + size exprB
      Rename _ _ expr' -> size expr'
      Difference exprA exprB -> size exprA + size exprB
      Group _ _ expr' -> size expr'
      Ungroup _ expr' -> size expr'
      Restrict _ expr' -> size expr'
      Equals exprA exprB -> size exprA + size exprB
      NotEquals exprA exprB -> size exprA + size exprB
      Extend _ expr' -> size expr'
      With macros expr' -> size expr' + sum (fmap (size . snd) macros)

ptrSize :: Int -> Int64
ptrSize l = fromIntegral $ 8 * l

instance Size RelationTupleSet where
  size (RelationTupleSet tups) = sum (fmap size tups) + ptrSize (length tups)

instance Size RelationTuple where
  size (RelationTuple _ atoms) = sum (fmap size atoms) + ptrSize (length atoms)
                                     
instance Size (TupleExprsBase a) where
  size (TupleExprs _ exprs) = sum (fmap size exprs) + ptrSize (length exprs)

instance Size (TupleExprBase a) where
  size (TupleExpr tupMap) = sum (fmap size tupMap) + ptrSize (M.size tupMap) * 2

instance Size (AtomExprBase a) where
  size expr =
    case expr of
      AttributeAtomExpr{} -> 0
      NakedAtomExpr a -> size a
      FunctionAtomExpr _ exprs _ -> sum (fmap size exprs) + ptrSize (length exprs)
      RelationAtomExpr relexpr -> size relexpr
      ConstructedAtomExpr _ exprs _ -> sum (fmap size exprs) + ptrSize (length exprs)

instance Size Atom where
  size atom =
    case atom of
      IntegerAtom i -> size i
      IntAtom _ -> 8
      DoubleAtom _ -> 8
      TextAtom t -> fromIntegral $ T.length t * 2 --UTF-16 until text-2.0 is widespread
      DayAtom d -> size (toModifiedJulianDay d)
      DateTimeAtom (UTCTime d dt) -> size d + size dt
      ByteStringAtom bs -> fromIntegral $ BS.length bs
      BoolAtom _ -> 1
      UUIDAtom _ -> 16
      RelationAtom rel -> size rel
      RelationalExprAtom expr -> size expr
      ConstructedAtom _ _ atoms -> 8 + sum (fmap size atoms)

instance Size Relation where
  size (Relation _ tupSet) = size tupSet + ptrSize (length (asList tupSet))
  
instance Size Integer where
  size (S# _) = 8
  size (Jp# (BN# bytearray)) = fromIntegral (I# (sizeofByteArray# bytearray))
  size (Jn# (BN# bytearray)) = fromIntegral (I# (sizeofByteArray# bytearray))

instance Size Day where
  size d = size $ toModifiedJulianDay d

instance Size DiffTime where
  size d = size $ diffTimeToPicoseconds d
