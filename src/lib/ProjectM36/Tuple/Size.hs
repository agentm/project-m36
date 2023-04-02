-- used to provide fast estimates for the size of the tuple in caching contexts, therefore, no serialization tests should be performed here
module ProjectM36.Tuple.Size where
import ProjectM36.Base
import qualified Data.Vector as V
import GHC.Num
import Foreign.Storable
import GHC.Exts
import GHC
import Data.Text as T
import qualified Data.ByteString as BS
import Data.Time.Calendar
import Data.Time.Clock

type ByteCount = Int

class ByteSize a where
  byteSize :: a -> ByteCount

instance ByteSize RelationTuple where  
  byteSize (RelationTuple typ tup) = byteSize typ + V.sum (V.map byteSize tup)

instance ByteSize Attributes where
  byteSize (Attributes as) = V.sum (V.map byteSize as)

instance ByteSize Attribute where
  byteSize (Attribute nam typ) = byteSize nam + byteSize typ

instance ByteSize Integer where
  -- this seems to be an API lacking which would return the size of the underlying bytearray across multiple GHC version, so we'll just punt on this for now
  byteSize _ = 8

instance ByteSize Text where
  byteSize t = T.length t * 2 -- lengthWord8 t -- only since Text 2.0, so approximate

instance ByteSize Day where
  byteSize d = byteSize (toModifiedJulianDay d)

instance ByteSize UTCTime where
  byteSize d = byteSize (utctDay d) + byteSize (utctDayTime d)

instance ByteSize DiffTime where
  byteSize d = 8 -- Integer-backed
  
instance ByteSize Atom where
  byteSize x =
    case x of
      IntegerAtom i -> byteSize i
      IntAtom i -> sizeOf i
      DoubleAtom d -> sizeOf d
      TextAtom t -> byteSize t
      DayAtom d -> byteSize d
      DateTimeAtom d -> byteSize d
      ByteStringAtom bs -> BS.length bs
      BoolAtom b -> sizeOf b
      UUIDAtom u -> sizeOf u
      RelationAtom rel -> byteSize rel
      RelationalExprAtom re -> byteSize re
      ConstructedAtom dConsName typ atoms -> byteSize dConsName + byteSize typ + sum (fmap byteSize atoms)
      
instance ByteSize AtomType where
  byteSize t =
    case t of
      IntAtomType -> 8
      IntegerAtomType -> 8
      DoubleAtomType -> 8
      TextAtomType -> 8
      DayAtomType -> 8
      DateTimeAtomType -> 8
      ByteStringAtomType -> 8
      BoolAtomType -> 8
      UUIDAtomType -> 8
      RelationAtomType a -> byteSize a
      ConstructedAtomType tConsName tVarMap -> byteSize tConsName + byteSize tVarMap
      RelationalExprAtomType -> 8
      TypeVariableType tVarName -> byteSize tVarName
      
