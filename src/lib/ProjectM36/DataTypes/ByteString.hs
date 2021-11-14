module ProjectM36.DataTypes.ByteString where
import ProjectM36.Base
import ProjectM36.AtomFunctionError
import ProjectM36.AtomFunctionBody
import qualified Data.HashSet as HS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE

bytestringAtomFunctions :: AtomFunctions
bytestringAtomFunctions = HS.fromList [
  Function { funcName = "bytestring",
             funcType = [TextAtomType, ByteStringAtomType],
             funcBody = compiledAtomFunctionBody $ \(TextAtom textIn:_) -> case B64.decode (TE.encodeUtf8 textIn) of
                   Left err -> Left (AtomFunctionBytesDecodingError err)
                   Right bs -> pure (ByteStringAtom bs) 
               }
  ]
       
