module ProjectM36.Relation.Parse.CSV where
--parse Relations from CSV
import ProjectM36.Base
import ProjectM36.Error
import ProjectM36.Relation
import ProjectM36.AtomType
import ProjectM36.DataTypes.Interval
import qualified ProjectM36.Attribute as A

import Data.Csv.Parser
import qualified Data.Vector as V
import Data.Char (ord, isUpper, isSpace)
import qualified Data.ByteString.Lazy as BS
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Set as S
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString.Lazy as APBL
import qualified Data.Attoparsec.Text as APT
import Control.Arrow
import Text.Read hiding (parens)
import Control.Applicative
import Data.Either
import Control.Monad (void)

data CsvImportError = CsvParseError String |
                      AttributeMappingError RelationalError |
                      HeaderAttributeMismatchError (S.Set AttributeName)
                    deriving (Show)

csvDecodeOptions :: DecodeOptions
csvDecodeOptions = DecodeOptions {decDelimiter = fromIntegral (ord ',')}

csvAsRelation :: Attributes -> TypeConstructorMapping -> BS.ByteString -> Either CsvImportError Relation
csvAsRelation attrs tConsMap inString = case APBL.parse (csvWithHeader csvDecodeOptions) inString of
  APBL.Fail _ _ err -> Left (CsvParseError err)
  APBL.Done _ (headerRaw,vecMapsRaw) -> do
    let strHeader = V.map decodeUtf8 headerRaw
        strMapRecords = V.map convertMap vecMapsRaw
        convertMap hmap = HM.fromList $ L.map (decodeUtf8 *** (T.unpack . decodeUtf8)) (HM.toList hmap)
        attrNameSet = A.attributeNameSet attrs
        headerSet = S.fromList (V.toList strHeader)
        parseAtom attrName aType textIn = case APT.parseOnly (parseCSVAtomP attrName tConsMap aType <* APT.endOfInput) textIn of
          Left err -> Left (ParseError (T.pack err))
          Right eAtom -> eAtom 
        makeTupleList :: HM.HashMap AttributeName String -> [Either CsvImportError Atom]
        makeTupleList tupMap = V.toList $ V.map (\attr -> 
                                                  either (Left . AttributeMappingError) Right $ 
                                                  parseAtom (A.attributeName attr) (A.atomType attr) (T.pack $ tupMap HM.! A.attributeName attr)) (attributesVec attrs)
    if attrNameSet == headerSet then do
      tupleList <- mapM sequence $ V.toList (V.map makeTupleList strMapRecords)
      case mkRelationFromList attrs tupleList of
        Left err -> Left (AttributeMappingError err)
        Right rel -> Right rel
      else
      Left $ HeaderAttributeMismatchError (S.difference attrNameSet headerSet)


parseCSVAtomP :: AttributeName -> TypeConstructorMapping -> AtomType -> APT.Parser (Either RelationalError Atom)
parseCSVAtomP _ _ IntegerAtomType = Right . IntegerAtom <$> APT.decimal
parseCSVAtomP _ _ IntAtomType = Right . IntAtom <$> APT.decimal
parseCSVAtomP _ _ DoubleAtomType = Right . DoubleAtom <$> APT.double
parseCSVAtomP _ _ TextAtomType = 
  Right . TextAtom <$> (quotedString <|> takeToEndOfData)
parseCSVAtomP _ _ DayAtomType = do
  dString <- T.unpack <$> takeToEndOfData
  case readMaybe dString of
    Nothing -> fail ("invalid Day string: " ++ dString)
    Just date -> pure (Right (DayAtom date))
parseCSVAtomP _ _ DateTimeAtomType = do    
  dString <- T.unpack <$> takeToEndOfData
  case readMaybe dString of
    Nothing -> fail ("invalid Date string: " ++ dString)
    Just date -> pure (Right (DateTimeAtom date))
parseCSVAtomP _ _ ByteStringAtomType = do    
  bsString <- T.unpack <$> takeToEndOfData
  case readMaybe bsString of
    Nothing -> fail ("invalid ByteString string: " ++ bsString)
    Just bs -> pure (Right (ByteStringAtom bs))
parseCSVAtomP _ _ BoolAtomType = do    
  bString <- T.unpack <$> takeToEndOfData
  case readMaybe bString of
    Nothing -> fail ("invalid BoolAtom string: " ++ bString)
    Just b -> pure (Right (BoolAtom b))
parseCSVAtomP _ _ RelationalExprAtomType = do
  reString <- T.unpack <$> takeToEndOfData      
  case readMaybe reString of
    Nothing -> fail ("invalid RelationalExprAtom string: " ++ reString)
    Just b -> pure (Right (RelationalExprAtom b))
parseCSVAtomP attrName tConsMap typ@(ConstructedAtomType _ tvmap) 
  | isIntervalAtomType typ = do
    begin <- (APT.char '[' >> pure False) <|> (APT.char '(' >> pure True)
    let iType = intervalSubType typ
    eBeginv <- parseCSVAtomP attrName tConsMap iType
    case eBeginv of
      Left err -> pure (Left err)
      Right beginv -> do
        _ <- APT.char ','
        eEndv <- parseCSVAtomP attrName tConsMap iType
        case eEndv of
          Left err -> pure (Left err)
          Right endv -> do
            end <- (APT.char ']' >> pure False) <|> 
                   (APT.char ')' >> pure True)
            pure (Right (ConstructedAtom "Interval" typ [beginv, endv, 
                                                         BoolAtom begin, BoolAtom end]))
  | otherwise = do
    dConsName <- capitalizedIdentifier
    APT.skipSpace
  --we need to look up the name right away in order to determine the types of the following arguments
  -- grab the data constructor
    case findDataConstructor dConsName tConsMap of
      Nothing -> pure (Left (NoSuchDataConstructorError dConsName))
      Just (_, dConsDef) -> 
      -- identify the data constructor's expected atom type args
        case resolvedAtomTypesForDataConstructorDefArgs tConsMap tvmap dConsDef of
          Left err -> pure (Left err)
          Right argAtomTypes -> do
            atomArgs <- mapM (\argTyp -> let parseNextAtom = parseCSVAtomP attrName tConsMap argTyp <* APT.skipSpace in
                               case argTyp of
                                 ConstructedAtomType _ _ -> 
                                   parens parseNextAtom <|>
                                   parseNextAtom
                                 _ -> parseNextAtom
                             ) argAtomTypes
            case lefts atomArgs of
              [] -> pure (Right (ConstructedAtom dConsName typ (rights atomArgs)))
              errs -> pure (Left (someErrors errs))
parseCSVAtomP attrName _ (RelationAtomType _) = pure (Left (RelationValuedAttributesNotSupportedError [attrName]))
parseCSVAtomP _ _ (TypeVariableType x) = pure (Left (TypeConstructorTypeVarMissing x))
      
capitalizedIdentifier :: APT.Parser T.Text
capitalizedIdentifier = do
  fletter <- APT.satisfy isUpper APT.<?> "capitalized data constructor letter"
  rest <- APT.takeWhile (\c -> not (isSpace c || c == ')')) APT.<?> "data constructor name"
  pure (fletter `T.cons` rest)
  
--read data for Text.Read parser but be wary of end of interval blocks  
takeToEndOfData :: APT.Parser T.Text
takeToEndOfData = APT.takeWhile (APT.notInClass ",)]")
  
parens :: APT.Parser a -> APT.Parser a  
parens p = do
  void $ APT.char '('
  APT.skipSpace
  v <- p
  APT.skipSpace
  void $ APT.char ')'
  pure v
  
quotedString :: APT.Parser T.Text
quotedString = do
  let escapeMap = [('"','"'), ('n', '\n'), ('r', '\r')]
      doubleQuote = void $ APT.char '"'
  doubleQuote      
  (_, s) <- APT.runScanner [] (\prevl nextChar -> case prevl of
                             [] -> Just [nextChar]
                             chars | last chars == '\\' ->
                                        case lookup nextChar escapeMap of 
                                          Nothing -> Just (chars ++ [nextChar]) --there is no escape sequence, so leave backslash + char
                                          Just escapeVal -> Just (init chars ++ [escapeVal]) -- nuke the backslash and add the escapeVal
                                   | nextChar == '"' -> Nothing
                                   | otherwise -> Just (chars ++ [nextChar]))
  doubleQuote
  pure (T.pack s)
  
