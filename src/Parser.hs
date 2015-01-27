{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Parser where

import Data.Foldable (asum)
import Data.Functor
import Data.Char (digitToInt)
import Data.List (foldl', find)
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.String (fromString)
import Data.Text.Lazy (Text, unpack)
-- import Text.ParserCombinators.Parsec hiding (Line, Parser)
import Text.Parsec hiding (Line, Parser)

-- type Parser = GenParser Char ConvertMap
type Parser = Parsec Text ConvertMap

data ExportType
    = ModuleExport
    | NameExport Text
    deriving (Eq, Show)

-- Output data structure
data Line
    = PlainLine Text
    | ImportLine {
      jsExp :: JSExpr,
      hsName :: Text,
      cConstraints :: [ClassConstraint],
      hsType :: Type
    }
    | ExportLine {
      exType :: ExportType,
      hsName :: Text,
      hsType :: Type
    }
    deriving (Eq,Show)

-- Type distinguishes between those types, that are important to us
data Type = IOVoid
          | IOType Type
          | PlainType Text
          | ConvertType ConvertData -- a type that must be converted
          | FunctionType Type Type
          deriving (Eq,Show)

-- class constraint in the type signature
data ClassConstraint = ClassConstraint {
     className  :: Text,
     parameters :: [Text]
  } deriving (Eq,Show)

data ConvertData = ConvertData {
    typeName        :: Text,
    foreignTypeName :: Text,
    toConvert       :: Text,
    fromConvert     :: Text
    }
    deriving(Eq,Show)
type ConvertMap = [ConvertData]

type JSExpr = [JSExprPart]
data JSExprPart = StringPart Text | ArgumentPart Int | RestArgPart deriving (Eq,Show)

parseFFIFile :: Parser [Line]
parseFFIFile = endBy line eol

line :: Parser Line
line = importLine <|> exportLine <|> plainLine

plainLine :: Parser Line
plainLine = PlainLine . fromString <$> many (noneOf "\n")

whiteSpaces :: Parser Text
whiteSpaces = fromString <$> many (char ' ' <|> char '\t' <?> "Whitespace")

whiteSpaces1 :: Parser Text
whiteSpaces1 = fromString <$> many1 (char ' ' <|> char '\t' <?> "Whitespace")

quoted :: Parser Text
-- quoted = let qt = char '"' in between qt qt (fromString <$> manyTill anyChar qt)
quoted = let qt = char '"' in qt >> fromString <$> manyTill anyChar qt

exportType :: Parser ExportType
exportType = (ModuleExport <$ string "module")
         <|> (NameExport <$> quoted)
         <?> "module or export name"

exportLine :: Parser Line
exportLine = do
    try $ do
        string "foreign"
        whiteSpaces1
        string "export"
        whiteSpaces1
        string "commonjs"
    whiteSpaces1
    exTy <- exportType
    whiteSpaces1
    hsName <- fromString <$> many1 (alphaNum <|> char '_')
    whiteSpaces
    string "::"
    whiteSpaces
    signature <- typeSignature
    return $ ExportLine exTy hsName signature


importLine :: Parser Line
importLine = do
  try $ do
    string "foreign"
    whiteSpaces1
    string "import"
    whiteSpaces1
    string "commonjs"
  whiteSpaces1
  char '"'
  jsName <- jsExpr
  char '"'
  whiteSpaces
  hsName <- fromString <$> many1 (alphaNum <|> char '_')
  whiteSpaces
  string "::"
  whiteSpaces
  (constraints, newCm) <- try classConstraints <|> (([],) <$> getState)
  whiteSpaces
  cm <- getState
  setState newCm
  signature <- typeSignature
  setState cm
  return $ ImportLine jsName hsName constraints signature

jsExpr :: Parser JSExpr
jsExpr = many1 jsExprPart

jsExprPart :: Parser JSExprPart
jsExprPart = try jsExprArgPart <|> try jsExprRestArgPart <|> jsExprStringPart

positiveNatural :: Parser Int
positiveNatural =
    foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

jsExprArgPart :: Parser JSExprPart
jsExprArgPart = do
  char '%'
  n <- positiveNatural
  return $ ArgumentPart n

jsExprRestArgPart :: Parser JSExprPart
jsExprRestArgPart = string "%*" >> return RestArgPart

jsExprStringPart :: Parser JSExprPart
jsExprStringPart = StringPart . fromString <$> many1 (noneOf "\"%")

typeSignature :: Parser Type
typeSignature = try functionType <|> try oneArgumentType

oneArgumentType :: Parser Type
oneArgumentType =
  try (do char '('
          res <- typeSignature
          char ')'
          return res)
  <|> try convertType
  <|> try ioVoidType
  <|> try ioType
  <|> try plainType
  <?> "Some haskell type"

convertType :: Parser Type
convertType = do
  whiteSpaces
  cm <- getState
  -- test if any of the types match (they are in fst cm)
  r <- asum $ map
      (\c -> do
          string $ unpack $ typeName c
          return $ ConvertType c
      )
      cm
  whiteSpaces
  return r

ioHeader :: Parser ()
ioHeader = do
  whiteSpaces
  string "IO"
  whiteSpaces
  return ()

ioVoidType :: Parser Type
ioVoidType = do
  ioHeader
  string "()"
  return IOVoid

ioType :: Parser Type
ioType = do
  ioHeader
  r <- oneArgumentType
  whiteSpaces
  return $ IOType r

plainType :: Parser Type
plainType = do
  whiteSpaces
  parts <- many1 plainTypePart
  whiteSpaces
  return $ PlainType $ mconcat parts

plainTypePart :: Parser Text
plainTypePart =
    try $ do char '('
             parts <- many plainTypePart
             char ')'
             return $ "(" <> mconcat parts <> ")"
    <|> (fromString <$> many1 (alphaNum <|> char ' '))

functionType :: Parser Type
functionType = do
  whiteSpaces
  t1 <- oneArgumentType
  whiteSpaces
  string "->"
  whiteSpaces
  t2 <- typeSignature
  whiteSpaces
  return $ FunctionType t1 t2

classConstraints :: Parser ([ClassConstraint], ConvertMap)
classConstraints = do
  cc <- try $ do c <- singleClassConstraint
                 return [c]
        <|> manyClassConstraints
  whiteSpaces
  string "=>"
  cm <- getState
  -- find the class names in the convert map, and create appropriae new convert entries
  let singleParamConstraints = filter (\(ClassConstraint _ parameters) -> length parameters == 1) cc
      constrAppliesToConvertData constr convDat = typeName convDat == className constr
      makeConvDataWithClassConstr constr convData = convData {typeName = (head . parameters) constr}
      newCM = mapMaybe
          (\constr ->
              makeConvDataWithClassConstr constr <$>
              find (constrAppliesToConvertData constr) cm
          )
          singleParamConstraints
  return (cc, cm ++ newCM)

singleClassConstraint :: Parser ClassConstraint
singleClassConstraint = do
  whiteSpaces
  first <- upper
  rest  <- many alphaNum
  let className = fromString $ first:rest
  whiteSpaces1
  names <- many1 nameInClassConstraint
  return $ ClassConstraint className names

nameInClassConstraint :: Parser Text
nameInClassConstraint = do
  first <- lower
  rest  <- many alphaNum
  whiteSpaces
  return $ fromString (first:rest)

manyClassConstraints :: Parser [ClassConstraint]
manyClassConstraints = do
  char '('
  whiteSpaces
  res <- sepBy1 singleClassConstraint (char ',')
  char ')'
  return res



eol = char '\n' <?> "end of line"
