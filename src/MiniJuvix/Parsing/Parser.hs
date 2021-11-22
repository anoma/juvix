-- | Adapted from https://github.com/heliaxdev/juvix/
module MiniJuvix.Parsing.Parser where

--------------------------------------------------------------------------------

import MiniJuvix.Parsing.Language
import MiniJuvix.Utils.Parser
import qualified MiniJuvix.Utils.Parser as P
import MiniJuvix.Utils.Prelude
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P
import qualified Data.Set as Set
import qualified Data.ByteString as ByteString
import qualified MiniJuvix.Utils.Prelude as Encoding

--------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Reserved words/symbols
------------------------------------------------------------------------------

kwImport
  , kwIn
  , kwInfix
  , kwInfixL
  , kwInfixR
  , kwLet
  , kwModule
  , kwOpen
  , kwDataType
  , kwRecordType
  , kwRecordTypeConstructor
  , kwPretype 
  , kwPreterm
  , kwUniverse
  , kwTypeSignature
  , kwWhere :: ByteString

kwImport = "import"
kwIn = "in"
kwInfix = "infix"
kwInfixL = "infixl"
kwInfixR = "infixr"
kwLet = "let"
kwModule = "mod"
kwOpen = "open"
kwDataType = "data"
kwRecordType = "record"
kwRecordTypeConstructor = "constructor"
kwPretype = "Pretype"
kwPreterm = "Preterm"
kwTypeSignature = "sig"
kwUniverse = "U#"
kwWhere = "where"

reservedWords :: Set ByteString 
reservedWords =
  Set.fromList
    [ kwImport,
      kwIn,
      kwInfix,
      kwInfix,
      kwInfix,
      kwLet,
      kwModule,
      kwOpen,
      kwDataType,
      kwRecordType,
      kwRecordTypeConstructor,
      kwTypeSignature,
      kwWhere
    ]

symLambdaUnicode,
  symLambdaBody,
  symTyping,
  symDefine,
  symGuard,
  symComment,
  symTypeConstraint :: ByteString

symLambdaUnicode = "λ"
symLambdaBody = "->"
symTyping = ":"
symDefine = "="
symGuard = "|"
symComment = "--"
symTypeConstraint = "=>"

reservedSymbols :: Set ByteString
reservedSymbols =
  Set.fromList
    [ symTyping,
      symDefine,
      symGuard,
      symComment,
      symTypeConstraint
    ]

--------------------------------------------------------------------------------
-- Comment related methods
--------------------------------------------------------------------------------

removeComments :: ByteString -> ByteString
removeComments = ByteString.concat . grabComments
  where
    onBreakDo _break _con "" = []
    onBreakDo _break cont str = _break str |> cont
    grabComments = breakComment `onBreakDo` f
      where
        f (notIn, in') = notIn : grabComments (dropNewLine in')
    dropNewLine = ByteString.dropWhile (/= P.newLine)

breakComment :: ByteString -> (ByteString, ByteString)
breakComment = ByteString.breakSubstring symComment

--------------------------------------------------------------------------------
-- File Header
-- e.g. "mod Main where"
--------------------------------------------------------------------------------

fileHeaderCase :: Parser (FileHeader TopLevel)
fileHeaderCase = do
  reserved kwModule
  name <- prefixSymbolDotSN
  reserved kwWhere
  FileHeader name <$> P.some topLevelSN

noFileHeaderCase :: Parser (FileHeader TopLevel)
noFileHeaderCase = NoFileHeader <$> P.some topLevelSN

fileHeader :: Parser (FileHeader TopLevel)
fileHeader = P.try fileHeaderCase <|> noFileHeaderCase

----------------------------------------------------------------------------
-- Top Level
------------------------------------------------------------------------------

topLevelSN :: Parser TopLevel
topLevelSN = spaceLiner topLevel

topLevel :: Parser TopLevel
topLevel =
  P.try (FixityDeclaration <$> fixity)
    <|> P.try (TypeSignatureDeclaration <$> typeSignature)
    <|> P.try (DataTypeDeclaration <$> dataTypeConstructor)
    <|> P.try (RecordTypeDeclaration <$> recordTypeConstructor)
    <|> P.try (ModuleDeclaration <$> moduleD)
    <|> P.try (OpenModuleDeclaration <$> openModuleD)
    <|> P.try (FunctionDeclaration <$> function)

--------------------------------------------------------------------------------
-- Fixity declarations
-- e.g. "infixr _*_ 100"
--------------------------------------------------------------------------------

fixityDeclaration :: Parser FixityMode
fixityDeclaration = undefined 
  -- do
  -- _ <- P.string kwInfix
  -- fixityMode <-
  --   P.try
  --     (LeftAssociative <$ P.string "l")
  --     <|> P.try (RightAssociative <$ P.string "r")
  --     <|> pure NonAssociative
  -- symbolEnd
  -- name <- prefixSymbolSN
  -- fixityMode name . fromInteger <$> spaceLiner integer

fixity :: Parser Fixity
fixity = Fixity <$> fixityDeclaration

--------------------------------------------------------------------------------
-- Type signature
-- e.g. "sig x ω : ctx => T"
-- where "ctx" has the form "(e [,e ])" where e is a valid expr.
-- If ω is greater than one, it just Many.
--------------------------------------------------------------------------------

typeSignatureContext :: Parser [Expression]
typeSignatureContext = undefined 
  -- P.try (pure <$> expression <* reserved symTypeConstraint)
  --   <|> P.try
  --     ( P.parens
  --         ( P.sepBy
  --             expression
  --             (skipLiner P.comma)
  --         )
  --         <* reserved symTypeConstraint
  --     )
  --   <|> pure []

typeSignatureContextSN :: Parser [Expression]
typeSignatureContextSN = spaceLiner typeSignatureContext

typeSignature :: Parser TypeSignature
typeSignature = undefined 
  -- do
  -- reserved kwTypeSignature
  -- name <- prefixSymbolSN
  -- maybeQuantity <- P.optional (spaceLiner integer)
  -- P.skipLiner P.colon
  -- ctx <- typeSignatureContextSN
  -- TypeSignature name maybeQuantity ctx <$> expression
  

--------------------------------------------------------------------------------
-- Data type constructor
-- e.g. "type T where" or "type T v1 where One : (p : v1) -> T p"
--------------------------------------------------------------------------------

parseDataConstructors :: Parser [ DataConstructor ]
parseDataConstructors = undefined

dataTypeConstructorSN :: Parser DataType
dataTypeConstructorSN = spaceLiner dataTypeConstructor

dataTypeConstructor :: Parser DataType
dataTypeConstructor = undefined 
    -- do
  -- reserved kwDataType
  -- typeName <- prefixSymbolSN
  -- typeParams <- P.many prefixSymbolSN
  -- reserved kwWhere
  -- DataType typeName typeParams <$> parseDataConstructors

--------------------------------------------------------------------------------
-- Record type constructor
-- e.g. "record Cat Δ where
--         constructor cat
--         a : (v : Δ) → T₁ v
--------------------------------------------------------------------------------

parseRecordFields :: Parser RecordField
parseRecordFields = undefined

recordTypeConstructor :: Parser RecordType
recordTypeConstructor = undefined
    -- do
  --  reserved kwRecordType
  --  typeName <- prefixSymbolSN
  --  typeParams <- P.many prefixSymbolSN
  --  reserved kwWhere
  --  reserved kwRecordTypeConstructor
  --  RecordType typeName recordTypeConstructorName typeParams parseRecordFields
   
--------------------------------------------------------------------------------
-- Module/Open Module
-- e.g. "module A where"
--------------------------------------------------------------------------------

moduleD :: Parser Module
moduleD =  undefined 
    -- do
  -- reserved kwModule
  -- name <- prefixSymbolDotSN
  -- reserved kwWhere
  -- Module name <$> P.some topLevelSN

openModuleD :: Parser OpenModule
openModuleD = undefined

--------------------------------------------------------------------------------
-- Universe
--------------------------------------------------------------------------------

universeSymbol :: Parser Expression
universeSymbol = undefined 
  -- do
  -- _ <- P.string kwUniverse
  -- level <- skipLiner integer
  -- pure (UniverseExpr . Universe . fromInteger $ level)

universeExpression :: Parser Universe
universeExpression = undefined
  --  P.try (Universe <$> prefixSymbolSN)
  --     <|> Universe <$> P.parens prefixSymbolSN

--------------------------------------------------------------------------------
-- Let
--------------------------------------------------------------------------------

letBlock :: Parser LetBlock
letBlock = undefined 

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

function :: Parser Function
function = undefined 

--------------------------------------------------------------------------------
-- Lambda
--------------------------------------------------------------------------------

lambda :: Parser Lambda
lambda = undefined 
  -- skipLiner P.backSlash
  -- args <- P.many1H patternSN
  -- reserved symLambdaBody
  -- Lamb args <$> expression

--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------

application :: Parser Application
application = undefined 

--------------------------------------------------------------------------------
-- Pre- types and terms
--------------------------------------------------------------------------------

-- primitives :: Parser Primitive
-- primitives = do
--   _ <- P.single P.percent
--   Prim <$> prefixSymbolDot

-- list :: Parser List
-- list = ListLit <$> P.brackets (P.sepBy expression (skipLiner P.comma))

-- tupleParen :: Parser Expression
-- tupleParen = do
--   p <- P.parens (P.sepBy1 (expressionGen all'') (skipLiner P.comma))
--   case p of
--     [] -> fail "doesn't happen"
--     [x] -> pure (Parened x)
--     _ : _ -> pure (Tuple (TupleLit p))

--------------------------------------------------------------------------------
-- SN Derivatives
--------------------------------------------------------------------------------

expressionSN :: Parser Expression
expressionSN = undefined 

--------------------------------------------------------------------------------

-- Unwrap the header from the rest of the definitions
extractTopLevel :: FileHeader topLevel -> [topLevel]
extractTopLevel (FileHeader _ decls) = decls
extractTopLevel (NoFileHeader decls) = decls

--------------------------------------------------------------------------------
-- Symbol Handlers
--------------------------------------------------------------------------------

prefixCapitalDotSN :: Parser (NonEmpty Symbol)
prefixCapitalDotSN = spaceLiner prefixCapitalDot

prefixSymbolDotSN :: Parser (NonEmpty Symbol)
prefixSymbolDotSN = spaceLiner prefixSymbolDot

prefixSymbolSN :: Parser Symbol
prefixSymbolSN = spaceLiner prefixSymbol

infixSymbolGen :: Parser Symbol -> Parser Symbol
infixSymbolGen p = undefined
  -- do
  -- symb <- p
  -- if Set.member symb reservedSymbols
  --   then fail "symbol is reserved word"
  --   else pure symb

infixSymbolDot :: Parser (NonEmpty Symbol)
infixSymbolDot = undefined
  -- do
  -- qualified <-
  --   P.option
  --     []
  --     (toList <$> prefixSymbolDotPermissive <* P.char dot)
  -- -- -o is a bit special since it's a normal letter
  -- -- this is a bit of a hack
  -- infix' <- P.try ("-o" <$ P.string "-o") <|> P.try infixSymbol
  -- pure (fromList (qualified <> [infix']))

infixSymbol :: Parser Symbol
infixSymbol = infixSymbolGen (P.try infixSymbol' <|> infixPrefix)

infixSymbol' :: Parser Symbol
infixSymbol' = undefined 
  -- internText . Encoding.decodeUtf8
  --   <$> P.takeWhile1P
  --     (Just "Valid Infix Symbol")
  --     validInfixSymbol

infixPrefix :: Parser Symbol
infixPrefix =
  P.single backtick *> prefixSymbol <* P.single backtick

prefixSymbolGen :: Parser Word8 -> Parser Symbol
prefixSymbolGen startParser = undefined 
  -- do
  -- start <- startParser
  -- rest <- P.takeWhileP (Just "Valid Middle Symbol") validMiddleSymbol
  -- -- Slow O(n) call, could maybe peek ahead instead, then parse it all at once?
  -- let new = ByteString.cons start rest
  -- if Set.member new reservedWords
  --   then fail "symbol is reserved operator"
  --   else pure (internText $ Encoding.decodeUtf8 new)

-- TODO: this may be bad
-- this allows "(*).Foo.(<*>)" to be accepted
-- Though Should we allow this since, these are prefix if spelled this way
-- we don't enforce capitalization, and thus it would be improper for to
-- special case it out!

prefixSepGen :: Parser Symbol -> Parser (NonEmpty Symbol)
prefixSepGen parser = do
  ret <- sepBy1H parser (P.char dot)
  v <- P.optional $ P.try P.eof
  case v of
    Nothing -> do
      d <- P.optional $ P.lookAhead (P.char dot)
      case d of
        Nothing -> pure ret
        Just _ -> fail "symbol not prefix"
    Just _ -> pure ret

-- the permissive functions allow the functions to not fully parse the word
-- useful for infix application
prefixSymbolDotPermissive :: Parser (NonEmpty Symbol)
prefixSymbolDotPermissive = sepBy1H prefixSymbol (P.char dot)

prefixSymbolDot :: Parser (NonEmpty Symbol)
prefixSymbolDot = prefixSepGen prefixSymbol

prefixCapitalDot :: Parser (NonEmpty Symbol)
prefixCapitalDot = undefined 
  -- prefixSepGen prefixCapital

prefixSymbol :: Parser Symbol
prefixSymbol = undefined 
  -- P.try (prefixSymbolGen (P.satisfy validStartSymbol))
  --   <|> parend

-- parend :: Parser Symbol
-- parend =
--   skipLiner openParen
--     *> spaceLiner (infixSymbolGen infixSymbol') <* P.char closeParen

-- prefixCapital :: Parser Symbol
-- prefixCapital = prefixSymbolGen (P.satisfy validUpperSymbol)

------------------------------------------------------------------------------
-- Top Level Runner
------------------------------------------------------------------------------

-- prettyParse :: ByteString -> Either [Char] (Header TopLevel)
-- prettyParse = left P.errorBundlePretty . parse

-- parse :: ByteString -> Either ParserError (Header TopLevel)
-- parse = P.parse (P.eatSpaces (header <* P.eof)) "" . removeComments

-- -- | Parse multiple files into ML AST
-- parseFiles :: [FilePath] -> IO (Either Error [(NameSymbol.T, [TopLevel])])
-- parseFiles =
--   -- fmap gets through the IO, so that sequenceA flips the either and list
--   fmap sequenceA . traverse parseSingleFile

-- parseFile :: FilePath -> IO (Either Error (NameSymbol.T, [TopLevel]))
-- parseFile = parseSingleFile

-- -- | Parse single file into ML AST
-- parseSingleFile :: FilePath -> IO (Either Error (NameSymbol.T, [TopLevel]))
-- parseSingleFile file = do
--   read <- ByteString.readFile file
--   pure $ case Parser.parse read of
--     Left x ->
--       Left (ParseError x)
--     Right (NoHeader _xs) ->
--       Left (NoHeaderErr file)
--     Right (Header name xs) ->
--       Right (name, xs)

-- _fileNameToModuleName :: FilePath -> NameSymbol.T
-- _fileNameToModuleName =
--   NameSymbol.fromSymbol . intern . toUpperFirst . FilePath.takeBaseName