-- {-# LANGUAGE ApplicativeDo #-}

-- | Adapted from https://github.com/heliaxdev/juvix/
module MiniJuvix.Parsing.Parser
  (
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Parsing.Language
import qualified MiniJuvix.Utils.Parser as P
import MiniJuvix.Utils.Prelude
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P

--------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Reserved words/symbols
------------------------------------------------------------------------------

kwBegin
  , kwCase
  , kwDeclare
  , kwEnd
  , kwIf
  , kwImport
  , kwIn
  , kwInfix
  , kwInfixL
  , kwInfixR
  , kwLet
  , kwModule
  , kwOpen
  , kwThen
  , kwDataTypeConstructor
  , kwRecordTypeConstructor
  , kwTypeSignature
  , kwWhere :: ByteString

kwBegin = "begin"
kwCase = "case"
kwDeclare = "declare"
kwEnd = "end"
kwIf = "if"
kwImport = "import"
kwIn = "in"
kwInfix = "infix"
kwInfixL = "infixl"
kwInfixR = "infixr"
kwLet = "let"
kwModule = "mod"
kwOpen = "open"
kwThen = "then"
kwDataTypeConstructor = "data"
kwRecordTypeConstructor = "record"
kwTypeSignature = "sig"
kwWhere = "where"

reservedWords :: (Ord a, IsString a) => Set a
reservedWords =
  Set.fromList
    [ kwBegin,
      kwCase,
      kwDeclare,
      kwEnd,
      kwIf,
      kwImport,
      kwIn,
      kwInfix,
      kwInfix,
      kwInfix,
      kwLet,
      kwModule,
      kwOpen,
      kwThen,
      kwDataTypeConstructor,
      kwRecordTypeConstructor,
      kwTypeSignature,
      kwWhere
    ]

symTyping,
  symDefine,
  symGuard,
  symComment,
  symConstraint :: ByteString

symTyping = ":"
symDefine = "="
symGuard = "|"
symComment = "--"
symTypeConstraint = "=>"

reservedSymbols :: (Ord a, IsString a) => Set a
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
    onBreakDo break cont str = break str |> cont
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

fileHeaderCase :: Parser (Header TopLevel)
fileHeaderCase = do
  reserved kwModule
  name <- prefixSymbolDotSN
  reserved kwWhere
  FileHeader name <$> P.some topLevelSN

noFileHeaderCase :: Parser (Header TopLevel)
noFileHeaderCase = NoFileHeader <$> P.some topLevelSN

fileHeader :: Parser (Header TopLevel)
fileHeader = P.try fileHeaderCase <|> noFileHeaderCase

--------------------------------------------------------------------------------
-- Fixity declarations
-- e.g. "infixr _*_ 100"
--------------------------------------------------------------------------------

fixityDeclaration :: Parser InfixDeclaration
fixityDeclaration = do
  _ <- P.string kwInfix
  fixityMode <-
    P.try
      (LeftAssociative <$ P.string "l")
      <|> P.try (RightAssociative <$ P.string "r")
      <|> pure NonAssociative
  symbolEnd
  name <- prefixSymbolSN
  fixityMode name . fromInteger <$> spaceLiner integer

fixity :: Parser Declaration
fixity = Fixity <$> fixityDeclaration

--------------------------------------------------------------------------------
-- Type signature
-- e.g. "sig x ω : ctx => T"
-- where "ctx" has the form "(e [,e ])" where e is a valid expr.
-- If ω is greater than one, it just Many.
--------------------------------------------------------------------------------

typeSignatureContext :: Parser [Expression]
typeSignatureContext =
  P.try (pure <$> expression <* reserved symTypeConstraint)
    <|> P.try
      ( P.parens
          ( P.sepBy
              expression
              (skipLiner P.comma)
          )
          <* reserved symTypeConstraint
      )
    <|> pure []

typeSignatureContextSN :: Parser [Expression]
typeSignatureContextSN = spaceLiner typeSignatureContext

typeSignature :: Parser TypeSignature
typeSignature = do
  reserved kwTypeSignature
  name <- prefixSymbolSN
  maybeQuantity <- P.optional (spaceLiner integer)
  skipLiner P.colon
  ctx <- typeSignatureContextSN
  Signature name maybeQuantity ctx <$> expression

--------------------------------------------------------------------------------
-- Data type constructor
-- e.g. "type T where" or "type T v1 where One : (p : v1) -> T p"
--------------------------------------------------------------------------------

parseDataConstructors :: Parser [ DataConstructor ]
parseDataConstructors = undefined
-- parseDataConstructors = do
  -- arrow <- P.optional (skipLiner P.colon *> expression)
--   P.skipLiner P.equals
--   adt <- adt
--   case arrow of
--     Just arr -> pure (Types.Arrowed arr adt)
--     Nothing -> pure (Types.NonArrowed adt)

dataTypeConstructor :: Parser DataTypeConstructor
dataTypeConstructor = do
  reserved kwDataTypeConstructor
  typeName <- prefixSymbolSN
  typeParams <- P.many prefixSymbolSN
  reserved kwWhere
  DataType typeName typeParams <$> parseDataConstructors

-- record :: Parser Types.Record
-- record = do
--   names <-
--     spaceLiner
--       $ P.curly
--       $ P.sepBy1HFinal nameTypeSN (skipLiner P.comma)
--   familySignature <- P.optional (skipLiner P.colon *> expression)
--   pure (Types.Record'' names familySignature)

-- nameType :: Parser Types.NameType
-- nameType = do
--   name <- nameParserSN
--   maybeQuantity <-
--     P.optional (fmap Types.Constant constantSN <|> spaceLiner (P.parens expressionSN))
--   skipLiner P.colon
--   sig <- expression
--   pure (Types.NameType' sig name maybeQuantity)

-- -- nameParserColon :: Parser Types.Name
-- -- nameParserColon =
-- --   nameParserSN <* skip (== P.colon)

-- nameParser :: Parser Types.Name
-- nameParser =
--   P.try (P.skipSome (P.char P.hash) *> fmap Types.Implicit prefixSymbol)
--     <|> Types.Concrete <$> prefixSymbol

----------------------------------------------------------------------------
-- Top Level
------------------------------------------------------------------------------

-- expressionGen' ::
--   Parser Types.Expression -> Parser Types.Expression
-- expressionGen' p =
--   P.try (Types.Cond <$> cond)
--     <|> P.try (Types.Let <$> let')
--     <|> P.try (Types.LetType <$> letType)
--     <|> P.try (Types.ModuleE <$> mod)
--     <|> P.try (Types.Match <$> match)
--     <|> P.try (Types.OpenExpr <$> moduleOpenExpr)
--     <|> P.try (Types.Block <$> block)
--     <|> P.try (Types.Lambda <$> lam)
--     <|> P.try (Types.Primitive <$> primitives)
--     <|> P.try (Types.DeclarationE <$> declarationExpression)
--     <|> P.try p
--     <|> expressionArguments

-- expressionArguments :: Parser Types.Expression
-- expressionArguments =
--   P.try (Types.Block <$> block)
--     <|> P.try (Types.ExpRecord <$> expRecord)
--     <|> P.try (Types.RecordDec <$> record)
--     <|> P.try (Types.Constant <$> constant)
--     -- <|> try (Types.NamedTypeE <$> namedRefine)
--     <|> P.try universeSymbol
--     <|> P.try (Types.Name <$> prefixSymbolDot)
--     <|> P.try (Types.List <$> list)
--     -- We wrap this in a paren to avoid conflict
--     -- with infixity that we don't know about at this phase!
--     <|> tupleParen

-- do''' :: Parser Types.Expression
-- do''' = Types.Do <$> do'

-- app'' :: Parser Types.Expression
-- app'' = Types.Application <$> P.try application

-- all'' :: Parser Types.Expression
-- all'' = P.try do''' <|> app'' <|> (Types.EffApp <$> P.try via_)

-- expressionGen :: Parser Types.Expression -> Parser Types.Expression
-- expressionGen p =
--   Expr.makeExprParser (spaceLiner (expressionGen' p)) tableExp

-- -- used to remove do from parsing
-- expression' :: Parser Types.Expression
-- expression' = expressionGen app''

-- -- used to remove both from parsing
-- expression''' :: Parser Types.Expression
-- expression''' = expressionGen (fail "")

-- expression :: Parser Types.Expression
-- expression = expressionGen all''

-- usage :: Parser Types.Expression
-- usage = P.string "u#" *> expression

topLevel :: Parser Types.TopLevel
topLevel =
        P.try (TypeSignature <$> signature)
    <|> P.try (FixityDeclaration <$> declaration)
    <|> P.try (TypeConstructorDeclaration <$> dataTypeConstructor)
    <|> P.try fun
    <|> P.try modT
    <|> P.try (Types.ModuleOpen <$> moduleOpen)

-- --------------------------------------------------------------------------------
-- -- Modules/ Function Gen
-- --------------------------------------------------------------------------------

-- functionModStartReserved ::
--   ByteString -> (Symbol -> [Types.Arg] -> Parser a) -> Parser a
-- functionModStartReserved str f = do
--   reserved str
--   name <- prefixSymbolSN
--   args <- P.many argSN
--   f name args

-- functionModGen :: Parser a -> Parser (Types.FunctionLike a)
-- functionModGen p =
--   functionModStartReserved
--     "let"
--     ( \name args -> do
--         guard <- guard p
--         pure (Types.Like name args guard)
--     )

-- --------------------------------------------------------------------------------
-- -- Guard
-- --------------------------------------------------------------------------------

-- guard :: Parser a -> Parser (Types.GuardBody a)
-- guard p =
--   P.try (Types.Guard <$> condB p)
--     <|> P.try (Types.Body <$> (P.skipLiner P.equals *> p))

-- --------------------------------------------------------------------------------
-- -- Args
-- --------------------------------------------------------------------------------

-- arg :: Parser Types.Arg
-- arg =
--   P.try (Types.ImplicitA <$> (P.char P.hash *> matchLogic))
--     <|> (Types.ConcreteA <$> matchLogic)

-- --------------------------------------------------------------------------------
-- -- Match
-- --------------------------------------------------------------------------------

-- match :: Parser Types.Match
-- match = do
--   reserved "case"
--   matchOn <- expressionSN
--   reserved "of"
--   matchs <- P.many1H (P.try matchLSN)
--   pure (Types.Match'' matchOn matchs)

-- matchL :: Parser Types.MatchL
-- matchL = do
--   skipLiner P.pipe
--   match <- P.try matchLogicSN
--   spaceLiner (P.string "->")
--   Types.MatchL match <$> P.try expression

-- matchLogic :: Parser Types.MatchLogic
-- matchLogic = P.maybeParend (P.try matchLogicNamedSN <|> matchLogicNotNamedSN)

-- matchLogicNamed :: Parser Types.MatchLogic
-- matchLogicNamed = do
--   name <- prefixSymbol
--   skipLiner P.at
--   start <- P.maybeParend matchLogicStartSN
--   pure (Types.MatchLogic start (Just name))

-- matchLogicNotNamed :: Parser Types.MatchLogic
-- matchLogicNotNamed = do
--   start <- matchLogicStart
--   pure (Types.MatchLogic start Nothing)

-- matchLogicStart :: Parser Types.MatchLogicStart
-- matchLogicStart =
--   P.try matchRecord
--     <|> P.try matchCon
--     <|> P.try matchName
--     <|> matchConstant

-- matchConstant :: Parser Types.MatchLogicStart
-- matchConstant = Types.MatchConst <$> constant

-- matchCon :: Parser Types.MatchLogicStart
-- matchCon = do
--   con <- prefixCapitalDotSN
--   matchd <- P.many matchLogicSN
--   pure (Types.MatchCon con matchd)

-- matchName :: Parser Types.MatchLogicStart
-- matchName = Types.MatchName <$> prefixSymbol

-- matchRecord :: Parser Types.MatchLogicStart
-- matchRecord =
--   Types.MatchRecord <$> nameSetMany matchLogic

-- --------------------------------------------------------------------------------
-- -- NameSet
-- --------------------------------------------------------------------------------

-- nameSetMany' :: Parser a -> Parser (NonEmpty (Types.NameSet a))
-- nameSetMany' parser =
--   P.curly $ do
--     x <- P.sepBy1H (nameSetSN parser) (skipLiner P.comma)
--     if  | length x == 1 && isPunned x ->
--           x <$ skipLiner P.comma
--         | otherwise ->
--           x <$ P.optional (skipLiner P.comma)

-- isPunned :: NonEmpty (Types.NameSet t) -> Bool
-- isPunned (Types.Punned {} :| _) = True
-- isPunned (Types.NonPunned {} :| _) = False

-- nameSetMany :: Parser a -> Parser (NonEmpty (Types.NameSet a))
-- nameSetMany parser =
--   P.curly (P.sepBy1HFinal (nameSetSN parser) (skipLiner P.comma))

-- nameSet :: Parser a -> Parser (Types.NameSet a)
-- nameSet parser = P.try (nameMatch parser) <|> namePunned

-- namePunned :: Parser (Types.NameSet a)
-- namePunned = Types.Punned <$> prefixSymbolDot

-- nameMatch :: Parser a -> Parser (Types.NameSet a)
-- nameMatch parser = do
--   name <- prefixSymbolDotSN
--   P.skipLiner P.equals
--   Types.NonPunned name <$> parser

-- --------------------------------------------------------------------------------
-- -- Modules and Functions
-- --------------------------------------------------------------------------------

-- genGuard :: Symbol -> [Types.Arg] -> Parser a -> Parser (Types.FunctionLike a)
-- genGuard n a p =
--   guard p >>| Types.Like n a

-- fun :: Parser Types.TopLevel
-- fun = functionModStartReserved "let" func
--   where
--     func n a =
--       genGuard n a expression
--         >>| Types.Function . Types.Func

-- modT :: Parser Types.TopLevel
-- modT = functionModStartReserved "mod" mod
--   where
--     mod n a =
--       ( genGuard n a (P.many1H topLevelSN)
--           >>| Types.Module . Types.Mod
--       )
--         <* reserved "end"

-- moduleOpen :: Parser Types.ModuleOpen
-- moduleOpen = do
--   reserved "open"
--   Types.Open <$> moduleName

-- moduleName :: Parser Types.ModuleName
-- moduleName =
--   prefixSymbolDot

-- moduleOpenExpr :: Parser Types.ModuleOpenExpr
-- moduleOpenExpr = P.try moduleOpenExprNormal <|> moduleOpenExprParens

-- moduleOpenExprParens :: Parser Types.ModuleOpenExpr
-- moduleOpenExprParens = do
--   -- we want it to fail at the ., since it's a paren after it
--   -- Int.()
--   name <- prefixSymbolDotPermissive
--   P.char P.dot
--   expr <- P.parens expression
--   pure (Types.OpenExpress name expr)

-- moduleOpenExprNormal :: Parser Types.ModuleOpenExpr
-- moduleOpenExprNormal = do
--   reserved "open"
--   name <- moduleNameSN
--   kwIn
--   Types.OpenExpress name <$> expression

-

-- --------------------------------------------------------------------------------
-- -- Effect handler parser
-- --------------------------------------------------------------------------------

-- handlerParser :: Parser Types.Handler
-- handlerParser = do
--   reserved "handler"
--   name <- prefixSymbolSN
--   P.skipLiner P.equals
--   ops <- P.many (P.spaceLiner opParser)
--   reserved "end"
--   pure $ Types.Hand name ops

-- opParser :: Parser Types.Operation
-- opParser =
--   Types.Op
--     <$> functionModStartReserved
--       "let"
--       ( \name args -> do
--           guard <- guard expression
--           pure (Types.Like name args guard)
--       )

-- effParser :: Parser Types.Effect
-- effParser = do
--   reserved "effect"
--   name <- prefixSymbolSN
--   P.skipLiner P.equals
--   ops <- P.many (P.spaceLiner opSig)
--   reserved "end"
--   pure $ Types.Eff {effName = name, effOps = ops}

-- opSig :: Parser TypeSignature
-- opSig = do
--   reserved "let"
--   name <- prefixSymbolSN
--   maybeQuantity <-
--     P.optional
--       ( fmap Types.Constant constantSN
--           <|> spaceLiner (P.parens expressionSN)
--       )
--   skipLiner P.colon
--   typeclasses <- signatureContextSN
--   exp <- expression
--   pure (Types.Sig name maybeQuantity exp typeclasses)

-- via_ :: Parser Types.EffApp
-- via_ = do
--   args <- spaceLiner expressionArguments
--   reserved "via"
--   name <- spaceLiner (expressionGen' (fail ""))
--   pure (Types.Via name args)

-- --------------------------------------------------------------------------------
-- -- Arrow Type parser
-- --------------------------------------------------------------------------------

-- -- namedRefine :: Parser Types.NamedType
-- -- namedRefine =
-- --   Types.NamedType <$> nameParserColonSN <*> expression

-- --------------------------------------------------------------------------------
-- -- TypeNameParser and typeRefine Parser
-- --------------------------------------------------------------------------------

-- universeSymbol :: Parser Types.Expression
-- universeSymbol = do
--   _ <- P.string "u#"
--   Types.UniverseName <$> universeExpression

-- universeExpression :: Parser Types.UniverseExpression
-- universeExpression =
--   P.try (Types.UniverseExpression <$> prefixSymbolSN)
--     -- TODO: make this proper do + and max!
--     <|> Types.UniverseExpression <$> P.parens prefixSymbolSN

-- --------------------------------------------------------------------------------
-- -- Expressions
-- --------------------------------------------------------------------------------

-- block :: Parser Types.Block
-- block = do
--   reserved "begin"
--   exp <- expressionSN
--   reserved "end"
--   pure (Types.Bloc exp)

-- --------------------------------------------------------------------------------
-- -- Records
-- --------------------------------------------------------------------------------

-- expRecord :: Parser Types.ExpRecord
-- expRecord = Types.ExpressionRecord <$> nameSetMany' expression

-- --------------------------------------------------------------------------------
-- -- Let
-- --------------------------------------------------------------------------------

-- mod :: Parser Types.ModuleE
-- mod = do
--   kwModule
--   name <- prefixSymbolSN
--   args <- P.many argSN
--   guarded <- guard (P.many1H topLevelSN)
--   reserved "end"
--   kwIn
--   Types.ModE (Types.Like name args guarded) <$> expression

-- let' :: Parser Types.Let
-- let' = do
--   binds <- functionModGen expression
--   kwIn
--   Types.Let' binds <$> expression

-- letType :: Parser Types.LetType
-- letType = do
--   reserved "let"
--   typ <- dataTypeConstructorSN
--   kwIn
--   Types.LetType'' typ <$> expression

-- --------------------------------------------------------------------------------
-- -- Cond
-- --------------------------------------------------------------------------------

-- cond :: Parser (Types.Cond Types.Expression)
-- cond = do
--   reserved "if"
--   condB expression

-- condB :: Parser a -> Parser (Types.Cond a)
-- condB p = Types.C <$> P.many1H (P.try $ condLogicSN p)

-- condLogic :: Parser a -> Parser (Types.CondLogic a)
-- condLogic p = do
--   P.skipLiner P.pipe
--   pred <- expressionSN
--   P.skipLiner P.equals
--   Types.CondExpression pred <$> p

-- --------------------------------------------------------------------------------
-- -- Lambda
-- --------------------------------------------------------------------------------

-- lam :: Parser Types.Lambda
-- lam = do
--   skipLiner P.backSlash
--   args <- P.many1H matchLogicSN
--   reserved "->"
--   Types.Lamb args <$> expression

-- --------------------------------------------------------------------------------
-- -- Application
-- --------------------------------------------------------------------------------

-- application :: Parser Types.Application
-- application = do
--   name <- spaceLiner (expressionGen' (fail ""))
--   args <- P.many1H (spaceLiner expressionArguments)
--   pure (Types.App name args)

-- --------------------------------------------------------------------------------
-- -- Literals
-- --------------------------------------------------------------------------------

-- primitives :: Parser Types.Primitive
-- primitives = do
--   _ <- P.single P.percent
--   Types.Prim <$> prefixSymbolDot

-- list :: Parser Types.List
-- list = Types.ListLit <$> P.brackets (P.sepBy expression (skipLiner P.comma))

-- tupleParen :: Parser Types.Expression
-- tupleParen = do
--   p <- P.parens (P.sepBy1 (expressionGen all'') (skipLiner P.comma))
--   case p of
--     [] -> fail "doesn't happen"
--     [x] -> pure (Types.Parened x)
--     _ : _ -> pure (Types.Tuple (Types.TupleLit p))

-- constant :: Parser Types.Constant
-- constant =
--   P.try (Types.Number <$> number)
--     <|> Types.String <$> string'

-- number :: Parser Types.Numb
-- number =
--   P.try (Types.Integer' <$> P.integer)
--     <|> Types.Double' <$> float

-- digits :: Parser ByteString
-- digits = P.takeWhile1P (Just "digits") isDigit

-- float :: Parser Double
-- float = do
--   _s1 <- digits
--   void (P.char P.dot)
--   _s2 <- digits
--   fail "float not implemented"

-- stringEscape :: Parser ByteString
-- stringEscape =
--   P.between
--     (P.string "\\'")
--     (P.string "\\'")
--     (P.takeWhile1P (Just "Not quote") (/= P.quote))

-- doubleStringEscape :: Parser ByteString
-- doubleStringEscape =
--   P.between
--     (P.string "\\\"")
--     (P.string "\\\"")
--     (P.takeWhile1P (Just "Not quote") (/= P.doubleQuote))

-- stringWithoutEscape :: Parser ByteString
-- stringWithoutEscape =
--   P.between P.quote (P.takeWhile1P (Just "Not quote") (/= P.quote)) P.quote

-- doubleStringWithoutEscape :: Parser ByteString
-- doubleStringWithoutEscape =
--   P.between
--     P.doubleQuote
--     ( P.takeWhile1P
--         (Just "Not quote")
--         (/= P.doubleQuote)
--     )
--     P.doubleQuote

-- string' :: Parser Types.String'
-- string' = do
--   words <-
--     P.try stringEscape
--       <|> P.try doubleStringEscape
--       <|> P.try stringWithoutEscape
--       <|> doubleStringWithoutEscape
--   pure (Types.Sho $ Encoding.decodeUtf8 words)

-- --------------------------------------------------------------------------------
-- -- Do
-- --------------------------------------------------------------------------------

-- do' :: Parser Types.Do
-- do' = do
--   doExp <- do''
--   case length doExp of
--     0 -> fail "parser failed: do expression with zero values"
--     1 -> fail "parser failed: do expression with one value"
--     _ -> pure (Types.Do'' $ NonEmpty.fromList doExp)

-- doBind :: Parser [Types.DoBody]
-- doBind = do
--   name <- prefixSymbolSN
--   spaceLiner (P.string "<-")
--   body <- compParse
--   pure [Types.DoBody (Just name) body]

-- doNotBind :: Parser [Types.DoBody]
-- doNotBind = do
--   body <- compParse
--   pure [Types.DoBody Nothing body]

-- do'' :: Parser [Types.DoBody]
-- do'' = Expr.makeExprParser (P.try doBind <|> doNotBind) table P.<?> "bind expr"

-- compParse :: Parser Types.Computation
-- compParse = doPureParser <|> doOpParser

-- doOpParser :: Parser Types.Computation
-- doOpParser = do
--   name <- spaceLiner (expressionGen' (fail ""))
--   args <- P.many1H (spaceLiner expressionArguments)
--   pure (Types.DoOp $ Types.DoOp' name args)

-- doPureParser :: Parser Types.Computation
-- doPureParser = do
--   reserved "pure"
--   arg <- spaceLiner expressionArguments
--   pure (Types.DoPure $ Types.DoPure' arg)

-- --------------------------------------------------------------------------------
-- -- Expr Parser
-- --------------------------------------------------------------------------------

-- -- For Expressions
-- tableExp :: [[Expr.Operator Parser Types.Expression]]
-- tableExp =
--   [ [refine],
--     [infixOp],
--     [arrowExp]
--   ]

-- infixOp :: Expr.Operator Parser Types.Expression
-- infixOp =
--   Expr.InfixR
--     ( P.try $ do
--         inf <- spaceLiner infixSymbolDot
--         pure
--           (\l r -> Types.Infix (Types.Inf l inf r))
--     )

-- arrowExp :: Expr.Operator Parser Types.Expression
-- arrowExp =
--   Expr.InfixR
--     ( P.try $ do
--         skipLiner P.dash
--         exp <- expressionSN
--         reserved "->"
--         pure
--           (\l r -> Types.ArrowE (Types.Arr' l exp r))
--     )

-- refine :: Expr.Operator Parser Types.Expression
-- refine =
--   Expr.Postfix
--     $ P.try
--     $ do
--       refine <- spaceLiner (P.curly expressionSN)
--       pure (\p -> Types.RefinedE (Types.TypeRefine p refine))

-- -- For Do!
-- table :: Semigroup a => [[Expr.Operator Parser a]]
-- table = [[binary ";" (<>)]]

-- binary :: ByteString -> (a -> a -> a) -> Expr.Operator Parser a
-- binary name fun = Expr.InfixL (fun <$ spaceLiner (P.string name))

-- --------------------------------------------------------------------------------
-- -- SN Derivatives
-- --------------------------------------------------------------------------------

-- topLevelSN :: Parser Types.TopLevel
-- topLevelSN = spaceLiner topLevel

-- expression'SN :: Parser Types.Expression
-- expression'SN = spaceLiner expression'

-- expression'''SN :: Parser Types.Expression
-- expression'''SN = spaceLiner expression'''

-- -- TODO: Add Docs
-- expressionSN :: Parser Types.Expression
-- expressionSN = spaceLiner expression

-- signatureContextSN :: Parser [Expression]
-- signatureContextSN = spaceLiner signatureContext

-- argSN :: Parser Types.Arg
-- argSN = spaceLiner arg

-- matchLSN :: Parser Types.MatchL
-- matchLSN = spaceLiner matchL

-- matchLogicSN :: Parser Types.MatchLogic
-- matchLogicSN = spaceLiner matchLogic

-- matchLogicStartSN :: Parser Types.MatchLogicStart
-- matchLogicStartSN = spaceLiner matchLogicStart

-- matchLogicNamedSN :: Parser Types.MatchLogic
-- matchLogicNamedSN = spaceLiner matchLogicNamed

-- matchLogicNotNamedSN :: Parser Types.MatchLogic
-- matchLogicNotNamedSN = spaceLiner matchLogicNotNamed

-- nameSetSN :: Parser a -> Parser (Types.NameSet a)
-- nameSetSN = spaceLiner . nameSet

-- moduleNameSN :: Parser Types.ModuleName
-- moduleNameSN = spaceLiner moduleName

-- condLogicSN :: Parser a -> Parser (Types.CondLogic a)
-- condLogicSN = spaceLiner . condLogic

-- dataTypeConstructorSN :: Parser Types.Type
-- dataTypeConstructorSN = spaceLiner dataTypeConstructor

-- nameTypeSN :: Parser Types.NameType
-- nameTypeSN = spaceLiner nameType

-- -- nameParserColonSN :: Parser Types.Name
-- -- nameParserColonSN = spaceLiner nameParserColon

-- nameParserSN :: Parser Types.Name
-- nameParserSN = spaceLiner nameParser

-- -- namedRefineSN :: Parser Types.NamedType
-- -- namedRefineSN = spaceLiner namedRefine

-- sumSN :: Parser Types.Sum
-- sumSN = spaceLiner sum

-- prefixCapitalDotSN :: Parser (NonEmpty Symbol)
-- prefixCapitalDotSN = spaceLiner prefixCapitalDot

-- prefixSymbolDotSN :: Parser (NonEmpty Symbol)
-- prefixSymbolDotSN = spaceLiner prefixSymbolDot

-- prefixSymbolSN :: Parser Symbol
-- prefixSymbolSN = spaceLiner prefixSymbol

-- constantSN :: Parser Types.Constant
-- constantSN = spaceLiner constant

-- --------------------------------------------------------------------------------

-- -- Unwrap the header from the rest of the definitions
-- extractTopLevel :: Header topLevel -> [topLevel]
-- extractTopLevel (Header _ tops) = tops
-- extractTopLevel (NoHeader tops) = tops

-- -- we abuse laziness here
-- -- TODO ∷ add directory option
-- -- this will add top level to the thing, and properly handle paths

--------------------------------------------------------------------------------
-- Symbol Handlers
--------------------------------------------------------------------------------

-- prefixCapitalDotSN :: Parser (NonEmpty Symbol)
-- prefixCapitalDotSN = spaceLiner prefixCapitalDot

-- prefixSymbolDotSN :: Parser (NonEmpty Symbol)
-- prefixSymbolDotSN = spaceLiner prefixSymbolDot

-- prefixSymbolSN :: Parser Symbol
-- prefixSymbolSN = spaceLiner prefixSymbol

-- infixSymbolGen :: Parser Symbol -> Parser Symbol
-- infixSymbolGen p = do
--   symb <- p
--   if Set.member symb reservedSymbols
--     then fail "symbol is reserved word"
--     else pure symb

-- infixSymbolDot :: Parser (NonEmpty Symbol)
-- infixSymbolDot = do
--   qualified <-
--     P.option
--       []
--       (toList <$> prefixSymbolDotPermissive <* P.char dot)
--   -- -o is a bit special since it's a normal letter
--   -- this is a bit of a hack
--   infix' <- P.try ("-o" <$ P.string "-o") <|> P.try infixSymbol
--   pure (fromList (qualified <> [infix']))

-- infixSymbol :: Parser Symbol
-- infixSymbol = infixSymbolGen (P.try infixSymbol' <|> infixPrefix)

-- infixSymbol' :: Parser Symbol
-- infixSymbol' =
--   internText . Encoding.decodeUtf8
--     <$> P.takeWhile1P
--       (Just "Valid Infix Symbol")
--       validInfixSymbol

-- infixPrefix :: Parser Symbol
-- infixPrefix =
--   P.single backtick *> prefixSymbol <* P.single backtick

-- prefixSymbolGen :: Parser Word8 -> Parser Symbol
-- prefixSymbolGen startParser = do
--   start <- startParser
--   rest <- P.takeWhileP (Just "Valid Middle Symbol") validMiddleSymbol
--   -- Slow O(n) call, could maybe peek ahead instead, then parse it all at once?
--   let new = ByteString.cons start rest
--   if Set.member new reservedWords
--     then fail "symbol is reserved operator"
--     else pure (internText $ Encoding.decodeUtf8 new)

-- -- TODO: this may be bad
-- -- this allows "(*).Foo.(<*>)" to be accepted
-- -- Though Should we allow this since, these are prefix if spelled this way
-- -- we don't enforce capitalization, and thus it would be improper for to
-- -- special case it out!

-- prefixSepGen :: Parser Symbol -> Parser (NonEmpty Symbol)
-- prefixSepGen parser = do
--   ret <- sepBy1H parser (P.char dot)
--   v <- P.optional $ P.try P.eof
--   case v of
--     Nothing -> do
--       d <- P.optional $ P.lookAhead (P.char dot)
--       case d of
--         Nothing -> pure ret
--         Just _ -> fail "symbol not prefix"
--     Just _ -> pure ret

-- -- the permissive functions allow the functions to not fully parse the word
-- -- useful for infix application
-- prefixSymbolDotPermissive :: Parser (NonEmpty Symbol)
-- prefixSymbolDotPermissive = sepBy1H prefixSymbol (P.char dot)

-- prefixSymbolDot :: Parser (NonEmpty Symbol)
-- prefixSymbolDot = prefixSepGen prefixSymbol

-- prefixCapitalDot :: Parser (NonEmpty Symbol)
-- prefixCapitalDot = prefixSepGen prefixCapital

-- prefixSymbol :: Parser Symbol
-- prefixSymbol =
--   P.try (prefixSymbolGen (P.satisfy validStartSymbol))
--     <|> parend

-- parend :: Parser Symbol
-- parend =
--   skipLiner openParen
--     *> spaceLiner (infixSymbolGen infixSymbol') <* P.char closeParen

-- prefixCapital :: Parser Symbol
-- prefixCapital = prefixSymbolGen (P.satisfy validUpperSymbol)

------------------------------------------------------------------------------
-- Top Level Runner
------------------------------------------------------------------------------

-- prettyParse :: ByteString -> Either [Char] (Types.Header Types.TopLevel)
-- prettyParse = left P.errorBundlePretty . parse

-- parse :: ByteString -> Either ParserError (Types.Header Types.TopLevel)
-- parse = P.parse (P.eatSpaces (header <* P.eof)) "" . removeComments

-- -- | Parse multiple files into ML AST
-- parseFiles :: [FilePath] -> IO (Either Error [(NameSymbol.T, [Types.TopLevel])])
-- parseFiles =
--   -- fmap gets through the IO, so that sequenceA flips the either and list
--   fmap sequenceA . traverse parseSingleFile

-- parseFile :: FilePath -> IO (Either Error (NameSymbol.T, [Types.TopLevel]))
-- parseFile = parseSingleFile

-- -- | Parse single file into ML AST
-- parseSingleFile :: FilePath -> IO (Either Error (NameSymbol.T, [Types.TopLevel]))
-- parseSingleFile file = do
--   read <- ByteString.readFile file
--   pure $ case Parser.parse read of
--     Left x ->
--       Left (ParseError x)
--     Right (Types.NoHeader _xs) ->
--       Left (NoHeaderErr file)
--     Right (Types.Header name xs) ->
--       Right (name, xs)

-- _fileNameToModuleName :: FilePath -> NameSymbol.T
-- _fileNameToModuleName =
--   NameSymbol.fromSymbol . intern . toUpperFirst . FilePath.takeBaseName