module Juvix.Compiler.Core.Translation.FromSource
  ( module Juvix.Compiler.Core.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
import Control.Monad.Fail qualified as P
import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (fromList)
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo as LocationInfo
import Juvix.Compiler.Core.Info.NameInfo as NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Translation.Base
import Juvix.Compiler.Core.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: InfoTable -> Text -> Either ParserError (InfoTable, Maybe Node)
parseText = runParser ""

-- | Note: only new symbols and tags that are not in the InfoTable already will be
-- generated during parsing, but nameIds are generated starting from 0
-- regardless of the names already in the InfoTable
runParser :: FilePath -> InfoTable -> Text -> Either ParserError (InfoTable, Maybe Node)
runParser fileName tab input =
  case run $
    runInfoTableBuilder tab $
      runNameIdGen $
        P.runParserT parseToplevel fileName input of
    (_, Left err) -> Left (ParserError err)
    (tbl, Right r) -> Right (tbl, r)

guardSymbolNotDefined ::
  Member InfoTableBuilder r =>
  Symbol ->
  ParsecS r () ->
  ParsecS r ()
guardSymbolNotDefined sym err = do
  b <- lift $ checkSymbolDefined sym
  when b err

createBuiltinConstr ::
  Member NameIdGen r =>
  Symbol ->
  BuiltinDataTag ->
  Text ->
  Type ->
  Interval ->
  Sem r ConstructorInfo
createBuiltinConstr sym btag nameTxt ty i = do
  name <- freshName KNameConstructor nameTxt i
  let n = builtinConstrArgsNum btag
  return $
    ConstructorInfo
      { _constructorName = name,
        _constructorTag = BuiltinTag btag,
        _constructorType = ty,
        _constructorArgsNum = n,
        _constructorInductive = sym
      }

declareInductiveBuiltins ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Text ->
  [(BuiltinDataTag, Text, Type -> Type)] ->
  ParsecS r ()
declareInductiveBuiltins indName ctrs = do
  loc <- curLoc
  let i = mkInterval loc loc
  sym <- lift freshSymbol
  let ty = mkIdent' sym
  constrs <- lift $ mapM (\(tag, name, fty) -> createBuiltinConstr sym tag name (fty ty) i) ctrs
  ioname <- lift $ freshName KNameInductive indName i
  lift $
    registerInductive
      ( InductiveInfo
          { _inductiveName = ioname,
            _inductiveSymbol = sym,
            _inductiveKind = mkDynamic',
            _inductiveConstructors = constrs,
            _inductivePositive = True,
            _inductiveParams = []
          }
      )
  lift $ mapM_ registerConstructor constrs

declareIOBuiltins :: Members '[InfoTableBuilder, NameIdGen] r => ParsecS r ()
declareIOBuiltins =
  declareInductiveBuiltins
    "IO"
    [ (TagReturn, "return", mkPi' mkDynamic'),
      (TagBind, "bind", \ty -> mkPi' ty (mkPi' (mkPi' mkDynamic' ty) ty)),
      (TagWrite, "write", mkPi' mkDynamic'),
      (TagReadLn, "readLn", id)
    ]

declareBoolBuiltins :: Members '[InfoTableBuilder, NameIdGen] r => ParsecS r ()
declareBoolBuiltins =
  declareInductiveBuiltins
    "bool"
    [ (TagTrue, "true", id),
      (TagFalse, "false", id)
    ]

parseToplevel ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r (Maybe Node)
parseToplevel = do
  declareIOBuiltins
  declareBoolBuiltins
  space
  P.endBy statement (kw kwSemicolon)
  r <- optional expression
  P.eof
  return r

statement ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r ()
statement = statementDef <|> statementInductive

statementDef ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r ()
statementDef = do
  kw kwDef
  off <- P.getOffset
  (txt, i) <- identifierL
  r <- lift (getIdent txt)
  case r of
    Just (IdentFun sym) -> do
      guardSymbolNotDefined
        sym
        (parseFailure off ("duplicate definition of: " ++ fromText txt))
      tab <- lift getInfoTable
      let fi = fromMaybe impossible $ HashMap.lookup sym (tab ^. infoIdentifiers)
          ty = fi ^. identifierType
      parseDefinition sym ty
    Just IdentInd {} ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Just IdentConstr {} ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Nothing -> do
      mty <- optional typeAnnotation
      sym <- lift freshSymbol
      name <- lift $ freshName KNameFunction txt i
      let ty = fromMaybe mkDynamic' mty
          info =
            IdentifierInfo
              { _identifierName = Just name,
                _identifierSymbol = sym,
                _identifierType = ty,
                _identifierArgsNum = 0,
                _identifierArgsInfo = [],
                _identifierIsExported = False
              }
      lift $ registerIdent info
      void $ optional (parseDefinition sym ty)

parseDefinition ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Symbol ->
  Type ->
  ParsecS r ()
parseDefinition sym ty = do
  kw kwAssign
  off <- P.getOffset
  node <- expression
  lift $ registerIdentNode sym node
  let (is, _) = unfoldLambdas node
  when
    ( length is > length (typeArgs ty)
        && not (isDynamic (typeTarget ty))
    )
    $ parseFailure off "type mismatch: too many lambdas"
  lift $ setIdentArgsInfo sym (map (toArgumentInfo . (^. lambdaLhsBinder)) is)
  where
    toArgumentInfo :: Binder -> ArgumentInfo
    toArgumentInfo bi =
      ArgumentInfo
        { _argumentName = bi ^. binderName,
          _argumentType = bi ^. binderType,
          _argumentIsImplicit = Explicit
        }

statementInductive ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r ()
statementInductive = do
  kw kwInductive
  off <- P.getOffset
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  when (isJust idt) $
    parseFailure off ("duplicate identifier: " ++ fromText txt)
  mty <- optional typeAnnotation
  sym <- lift freshSymbol
  name <- lift $ freshName KNameConstructor txt i
  let ii =
        InductiveInfo
          { _inductiveName = name,
            _inductiveSymbol = sym,
            _inductiveKind = fromMaybe (mkUniv' 0) mty,
            _inductiveConstructors = [],
            _inductiveParams = [],
            _inductivePositive = True
          }
  lift $ registerInductive ii
  ctrs <- braces $ P.sepEndBy (constrDecl sym) (kw kwSemicolon)
  lift $ registerInductive ii {_inductiveConstructors = ctrs}

constrDecl ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Symbol ->
  ParsecS r ConstructorInfo
constrDecl symInd = do
  off <- P.getOffset
  (txt, i) <- identifierL
  idt <- lift $ getIdent txt
  when (isJust idt) $
    parseFailure off ("duplicate identifier: " ++ fromText txt)
  tag <- lift freshTag
  ty <- typeAnnotation
  name <- lift $ freshName KNameConstructor txt i
  let ci =
        ConstructorInfo
          { _constructorName = name,
            _constructorTag = tag,
            _constructorArgsNum = length (typeArgs ty),
            _constructorType = ty,
            _constructorInductive = symInd
          }
  lift $ registerConstructor ci
  return ci

typeAnnotation ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r Type
typeAnnotation = do
  kw kwColon
  expression

expression ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r Node
expression = do
  node <- expr 0 mempty
  tab <- lift getInfoTable
  return $ etaExpandApps tab node

expr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  -- | current de Bruijn index, i.e., the number of binders upwards
  Index ->
  -- | reverse de Bruijn indices (de Bruijn levels)
  HashMap Text Level ->
  ParsecS r Node
expr varsNum vars = typeExpr varsNum vars

bracedExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
bracedExpr varsNum vars = braces (expr varsNum vars) <|> expr varsNum vars

typeExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
typeExpr varsNum vars = ioExpr varsNum vars >>= typeExpr' varsNum vars

typeExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
typeExpr' varsNum vars node =
  typeFunExpr' varsNum vars node
    <|> return node

typeFunExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
typeFunExpr' varsNum vars l = do
  kw kwRightArrow
  r <- typeExpr (varsNum + 1) vars
  return $ mkPi' l r

ioExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
ioExpr varsNum vars = cmpExpr varsNum vars >>= ioExpr' varsNum vars

ioExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
ioExpr' varsNum vars node =
  bindExpr' varsNum vars node
    <|> seqExpr' varsNum vars node
    <|> return node

bindExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
bindExpr' varsNum vars node = do
  kw kwBind
  node' <- cmpExpr varsNum vars
  ioExpr' varsNum vars (mkConstr Info.empty (BuiltinTag TagBind) [node, node'])

seqExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
seqExpr' varsNum vars node = do
  ((), i) <- interval (kw kwSeq)
  node' <- cmpExpr (varsNum + 1) vars
  name <- lift $ freshName KNameLocal "_" i
  ioExpr' varsNum vars $
    mkConstr
      Info.empty
      (BuiltinTag TagBind)
      [node, mkLambda mempty (Binder (Just name) mkDynamic') node']

cmpExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
cmpExpr varsNum vars = arithExpr varsNum vars >>= cmpExpr' varsNum vars

cmpExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
cmpExpr' varsNum vars node =
  eqExpr' varsNum vars node
    <|> ltExpr' varsNum vars node
    <|> leExpr' varsNum vars node
    <|> gtExpr' varsNum vars node
    <|> geExpr' varsNum vars node
    <|> return node

eqExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
eqExpr' varsNum vars node = do
  kw kwEq
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpEq [node, node']

ltExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
ltExpr' varsNum vars node = do
  kw kwLt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node, node']

leExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
leExpr' varsNum vars node = do
  kw kwLe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node, node']

gtExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
gtExpr' varsNum vars node = do
  kw kwGt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node', node]

geExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
geExpr' varsNum vars node = do
  kw kwGe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node', node]

arithExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
arithExpr varsNum vars = factorExpr varsNum vars >>= arithExpr' varsNum vars

arithExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
arithExpr' varsNum vars node =
  plusExpr' varsNum vars node
    <|> minusExpr' varsNum vars node
    <|> return node

plusExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
plusExpr' varsNum vars node = do
  kw kwPlus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntAdd [node, node'])

minusExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
minusExpr' varsNum vars node = do
  kw kwMinus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntSub [node, node'])

factorExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
factorExpr varsNum vars = appExpr varsNum vars >>= factorExpr' varsNum vars

factorExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
factorExpr' varsNum vars node =
  mulExpr' varsNum vars node
    <|> divExpr' varsNum vars node
    <|> modExpr' varsNum vars node
    <|> return node

mulExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
mulExpr' varsNum vars node = do
  kw kwMul
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMul [node, node'])

divExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
divExpr' varsNum vars node = do
  kw kwDiv
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntDiv [node, node'])

modExpr' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
modExpr' varsNum vars node = do
  kw kwMod
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMod [node, node'])

appExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
appExpr varsNum vars = builtinAppExpr varsNum vars <|> atoms varsNum vars

builtinAppExpr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
builtinAppExpr varsNum vars = do
  op <-
    (kw kwEq >> return OpEq)
      <|> (kw kwLt >> return OpIntLt)
      <|> (kw kwLe >> return OpIntLe)
      <|> (kw kwPlus >> return OpIntAdd)
      <|> (kw kwMinus >> return OpIntSub)
      <|> (kw kwDiv >> return OpIntDiv)
      <|> (kw kwMul >> return OpIntMul)
      <|> (kw kwTrace >> return OpTrace)
      <|> (kw kwFail >> return OpFail)
  args <- P.many (atom varsNum vars)
  return $ mkBuiltinApp' op args

atoms ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
atoms varsNum vars = do
  es <- NonEmpty.some (atom varsNum vars)
  return $ mkApps' (head es) (NonEmpty.tail es)

atom ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
atom varsNum vars =
  exprConstInt
    <|> exprConstString
    <|> exprUniverse
    <|> exprDynamic
    <|> exprTypePrim
    <|> exprPi varsNum vars
    <|> exprLambda varsNum vars
    <|> exprLetrecMany varsNum vars
    <|> exprLetrecOne varsNum vars
    <|> exprLet varsNum vars
    <|> exprCase varsNum vars
    <|> exprMatch varsNum vars
    <|> exprIf varsNum vars
    <|> parens (expr varsNum vars)
    <|> exprNamed varsNum vars

exprConstInt :: ParsecS r Node
exprConstInt = P.try $ do
  (n, i) <- integer
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstInteger n)

exprConstString :: ParsecS r Node
exprConstString = P.try $ do
  (s, i) <- string
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstString s)

exprUniverse :: ParsecS r Type
exprUniverse = do
  kw kwType
  level <- optional (number 0 128) -- TODO: global Limits.hs file
  return $ mkUniv' (maybe 0 fst level)

exprDynamic :: ParsecS r Type
exprDynamic = kw kwAny $> mkDynamic'

exprTypePrim :: ParsecS r Type
exprTypePrim = P.try $ do
  txt <- identifier
  case txt of
    "int" -> return mkTypeInteger'
    "string" -> return mkTypeString'
    _ -> P.fail "not a primitive type"

parseLocalName ::
  forall r.
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r Name
parseLocalName = parseWildcardName <|> parseIdentName
  where
    parseWildcardName :: ParsecS r Name
    parseWildcardName = do
      ((), i) <- interval (kw kwWildcard)
      lift $ freshName KNameLocal "_" i

    parseIdentName :: ParsecS r Name
    parseIdentName = do
      (txt, i) <- identifierL
      lift $ freshName KNameLocal txt i

exprPi ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprPi varsNum vars = do
  kw kwPi
  name <- parseLocalName
  kw kwColon
  ty <- expr varsNum vars
  kw kwComma
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
      bi = Binder (Just name) ty
  body <- expr (varsNum + 1) vars'
  return $ mkPi mempty bi body

exprLambda ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLambda varsNum vars = do
  lambda
  (name, mty) <- lambdaName
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
      bi = Binder (Just name) (fromMaybe mkDynamic' mty)
  body <- bracedExpr (varsNum + 1) vars'
  return $ mkLambda mempty bi body
  where
    lambdaName =
      parens
        ( do
            n <- parseLocalName
            kw kwColon
            ty <- expr varsNum vars
            return (n, Just ty)
        )
        <|> (\n -> (n, Nothing)) <$> parseLocalName

exprLetrecOne ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLetrecOne varsNum vars = do
  kw kwLetRec
  name <- parseLocalName
  kw kwAssign
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
  value <- bracedExpr (varsNum + 1) vars'
  kw kwIn
  body <- bracedExpr (varsNum + 1) vars'
  let item :: LetItem
      item = LetItem (Binder (Just name) mkDynamic') value
  return $ mkLetRec mempty (pure item) body

exprLetrecMany ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLetrecMany varsNum vars = do
  off <- P.getOffset
  defNames <- P.try (kw kwLetRec >> letrecNames)
  when (null defNames) $
    parseFailure off "expected at least one identifier name in letrec signature"
  let (vars', varsNum') = foldl' (\(vs, k) txt -> (HashMap.insert txt k vs, k + 1)) (vars, varsNum) defNames
  defs <- letrecDefs defNames varsNum' vars'
  kw kwIn
  body <- bracedExpr varsNum' vars'
  return $ mkLetRec mempty defs body

letrecNames :: ParsecS r (NonEmpty Text)
letrecNames = P.between (symbol "[") (symbol "]") (NonEmpty.some identifier)

letrecDefs ::
  forall r.
  Members '[InfoTableBuilder, NameIdGen] r =>
  NonEmpty Text ->
  Index ->
  HashMap Text Level ->
  ParsecS r (NonEmpty LetItem)
letrecDefs names varsNum vars = forM names letrecItem
  where
    letrecItem :: Text -> ParsecS r LetItem
    letrecItem n = do
      off <- P.getOffset
      (txt, i) <- identifierL
      when (n /= txt) $
        parseFailure off "identifier name doesn't match letrec signature"
      name <- lift $ freshName KNameLocal txt i
      kw kwAssign
      v <- bracedExpr varsNum vars
      kw kwSemicolon
      return $ LetItem (Binder (Just name) mkDynamic') v

letrecDef ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Name, Node)
letrecDef varsNum vars = do
  (txt, i) <- identifierL
  name <- lift $ freshName KNameLocal txt i
  kw kwAssign
  v <- bracedExpr varsNum vars
  return (name, v)

exprLet ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLet varsNum vars = do
  kw kwLet
  name <- parseLocalName
  mty <- optional (kw kwColon >> expr varsNum vars)
  kw kwAssign
  value <- bracedExpr varsNum vars
  kw kwIn
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
      binder = Binder (Just name) (fromMaybe mkDynamic' mty)
  body <- bracedExpr (varsNum + 1) vars'
  return $ mkLet mempty binder value body

exprCase ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprCase varsNum vars = do
  off <- P.getOffset
  kw kwCase
  value <- bracedExpr varsNum vars
  kw kwOf
  braces (exprCase' off value varsNum vars)
    <|> exprCase' off value varsNum vars

exprCase' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Int ->
  Node ->
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprCase' off value varsNum vars = do
  bs <- P.sepEndBy (caseBranchP varsNum vars) (kw kwSemicolon)
  let bss = map fromLeft' $ filter isLeft bs
  let def' = map fromRight' $ filter isRight bs
  case def' of
    [def] ->
      return $ mkCase' value bss (Just def)
    [] ->
      return $ mkCase' value bss Nothing
    _ ->
      parseFailure off "multiple default branches"

caseBranchP ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Either CaseBranch Node)
caseBranchP varsNum vars =
  (caseDefaultBranch varsNum vars <&> Right)
    <|> (caseMatchingBranch varsNum vars <&> Left)

caseDefaultBranch ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
caseDefaultBranch varsNum vars = do
  kw kwWildcard
  kw kwAssign
  bracedExpr varsNum vars

caseMatchingBranch ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r CaseBranch
caseMatchingBranch varsNum vars = do
  off <- P.getOffset
  txt <- identifier
  r <- lift (getIdent txt)
  case r of
    Just IdentFun {} ->
      parseFailure off ("not a constructor: " ++ fromText txt)
    Just IdentInd {} ->
      parseFailure off ("not a constructor: " ++ fromText txt)
    Just (IdentConstr tag) -> do
      ns :: [Name] <- P.many parseLocalName
      let bindersNum = length ns
      ci <- lift $ getConstructorInfo tag
      when
        (ci ^. constructorArgsNum /= bindersNum)
        (parseFailure off "wrong number of constructor arguments")
      kw kwAssign
      let vars' =
            fst $
              foldl'
                ( \(vs, k) name ->
                    (HashMap.insert (name ^. nameText) k vs, k + 1)
                )
                (vars, varsNum)
                ns
      br <- bracedExpr (varsNum + bindersNum) vars'
      let info = setInfoName (ci ^. constructorName) mempty
          binders = [Binder (Just name) mkDynamic' | name <- ns]
      return $ CaseBranch info tag binders bindersNum br
    Nothing ->
      parseFailure off ("undeclared identifier: " ++ fromText txt)

exprIf ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprIf varsNum vars = do
  kw kwIf
  value <- bracedExpr varsNum vars
  kw kwThen
  br1 <- bracedExpr varsNum vars
  kw kwElse
  br2 <- bracedExpr varsNum vars
  return $ mkIf Info.empty value br1 br2

exprMatch ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprMatch varsNum vars = do
  kw kwMatch
  values <- P.sepBy (bracedExpr varsNum vars) (kw kwComma)
  kw kwWith
  braces (exprMatch' values varsNum vars)
    <|> exprMatch' values varsNum vars

exprMatch' ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  [Node] ->
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprMatch' values varsNum vars = do
  bs <- P.sepEndBy (matchBranch (length values) varsNum vars) (kw kwSemicolon)
  return $ mkMatch' (fromList values) bs

matchBranch ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Int ->
  Index ->
  HashMap Text Level ->
  ParsecS r MatchBranch
matchBranch patsNum varsNum vars = do
  off <- P.getOffset
  pats <- P.sepBy branchPattern (kw kwComma)
  kw kwAssign
  unless (length pats == patsNum) $
    parseFailure off "wrong number of patterns"
  let pis :: [Binder]
      pis = concatMap getPatternBinders pats
      (vars', varsNum') =
        foldl'
          ( \(vs, k) name ->
              (HashMap.insert (name ^. nameText) k vs, k + 1)
          )
          (vars, varsNum)
          (map (fromJust . (^. binderName)) pis)
  br <- bracedExpr varsNum' vars'
  return $ MatchBranch Info.empty (fromList pats) br

branchPattern ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r Pattern
branchPattern =
  wildcardPattern
    <|> binderOrConstrPattern True
    <|> parens branchPattern

wildcardPattern :: ParsecS r Pattern
wildcardPattern = do
  kw kwWildcard
  return $ PatWildcard (PatternWildcard Info.empty)

binderOrConstrPattern ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Bool ->
  ParsecS r Pattern
binderOrConstrPattern parseArgs = do
  off <- P.getOffset
  (txt, i) <- identifierL
  r <- lift (getIdent txt)
  case r of
    Just (IdentConstr tag) -> do
      ps <- if parseArgs then P.many branchPattern else return []
      ci <- lift $ getConstructorInfo tag
      when
        (ci ^. constructorArgsNum /= length ps)
        (parseFailure off "wrong number of constructor arguments")
      let info = setInfoName (ci ^. constructorName) Info.empty
      return $ PatConstr (PatternConstr info tag ps)
    _ -> do
      n <- lift $ freshName KNameLocal txt i
      mp <- optional binderPattern
      let pat = fromMaybe (PatWildcard (PatternWildcard Info.empty)) mp
          binder = Binder (Just n) mkDynamic'
      return $ PatBinder (PatternBinder binder pat)

binderPattern ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  ParsecS r Pattern
binderPattern = do
  symbolAt
  wildcardPattern
    <|> binderOrConstrPattern False
    <|> parens branchPattern

exprNamed ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprNamed varsNum vars = do
  off <- P.getOffset
  (txt, i) <- identifierL
  case HashMap.lookup txt vars of
    Just k -> do
      name <- lift $ freshName KNameLocal txt i
      return $ mkVar (Info.singleton (NameInfo name)) (varsNum - k - 1)
    Nothing -> do
      r <- lift (getIdent txt)
      case r of
        Just (IdentFun sym) -> do
          name <- lift $ freshName KNameFunction txt i
          return $ mkIdent (Info.singleton (NameInfo name)) sym
        Just (IdentInd sym) -> do
          name <- lift $ freshName KNameConstructor txt i
          return $ mkTypeConstr (Info.singleton (NameInfo name)) sym []
        Just (IdentConstr tag) -> do
          name <- lift $ freshName KNameConstructor txt i
          return $ mkConstr (Info.singleton (NameInfo name)) tag []
        Nothing ->
          parseFailure off ("undeclared identifier: " ++ fromText txt)
