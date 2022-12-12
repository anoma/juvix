module Juvix.Compiler.Core.Translation.FromSource
  ( module Juvix.Compiler.Core.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as NonEmpty
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
import Juvix.Compiler.Core.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: InfoTable -> Text -> Either ParserError (InfoTable, Maybe Node)
parseText = runParser ""

-- | Note: only new symbols and tags that are not in the InfoTable already will be
-- generated during parsing
runParser :: FilePath -> InfoTable -> Text -> Either ParserError (InfoTable, Maybe Node)
runParser fileName tab input =
  case run $
    runInfoTableBuilder tab $
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
  Symbol ->
  Tag ->
  Text ->
  Type ->
  Interval ->
  Maybe BuiltinConstructor ->
  Sem r ConstructorInfo
createBuiltinConstr sym tag nameTxt ty i cblt = do
  return $
    ConstructorInfo
      { _constructorName = nameTxt,
        _constructorLocation = Just i,
        _constructorTag = tag,
        _constructorType = ty,
        _constructorArgsNum = length (typeArgs ty),
        _constructorInductive = sym,
        _constructorBuiltin = cblt
      }

declareInductiveBuiltins ::
  Member InfoTableBuilder r =>
  Text ->
  Maybe BuiltinInductive ->
  [(Tag, Text, Type -> Type, Maybe BuiltinConstructor)] ->
  ParsecS r ()
declareInductiveBuiltins indName blt ctrs = do
  loc <- curLoc
  let i = mkInterval loc loc
  sym <- lift freshSymbol
  let ty = mkIdent' sym
  constrs <- lift $ mapM (\(tag, name, fty, cblt) -> createBuiltinConstr sym tag name (fty ty) i cblt) ctrs
  lift $
    registerInductive
      indName
      ( InductiveInfo
          { _inductiveName = indName,
            _inductiveLocation = Just i,
            _inductiveSymbol = sym,
            _inductiveKind = mkDynamic',
            _inductiveConstructors = constrs,
            _inductivePositive = True,
            _inductiveParams = [],
            _inductiveBuiltin = blt
          }
      )
  lift $ mapM_ (\ci -> registerConstructor (ci ^. constructorName) ci) constrs

declareIOBuiltins :: Member InfoTableBuilder r => ParsecS r ()
declareIOBuiltins =
  declareInductiveBuiltins
    "IO"
    Nothing
    [ (BuiltinTag TagReturn, "return", mkPi' mkDynamic', Nothing),
      (BuiltinTag TagBind, "bind", \ty -> mkPi' ty (mkPi' (mkPi' mkDynamic' ty) ty), Nothing),
      (BuiltinTag TagWrite, "write", mkPi' mkDynamic', Nothing),
      (BuiltinTag TagReadLn, "readLn", id, Nothing)
    ]

declareBoolBuiltins :: Member InfoTableBuilder r => ParsecS r ()
declareBoolBuiltins =
  declareInductiveBuiltins
    "bool"
    (Just BuiltinBool)
    [ (BuiltinTag TagTrue, "true", id, Just BuiltinBoolTrue),
      (BuiltinTag TagFalse, "false", id, Just BuiltinBoolFalse)
    ]

declareNatBuiltins :: Member InfoTableBuilder r => ParsecS r ()
declareNatBuiltins = do
  tagZero <- lift freshTag
  tagSuc <- lift freshTag
  declareInductiveBuiltins
    "nat"
    (Just BuiltinNat)
    [ (tagZero, "zero", id, Just BuiltinNatZero),
      (tagSuc, "suc", \x -> mkPi' x x, Just BuiltinNatSuc)
    ]

parseToplevel ::
  Member InfoTableBuilder r =>
  ParsecS r (Maybe Node)
parseToplevel = do
  declareIOBuiltins
  declareBoolBuiltins
  declareNatBuiltins
  space
  P.endBy statement (kw kwSemicolon)
  r <- optional expression
  P.eof
  return r

statement ::
  Member InfoTableBuilder r =>
  ParsecS r ()
statement = statementBuiltin <|> void statementDef <|> statementInductive

statementBuiltin ::
  Member InfoTableBuilder r =>
  ParsecS r ()
statementBuiltin = do
  off <- P.getOffset
  kw kwBuiltin
  sym <- statementDef
  ii <- lift $ getIdentifierInfo sym
  case ii ^. identifierName of
    "plus" -> lift $ registerIdent (ii ^. identifierName) ii{_identifierBuiltin = Just BuiltinNatPlus}
    _ -> parseFailure off "unrecorgnized builtin definition"

statementDef ::
  Member InfoTableBuilder r =>
  ParsecS r Symbol
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
      return sym
    Just IdentInd {} ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Just IdentConstr {} ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Nothing -> do
      mty <- optional typeAnnotation
      sym <- lift freshSymbol
      let ty = fromMaybe mkDynamic' mty
          info =
            IdentifierInfo
              { _identifierName = txt,
                _identifierLocation = Just i,
                _identifierSymbol = sym,
                _identifierType = ty,
                _identifierArgsNum = 0,
                _identifierArgsInfo = [],
                _identifierIsExported = False,
                _identifierBuiltin = Nothing
              }
      lift $ registerIdent txt info
      void $ optional (parseDefinition sym ty)
      return sym

parseDefinition ::
  Member InfoTableBuilder r =>
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
          _argumentLocation = bi ^. binderLocation,
          _argumentType = bi ^. binderType,
          _argumentIsImplicit = Explicit
        }

statementInductive ::
  Member InfoTableBuilder r =>
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
  let ii =
        InductiveInfo
          { _inductiveName = txt,
            _inductiveLocation = Just i,
            _inductiveSymbol = sym,
            _inductiveKind = fromMaybe (mkUniv' 0) mty,
            _inductiveConstructors = [],
            _inductiveParams = [],
            _inductivePositive = True,
            _inductiveBuiltin = Nothing
          }
  lift $ registerInductive txt ii
  ctrs <- braces $ P.sepEndBy (constrDecl sym) (kw kwSemicolon)
  lift $ registerInductive txt ii {_inductiveConstructors = ctrs}

constrDecl ::
  Member InfoTableBuilder r =>
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
  let ci =
        ConstructorInfo
          { _constructorName = txt,
            _constructorLocation = Just i,
            _constructorTag = tag,
            _constructorArgsNum = length (typeArgs ty),
            _constructorType = ty,
            _constructorInductive = symInd,
            _constructorBuiltin = Nothing
          }
  lift $ registerConstructor txt ci
  return ci

typeAnnotation ::
  Member InfoTableBuilder r =>
  ParsecS r Type
typeAnnotation = do
  kw kwColon
  expression

expression ::
  Member InfoTableBuilder r =>
  ParsecS r Node
expression = do
  node <- expr 0 mempty
  tab <- lift getInfoTable
  return $ etaExpandApps tab node

expr ::
  Member InfoTableBuilder r =>
  -- | current de Bruijn index, i.e., the number of binders upwards
  Index ->
  -- | reverse de Bruijn indices (de Bruijn levels)
  HashMap Text Level ->
  ParsecS r Node
expr varsNum vars = typeExpr varsNum vars

bracedExpr ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
bracedExpr varsNum vars = braces (expr varsNum vars) <|> expr varsNum vars

typeExpr ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
typeExpr varsNum vars = ioExpr varsNum vars >>= typeExpr' varsNum vars

typeExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
typeExpr' varsNum vars node =
  typeFunExpr' varsNum vars node
    <|> return node

typeFunExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
typeFunExpr' varsNum vars l = do
  kw kwRightArrow
  r <- typeExpr (varsNum + 1) vars
  return $ mkPi' l r

ioExpr ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
ioExpr varsNum vars = cmpExpr varsNum vars >>= ioExpr' varsNum vars

ioExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
ioExpr' varsNum vars node =
  bindExpr' varsNum vars node
    <|> seqExpr' varsNum vars node
    <|> return node

bindExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
bindExpr' varsNum vars node = do
  kw kwBind
  node' <- cmpExpr varsNum vars
  ioExpr' varsNum vars (mkConstr Info.empty (BuiltinTag TagBind) [node, node'])

seqExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
seqExpr' varsNum vars node = do
  ((), i) <- interval (kw kwSeq)
  node' <- cmpExpr (varsNum + 1) vars
  ioExpr' varsNum vars $
    mkConstr
      Info.empty
      (BuiltinTag TagBind)
      [node, mkLambda mempty (Binder "_" (Just i) mkDynamic') node']

cmpExpr ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
cmpExpr varsNum vars = arithExpr varsNum vars >>= cmpExpr' varsNum vars

cmpExpr' ::
  Member InfoTableBuilder r =>
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
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
eqExpr' varsNum vars node = do
  kw kwEq
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpEq [node, node']

ltExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
ltExpr' varsNum vars node = do
  kw kwLt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node, node']

leExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
leExpr' varsNum vars node = do
  kw kwLe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node, node']

gtExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
gtExpr' varsNum vars node = do
  kw kwGt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node', node]

geExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
geExpr' varsNum vars node = do
  kw kwGe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node', node]

arithExpr ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
arithExpr varsNum vars = factorExpr varsNum vars >>= arithExpr' varsNum vars

arithExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
arithExpr' varsNum vars node =
  plusExpr' varsNum vars node
    <|> minusExpr' varsNum vars node
    <|> return node

plusExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
plusExpr' varsNum vars node = do
  kw kwPlus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntAdd [node, node'])

minusExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
minusExpr' varsNum vars node = do
  kw kwMinus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntSub [node, node'])

factorExpr ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
factorExpr varsNum vars = appExpr varsNum vars >>= factorExpr' varsNum vars

factorExpr' ::
  Member InfoTableBuilder r =>
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
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
mulExpr' varsNum vars node = do
  kw kwMul
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMul [node, node'])

divExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
divExpr' varsNum vars node = do
  kw kwDiv
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntDiv [node, node'])

modExpr' ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  Node ->
  ParsecS r Node
modExpr' varsNum vars node = do
  kw kwMod
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMod [node, node'])

appExpr ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
appExpr varsNum vars = builtinAppExpr varsNum vars <|> atoms varsNum vars

builtinAppExpr ::
  Member InfoTableBuilder r =>
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
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
atoms varsNum vars = do
  es <- NonEmpty.some (atom varsNum vars)
  return $ mkApps' (head es) (NonEmpty.tail es)

atom ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
atom varsNum vars =
  exprConstInt
    <|> exprConstString
    <|> exprUniverse
    <|> exprDynamic
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

parseLocalName :: ParsecS r (Text, Location)
parseLocalName = parseWildcardName <|> parseIdentName
  where
    parseWildcardName :: ParsecS r (Text, Location)
    parseWildcardName = do
      ((), i) <- interval (kw kwWildcard)
      return ("_", i)

    parseIdentName :: ParsecS r (Text, Location)
    parseIdentName = identifierL

exprPi ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprPi varsNum vars = do
  kw kwPi
  (name, loc) <- parseLocalName
  kw kwColon
  ty <- expr varsNum vars
  kw kwComma
  let vars' = HashMap.insert name varsNum vars
      bi = Binder name (Just loc) ty
  body <- expr (varsNum + 1) vars'
  return $ mkPi mempty bi body

exprLambda ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLambda varsNum vars = do
  lambda
  ((name, loc), mty) <- lambdaName
  let vars' = HashMap.insert name varsNum vars
      bi = Binder name (Just loc) (fromMaybe mkDynamic' mty)
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
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLetrecOne varsNum vars = do
  kw kwLetRec
  (name, loc) <- parseLocalName
  kw kwAssign
  let vars' = HashMap.insert name varsNum vars
  value <- bracedExpr (varsNum + 1) vars'
  kw kwIn
  body <- bracedExpr (varsNum + 1) vars'
  let item :: LetItem
      item = LetItem (Binder name (Just loc) mkDynamic') value
  return $ mkLetRec mempty (pure item) body

exprLetrecMany ::
  Member InfoTableBuilder r =>
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
  Member InfoTableBuilder r =>
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
      kw kwAssign
      v <- bracedExpr varsNum vars
      kw kwSemicolon
      return $ LetItem (Binder txt (Just i) mkDynamic') v

letrecDef ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Text, Location, Node)
letrecDef varsNum vars = do
  (txt, i) <- identifierL
  kw kwAssign
  v <- bracedExpr varsNum vars
  return (txt, i, v)

exprLet ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprLet varsNum vars = do
  kw kwLet
  (name, loc) <- parseLocalName
  mty <- optional (kw kwColon >> expr varsNum vars)
  kw kwAssign
  value <- bracedExpr varsNum vars
  kw kwIn
  let vars' = HashMap.insert name varsNum vars
      binder = Binder name (Just loc) (fromMaybe mkDynamic' mty)
  body <- bracedExpr (varsNum + 1) vars'
  return $ mkLet mempty binder value body

exprCase ::
  Member InfoTableBuilder r =>
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
  Member InfoTableBuilder r =>
  Int ->
  Node ->
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprCase' off value varsNum vars = do
  bs <- P.sepEndBy (caseBranchP varsNum vars) (kw kwSemicolon)
  let bss = map fromLeft' $ filter isLeft bs
  let def' = map fromRight' $ filter isRight bs
  case bss of
    CaseBranch {..} : _ -> do
      ci <- lift $ getConstructorInfo _caseBranchTag
      let sym = ci ^. constructorInductive
      case def' of
        [def] ->
          return $ mkCase' sym value bss (Just def)
        [] ->
          return $ mkCase' sym value bss Nothing
        _ ->
          parseFailure off "multiple default branches"
    [] ->
      case def' of
        [_] ->
          parseFailure off "case with only the default branch not allowed"
        [] ->
          parseFailure off "case without branches not allowed"
        _ ->
          parseFailure off "multiple default branches"

caseBranchP ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r (Either CaseBranch Node)
caseBranchP varsNum vars =
  (caseDefaultBranch varsNum vars <&> Right)
    <|> (caseMatchingBranch varsNum vars <&> Left)

caseDefaultBranch ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
caseDefaultBranch varsNum vars = do
  kw kwWildcard
  kw kwAssign
  bracedExpr varsNum vars

caseMatchingBranch ::
  Member InfoTableBuilder r =>
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
      ns :: [(Text, Location)] <- P.many parseLocalName
      let bindersNum = length ns
      ci <- lift $ getConstructorInfo tag
      when
        (ci ^. constructorArgsNum /= bindersNum)
        (parseFailure off "wrong number of constructor arguments")
      kw kwAssign
      let vars' =
            fst $
              foldl'
                ( \(vs, k) (name, _) ->
                    (HashMap.insert name k vs, k + 1)
                )
                (vars, varsNum)
                ns
      br <- bracedExpr (varsNum + bindersNum) vars'
      let info = setInfoName (ci ^. constructorName) mempty
          binders = zipWith (\(name, loc) -> Binder name (Just loc)) ns (typeArgs (ci ^. constructorType) ++ repeat mkDynamic')
      return $ CaseBranch info tag binders bindersNum br
    Nothing ->
      parseFailure off ("undeclared identifier: " ++ fromText txt)

exprIf ::
  Member InfoTableBuilder r =>
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
  sym <- lift getBoolSymbol
  return $ mkIf mempty sym value br1 br2

exprMatch ::
  Member InfoTableBuilder r =>
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
  Member InfoTableBuilder r =>
  [Node] ->
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprMatch' values varsNum vars = do
  bs <- P.sepEndBy (matchBranch (length values) varsNum vars) (kw kwSemicolon)
  return $ mkMatch' (fromList values) bs

matchBranch ::
  Member InfoTableBuilder r =>
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
              (HashMap.insert name k vs, k + 1)
          )
          (vars, varsNum)
          (map (^. binderName) pis)
  br <- bracedExpr varsNum' vars'
  return $ MatchBranch Info.empty (fromList pats) br

branchPattern ::
  Member InfoTableBuilder r =>
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
  Member InfoTableBuilder r =>
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
      mp <- optional binderPattern
      let pat = fromMaybe (PatWildcard (PatternWildcard Info.empty)) mp
          binder = Binder txt (Just i) mkDynamic'
      return $ PatBinder (PatternBinder binder pat)

binderPattern ::
  Member InfoTableBuilder r =>
  ParsecS r Pattern
binderPattern = do
  symbolAt
  wildcardPattern
    <|> binderOrConstrPattern False
    <|> parens branchPattern

exprNamed ::
  Member InfoTableBuilder r =>
  Index ->
  HashMap Text Level ->
  ParsecS r Node
exprNamed varsNum vars = do
  off <- P.getOffset
  (txt, i) <- identifierL
  case txt of
    "int" -> return mkTypeInteger'
    "string" -> return mkTypeString'
    _ ->
      case HashMap.lookup txt vars of
        Just k -> do
          return $ mkVar (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) (varsNum - k - 1)
        Nothing -> do
          r <- lift (getIdent txt)
          case r of
            Just (IdentFun sym) -> do
              return $ mkIdent (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) sym
            Just (IdentInd sym) -> do
              return $ mkTypeConstr (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) sym []
            Just (IdentConstr tag) -> do
              return $ mkConstr (Info.insert (LocationInfo i) (Info.singleton (NameInfo txt))) tag []
            Nothing ->
              parseFailure off ("undeclared identifier: " ++ fromText txt)
