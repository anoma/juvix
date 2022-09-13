module Juvix.Compiler.Core.Translation.FromSource
  ( module Juvix.Compiler.Core.Translation.FromSource,
    module Juvix.Parser.Error,
  )
where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty (fromList)
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.BinderInfo as BinderInfo
import Juvix.Compiler.Core.Info.LocationInfo as LocationInfo
import Juvix.Compiler.Core.Info.NameInfo as NameInfo
import Juvix.Compiler.Core.Info.TypeInfo as TypeInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: InfoTable -> Text -> Either ParserError (InfoTable, Maybe Node)
parseText = runParser "" ""

-- | Note: only new symbols and tags that are not in the InfoTable already will be
-- generated during parsing, but nameIds are generated starting from 0
-- regardless of the names already in the InfoTable
runParser :: FilePath -> FilePath -> InfoTable -> Text -> Either ParserError (InfoTable, Maybe Node)
runParser root fileName tab input =
  case run $
    runInfoTableBuilder tab $
      runReader params $
        runNameIdGen $
          P.runParserT parseToplevel fileName input of
    (_, Left err) -> Left (ParserError err)
    (tbl, Right r) -> Right (tbl, r)
  where
    params =
      ParserParams
        { _parserParamsRoot = root
        }

binderNameInfo :: Name -> Info
binderNameInfo name =
  Info.singleton (BinderInfo (Info.singleton (NameInfo name)))

freshName ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  NameKind ->
  Text ->
  Interval ->
  Sem r Name
freshName kind txt i = do
  nid <- freshNameId
  return $
    Name
      { _nameText = txt,
        _nameId = nid,
        _nameKind = kind,
        _namePretty = txt,
        _nameLoc = i
      }

declareBuiltinConstr ::
  Members '[InfoTableBuilder, NameIdGen] r =>
  BuiltinDataTag ->
  Text ->
  Interval ->
  Sem r ()
declareBuiltinConstr btag nameTxt i = do
  sym <- freshSymbol
  name <- freshName KNameConstructor nameTxt i
  registerConstructor
    ( ConstructorInfo
        { _constructorName = name,
          _constructorTag = BuiltinTag btag,
          _constructorType = mkDynamic',
          _constructorArgsNum = builtinConstrArgsNum btag,
          _constructorInductive = sym
        }
    )

guardSymbolNotDefined ::
  Member InfoTableBuilder r =>
  Symbol ->
  ParsecS r () ->
  ParsecS r ()
guardSymbolNotDefined sym err = do
  b <- lift $ checkSymbolDefined sym
  when b err

declareBuiltins :: Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r => ParsecS r ()
declareBuiltins = do
  loc <- curLoc
  let i = mkInterval loc loc
  lift $ declareBuiltinConstr TagTrue "true" i
  lift $ declareBuiltinConstr TagFalse "false" i
  lift $ declareBuiltinConstr TagReturn "return" i
  lift $ declareBuiltinConstr TagBind "bind" i
  lift $ declareBuiltinConstr TagWrite "write" i
  lift $ declareBuiltinConstr TagReadLn "readLn" i

parseToplevel ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r (Maybe Node)
parseToplevel = do
  declareBuiltins
  space
  P.endBy statement kwSemicolon
  r <- optional expression
  P.eof
  return r

statement ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r ()
statement = statementDef <|> statementConstr

statementDef ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r ()
statementDef = do
  kwDef
  off <- P.getOffset
  (txt, i) <- identifierL
  r <- lift (getIdent txt)
  case r of
    Just (IdentSym sym) -> do
      guardSymbolNotDefined
        sym
        (parseFailure off ("duplicate definition of: " ++ fromText txt))
      parseDefinition sym
    Just (IdentTag {}) ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Nothing -> do
      sym <- lift freshSymbol
      name <- lift $ freshName KNameFunction txt i
      let info =
            IdentifierInfo
              { _identifierName = Just name,
                _identifierSymbol = sym,
                _identifierType = mkDynamic',
                _identifierArgsNum = 0,
                _identifierArgsInfo = [],
                _identifierIsExported = False
              }
      lift $ registerIdent info
      void $ optional (parseDefinition sym)

parseDefinition ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Symbol ->
  ParsecS r ()
parseDefinition sym = do
  kwAssign
  node <- expression
  lift $ registerIdentNode sym node
  let (is, _) = unfoldLambdas node
  lift $ setIdentArgsInfo sym (map toArgumentInfo is)
  where
    toArgumentInfo :: Info -> ArgumentInfo
    toArgumentInfo i =
      ArgumentInfo
        { _argumentName = getInfoName bi,
          _argumentType = getInfoType bi,
          _argumentIsImplicit = False
        }
      where
        bi = getInfoBinder i

statementConstr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r ()
statementConstr = do
  kwConstr
  off <- P.getOffset
  (txt, i) <- identifierL
  (argsNum, _) <- number 0 128
  r <- lift (getIdent txt)
  case r of
    Just (IdentSym _) ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Just (IdentTag _) ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Nothing ->
      return ()
  tag <- lift freshTag
  sym <- lift freshSymbol
  name <- lift $ freshName KNameConstructor txt i
  let info =
        ConstructorInfo
          { _constructorName = name,
            _constructorTag = tag,
            _constructorType = mkDynamic',
            _constructorArgsNum = argsNum,
            _constructorInductive = sym
          }
  lift $ registerConstructor info

expression ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Node
expression = do
  node <- expr 0 mempty
  tab <- lift getInfoTable
  return $ etaExpandApps tab node

expr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  -- current de Bruijn index, i.e., the number of binders upwards
  Index ->
  -- reverse de Bruijn indices
  HashMap Text Index ->
  ParsecS r Node
expr varsNum vars = ioExpr varsNum vars

ioExpr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
ioExpr varsNum vars = cmpExpr varsNum vars >>= ioExpr' varsNum vars

ioExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
ioExpr' varsNum vars node = do
  bindExpr' varsNum vars node
    <|> seqExpr' varsNum vars node
    <|> return node

bindExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
bindExpr' varsNum vars node = do
  kwBind
  node' <- cmpExpr varsNum vars
  ioExpr' varsNum vars (mkConstr Info.empty (BuiltinTag TagBind) [node, node'])

seqExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
seqExpr' varsNum vars node = do
  ((), i) <- interval kwSeq
  node' <- cmpExpr (varsNum + 1) vars
  name <- lift $ freshName KNameLocal "_" i
  ioExpr' varsNum vars $
    mkConstr
      Info.empty
      (BuiltinTag TagBind)
      [node, mkLambda (binderNameInfo name) node']

cmpExpr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
cmpExpr varsNum vars = arithExpr varsNum vars >>= cmpExpr' varsNum vars

cmpExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
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
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
eqExpr' varsNum vars node = do
  kwEq
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpEq [node, node']

ltExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
ltExpr' varsNum vars node = do
  kwLt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node, node']

leExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
leExpr' varsNum vars node = do
  kwLe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node, node']

gtExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
gtExpr' varsNum vars node = do
  kwGt
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLt [node', node]

geExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
geExpr' varsNum vars node = do
  kwGe
  node' <- arithExpr varsNum vars
  return $ mkBuiltinApp' OpIntLe [node', node]

arithExpr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
arithExpr varsNum vars = factorExpr varsNum vars >>= arithExpr' varsNum vars

arithExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
arithExpr' varsNum vars node =
  plusExpr' varsNum vars node
    <|> minusExpr' varsNum vars node
    <|> return node

plusExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
plusExpr' varsNum vars node = do
  kwPlus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntAdd [node, node'])

minusExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
minusExpr' varsNum vars node = do
  kwMinus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (mkBuiltinApp' OpIntSub [node, node'])

factorExpr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
factorExpr varsNum vars = appExpr varsNum vars >>= factorExpr' varsNum vars

factorExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
factorExpr' varsNum vars node =
  mulExpr' varsNum vars node
    <|> divExpr' varsNum vars node
    <|> modExpr' varsNum vars node
    <|> return node

mulExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
mulExpr' varsNum vars node = do
  kwMul
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMul [node, node'])

divExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
divExpr' varsNum vars node = do
  kwDiv
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntDiv [node, node'])

modExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
modExpr' varsNum vars node = do
  kwMod
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (mkBuiltinApp' OpIntMod [node, node'])

appExpr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
appExpr varsNum vars = builtinAppExpr varsNum vars <|> atoms varsNum vars

builtinAppExpr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
builtinAppExpr varsNum vars = do
  op <-
    (kwEq >> return OpEq)
      <|> (kwLt >> return OpIntLt)
      <|> (kwLe >> return OpIntLe)
      <|> (kwPlus >> return OpIntAdd)
      <|> (kwMinus >> return OpIntSub)
      <|> (kwDiv >> return OpIntDiv)
      <|> (kwMul >> return OpIntMul)
      <|> (kwTrace >> return OpTrace)
      <|> (kwFail >> return OpFail)
  args <- P.many (atom varsNum vars)
  return $ mkBuiltinApp' op args

atoms ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
atoms varsNum vars = do
  es <- P.some (atom varsNum vars)
  return $ mkApps' (List.head es) (List.tail es)

atom ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
atom varsNum vars =
  exprNamed varsNum vars
    <|> exprConstInt
    <|> exprConstString
    <|> exprLambda varsNum vars
    <|> exprLetrecMany varsNum vars
    <|> exprLetrecOne varsNum vars
    <|> exprLet varsNum vars
    <|> exprCase varsNum vars
    <|> exprMatch varsNum vars
    <|> exprIf varsNum vars
    <|> parens (expr varsNum vars)
    <|> braces (expr varsNum vars)

exprNamed ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
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
        Just (IdentSym sym) -> do
          name <- lift $ freshName KNameFunction txt i
          return $ mkIdent (Info.singleton (NameInfo name)) sym
        Just (IdentTag tag) -> do
          name <- lift $ freshName KNameConstructor txt i
          return $ mkConstr (Info.singleton (NameInfo name)) tag []
        Nothing ->
          parseFailure off ("undeclared identifier: " ++ fromText txt)

exprConstInt ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Node
exprConstInt = P.try $ do
  (n, i) <- integer
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstInteger n)

exprConstString ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Node
exprConstString = P.try $ do
  (s, i) <- string
  return $ mkConstant (Info.singleton (LocationInfo i)) (ConstString s)

parseLocalName ::
  forall r.
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Name
parseLocalName = parseWildcardName <|> parseIdentName
  where
    parseWildcardName :: ParsecS r Name
    parseWildcardName = do
      ((), i) <- interval kwWildcard
      lift $ freshName KNameLocal "_" i

    parseIdentName :: ParsecS r Name
    parseIdentName = do
      (txt, i) <- identifierL
      lift $ freshName KNameLocal txt i

exprLambda ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprLambda varsNum vars = do
  kwLambda
  name <- parseLocalName
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
  body <- expr (varsNum + 1) vars'
  return $ mkLambda (binderNameInfo name) body

exprLetrecOne ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprLetrecOne varsNum vars = do
  kwLetRec
  name <- parseLocalName
  kwAssign
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
  value <- expr (varsNum + 1) vars'
  kwIn
  body <- expr (varsNum + 1) vars'
  return $ mkLetRec (Info.singleton (BindersInfo [Info.singleton (NameInfo name)])) (fromList [value]) body

exprLetrecMany ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprLetrecMany varsNum vars = do
  off <- P.getOffset
  defNames <- P.try (kwLetRec >> letrecNames)
  when (null defNames) $
    parseFailure off "expected at least one identifier name in letrec signature"
  let (vars', varsNum') = foldl' (\(vs, k) txt -> (HashMap.insert txt k vs, k + 1)) (vars, varsNum) defNames
  defs <- letrecDefs defNames varsNum' vars'
  body <- expr varsNum' vars'
  let infos = map (Info.singleton . NameInfo . fst) defs
  let values = map snd defs
  return $ mkLetRec (Info.singleton (BindersInfo infos)) (fromList values) body

letrecNames :: ParsecS r [Text]
letrecNames = P.between (symbol "[") (symbol "]") (P.many identifier)

letrecDefs ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  [Text] ->
  Index ->
  HashMap Text Index ->
  ParsecS r [(Name, Node)]
letrecDefs names varsNum vars = case names of
  [] -> return []
  n : names' -> do
    off <- P.getOffset
    (txt, i) <- identifierL
    when (n /= txt) $
      parseFailure off "identifier name doesn't match letrec signature"
    name <- lift $ freshName KNameLocal txt i
    kwAssign
    v <- expr varsNum vars
    if
        | null names' -> optional kwSemicolon >> kwIn
        | otherwise -> kwSemicolon
    rest <- letrecDefs names' varsNum vars
    return $ (name, v) : rest

letrecDef ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r (Name, Node)
letrecDef varsNum vars = do
  (txt, i) <- identifierL
  name <- lift $ freshName KNameLocal txt i
  kwAssign
  v <- expr varsNum vars
  return (name, v)

exprLet ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprLet varsNum vars = do
  kwLet
  name <- parseLocalName
  kwAssign
  value <- expr varsNum vars
  kwIn
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
  body <- expr (varsNum + 1) vars'
  return $ mkLet (binderNameInfo name) value body

exprCase ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprCase varsNum vars = do
  off <- P.getOffset
  kwCase
  value <- expr varsNum vars
  kwOf
  braces (exprCase' off value varsNum vars)
    <|> exprCase' off value varsNum vars

exprCase' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Int ->
  Node ->
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprCase' off value varsNum vars = do
  bs <- P.sepEndBy (caseBranchP varsNum vars) kwSemicolon
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
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r (Either CaseBranch Node)
caseBranchP varsNum vars =
  (caseDefaultBranch varsNum vars <&> Right)
    <|> (caseMatchingBranch varsNum vars <&> Left)

caseDefaultBranch ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
caseDefaultBranch varsNum vars = do
  kwWildcard
  kwAssign
  expr varsNum vars

caseMatchingBranch ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r CaseBranch
caseMatchingBranch varsNum vars = do
  off <- P.getOffset
  txt <- identifier
  r <- lift (getIdent txt)
  case r of
    Just (IdentSym {}) ->
      parseFailure off ("not a constructor: " ++ fromText txt)
    Just (IdentTag tag) -> do
      ns <- P.many parseLocalName
      let bindersNum = length ns
      ci <- lift $ getConstructorInfo tag
      when
        (ci ^. constructorArgsNum /= bindersNum)
        (parseFailure off "wrong number of constructor arguments")
      kwAssign
      let vars' =
            fst $
              foldl'
                ( \(vs, k) name ->
                    (HashMap.insert (name ^. nameText) k vs, k + 1)
                )
                (vars, varsNum)
                ns
      br <- expr (varsNum + bindersNum) vars'
      let info = setInfoName (ci ^. constructorName) $ setInfoBinders (map (Info.singleton . NameInfo) ns) Info.empty
      return $ CaseBranch info tag bindersNum br
    Nothing ->
      parseFailure off ("undeclared identifier: " ++ fromText txt)

exprIf ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprIf varsNum vars = do
  kwIf
  value <- expr varsNum vars
  kwThen
  br1 <- expr varsNum vars
  kwElse
  br2 <- expr varsNum vars
  return $ mkIf Info.empty value br1 br2

exprMatch ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprMatch varsNum vars = do
  kwMatch
  values <- P.sepBy (expr varsNum vars) kwComma
  kwWith
  braces (exprMatch' values varsNum vars)
    <|> exprMatch' values varsNum vars

exprMatch' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  [Node] ->
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprMatch' values varsNum vars = do
  bs <- P.sepEndBy (matchBranch (length values) varsNum vars) kwSemicolon
  return $ mkMatch' (fromList values) bs

matchBranch ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Int ->
  Index ->
  HashMap Text Index ->
  ParsecS r MatchBranch
matchBranch patsNum varsNum vars = do
  off <- P.getOffset
  pats <- P.sepBy branchPattern kwComma
  kwMapsTo
  unless (length pats == patsNum) $
    parseFailure off "wrong number of patterns"
  let pis = concatMap (reverse . getBinderPatternInfos) pats
  let (vars', varsNum') =
        foldl'
          ( \(vs, k) name ->
              (HashMap.insert (name ^. nameText) k vs, k + 1)
          )
          (vars, varsNum)
          (map (fromJust . getInfoName) pis)
  br <- expr varsNum' vars'
  return $ MatchBranch Info.empty (fromList pats) br

branchPattern ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Pattern
branchPattern =
  wildcardPattern
    <|> binderOrConstrPattern True
    <|> parens branchPattern

wildcardPattern :: ParsecS r Pattern
wildcardPattern = do
  kwWildcard
  return $ PatWildcard (PatternWildcard Info.empty)

binderOrConstrPattern ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Bool ->
  ParsecS r Pattern
binderOrConstrPattern parseArgs = do
  (txt, i) <- identifierL
  r <- lift (getIdent txt)
  case r of
    Just (IdentTag tag) -> do
      ps <- if parseArgs then P.many branchPattern else return []
      ci <- lift $ getConstructorInfo tag
      let info = setInfoName (ci ^. constructorName) Info.empty
      return $ PatConstr (PatternConstr info tag ps)
    _ -> do
      n <- lift $ freshName KNameLocal txt i
      mp <- optional binderPattern
      let pat = fromMaybe (PatWildcard (PatternWildcard Info.empty)) mp
      return $ PatBinder (PatternBinder (setInfoName n Info.empty) pat)

binderPattern ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Pattern
binderPattern = do
  kwAt
  wildcardPattern
    <|> binderOrConstrPattern False
    <|> parens branchPattern
