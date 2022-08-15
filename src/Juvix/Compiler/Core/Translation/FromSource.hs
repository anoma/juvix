module Juvix.Compiler.Core.Translation.FromSource where

import Control.Monad.Trans.Class (lift)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Info qualified as Info
import Juvix.Compiler.Core.Language.Info.BinderInfo as BinderInfo
import Juvix.Compiler.Core.Language.Info.BranchInfo as BranchInfo
import Juvix.Compiler.Core.Language.Info.LocationInfo as LocationInfo
import Juvix.Compiler.Core.Language.Info.NameInfo as NameInfo
import Juvix.Compiler.Core.Language.Type
import Juvix.Compiler.Core.Transformation.Eta
import Juvix.Compiler.Core.Translation.FromSource.Lexer
import Juvix.Parser.Error
import Text.Megaparsec qualified as P

parseText :: InfoTable -> Text -> Either ParserError (InfoTable, Maybe Node)
parseText = runParser "" ""

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
  name <- freshName KNameConstructor nameTxt i
  registerConstructor
    ( ConstructorInfo
        { _constructorName = name,
          _constructorTag = BuiltinTag btag,
          _constructorType = Star,
          _constructorArgsNum = builtinConstrArgsNum btag
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
  lift $ declareBuiltinConstr TagNil "nil" i
  lift $ declareBuiltinConstr TagCons "cons" i
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
    Just (Left sym) -> do
      guardSymbolNotDefined
        sym
        (parseFailure off ("duplicate definition of: " ++ fromText txt))
      parseDefinition sym
    Just (Right {}) ->
      parseFailure off ("duplicate identifier: " ++ fromText txt)
    Nothing -> do
      sym <- lift freshSymbol
      name <- lift $ freshName KNameFunction txt i
      let info =
            IdentInfo
              { _identName = name,
                _identSymbol = sym,
                _identType = Star,
                _identArgsNum = 0,
                _identArgsInfo = [],
                _identIsExported = False
              }
      lift $ registerIdent info
      void $ optional (parseDefinition sym)

parseDefinition ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Symbol ->
  ParsecS r ()
parseDefinition sym = do
  kwAssignment
  node <- expression
  lift $ registerIdentNode sym node
  let (is, _) = unfoldLambdas' node
  lift $ setIdentArgsInfo sym (map toArgumentInfo is)
  where
    toArgumentInfo :: Info -> ArgumentInfo
    toArgumentInfo i =
      case Info.lookup kBinderInfo i of
        Just bi ->
          ArgumentInfo
            { _argumentName = bi ^. BinderInfo.infoName,
              _argumentType = bi ^. BinderInfo.infoType,
              _argumentIsImplicit = False
            }
        Nothing -> error "missing binder info"

statementConstr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r ()
statementConstr = do
  kwConstr
  off <- P.getOffset
  (txt, i) <- identifierL
  (argsNum, _) <- number 0 128
  dupl <- lift (hasIdent txt)
  when
    dupl
    (parseFailure off ("duplicate identifier: " ++ fromText txt))
  tag <- lift freshTag
  name <- lift $ freshName KNameConstructor txt i
  let info =
        ConstructorInfo
          { _constructorName = name,
            _constructorTag = tag,
            _constructorType = Star,
            _constructorArgsNum = argsNum
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
expr varsNum vars = bindExpr varsNum vars

bindExpr ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
bindExpr varsNum vars = do
  node <- cmpExpr varsNum vars
  bindExpr' varsNum vars node <|> return node

bindExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
bindExpr' varsNum vars node = do
  kwBind
  node' <- bindExpr varsNum vars
  return $ ConstrApp Info.empty (BuiltinTag TagBind) [node, node']

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
  return $ BuiltinApp Info.empty OpIntEq [node, node']

ltExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
ltExpr' varsNum vars node = do
  kwLt
  node' <- arithExpr varsNum vars
  return $ BuiltinApp Info.empty OpIntLt [node, node']

leExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
leExpr' varsNum vars node = do
  kwLe
  node' <- arithExpr varsNum vars
  return $ BuiltinApp Info.empty OpIntLe [node, node']

gtExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
gtExpr' varsNum vars node = do
  kwGt
  node' <- arithExpr varsNum vars
  return $ BuiltinApp Info.empty OpIntLt [node', node]

geExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
geExpr' varsNum vars node = do
  kwGe
  node' <- arithExpr varsNum vars
  return $ BuiltinApp Info.empty OpIntLe [node', node]

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
  arithExpr' varsNum vars (BuiltinApp Info.empty OpIntAdd [node, node'])

minusExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
minusExpr' varsNum vars node = do
  kwMinus
  node' <- factorExpr varsNum vars
  arithExpr' varsNum vars (BuiltinApp Info.empty OpIntSub [node, node'])

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
  factorExpr' varsNum vars (BuiltinApp Info.empty OpIntMul [node, node'])

divExpr' ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  Node ->
  ParsecS r Node
divExpr' varsNum vars node = do
  kwDiv
  node' <- appExpr varsNum vars
  factorExpr' varsNum vars (BuiltinApp Info.empty OpIntDiv [node, node'])

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
    (kwEq >> return OpIntEq)
      <|> (kwLt >> return OpIntLt)
      <|> (kwLe >> return OpIntLe)
      <|> (kwPlus >> return OpIntAdd)
      <|> (kwMinus >> return OpIntSub)
      <|> (kwDiv >> return OpIntDiv)
      <|> (kwMul >> return OpIntMul)
  args <- P.many (atom varsNum vars)
  return $ BuiltinApp Info.empty op args

atoms ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
atoms varsNum vars = do
  es <- P.some (atom varsNum vars)
  return $ mkApp (List.head es) (List.tail es)

atom ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
atom varsNum vars =
  exprNamed varsNum vars
    <|> exprConstInt
    <|> exprConstBool
    <|> exprConstString
    <|> exprLambda varsNum vars
    <|> exprLet varsNum vars
    <|> exprCase varsNum vars
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
      return $ Var (Info.singleton (NameInfo name)) (varsNum - k - 1)
    Nothing -> do
      r <- lift (getIdent txt)
      case r of
        Just (Left sym) -> do
          name <- lift $ freshName KNameFunction txt i
          return $ Ident (Info.singleton (NameInfo name)) sym
        Just (Right tag) -> do
          name <- lift $ freshName KNameConstructor txt i
          return $ ConstrApp (Info.singleton (NameInfo name)) tag []
        Nothing ->
          parseFailure off ("undeclared identifier: " ++ fromText txt)

exprConstInt ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Node
exprConstInt = P.try $ do
  (n, i) <- integer
  return $ Constant (Info.singleton (LocationInfo i)) (ConstInteger n)

exprConstBool ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Node
exprConstBool = P.try $ do
  (b, i) <- boolean
  return $ Constant (Info.singleton (LocationInfo i)) (ConstBool b)

exprConstString ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Node
exprConstString = P.try $ do
  (s, i) <- string
  return $ Constant (Info.singleton (LocationInfo i)) (ConstString s)

parseLocalName ::
  forall r.
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  ParsecS r Name
parseLocalName =
  parseWildcardName <|> do
    (txt, i) <- identifierL
    lift $ freshName KNameLocal txt i
  where
    parseWildcardName :: ParsecS r Name
    parseWildcardName = do
      ((), i) <- interval kwWildcard
      lift $ freshName KNameLocal "_" i

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
  return $ Lambda (Info.singleton (BinderInfo name Star)) body

exprLet ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
exprLet varsNum vars = do
  kwLet
  name <- parseLocalName
  kwAssignment
  value <- expr varsNum vars
  kwIn
  let vars' = HashMap.insert (name ^. nameText) varsNum vars
  body <- expr (varsNum + 1) vars'
  return $ Let (Info.singleton (BinderInfo name Star)) value body

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
  bs <- P.sepEndBy (caseBranch varsNum vars) kwSemicolon
  rbrace
  let bs' = map fromLeft' $ filter isLeft bs
  let bss = map fst bs'
  let bsns = map snd bs'
  let def' = map fromRight' $ filter isRight bs
  let bi = CaseBinderInfo $ map (map (`BinderInfo` Star)) bsns
  bri <-
    CaseBranchInfo
      <$> mapM
        ( \(CaseBranch tag _ _) -> do
            ci <- lift $ getConstructorInfo tag
            return $ BranchInfo (ci ^. constructorName)
        )
        bss
  let info = Info.insert bri (Info.singleton bi)
  case def' of
    [def] ->
      return $ Case info value bss (Just def)
    [] ->
      return $ Case info value bss Nothing
    _ ->
      parseFailure off "multiple default branches"

caseBranch ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r (Either (CaseBranch, [Name]) Node)
caseBranch varsNum vars =
  (defaultBranch varsNum vars <&> Right)
    <|> (matchingBranch varsNum vars <&> Left)

defaultBranch ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r Node
defaultBranch varsNum vars = do
  kwWildcard
  kwMapsTo
  expr varsNum vars

matchingBranch ::
  Members '[Reader ParserParams, InfoTableBuilder, NameIdGen] r =>
  Index ->
  HashMap Text Index ->
  ParsecS r (CaseBranch, [Name])
matchingBranch varsNum vars = do
  off <- P.getOffset
  txt <- identifier
  r <- lift (getIdent txt)
  case r of
    Just (Left {}) ->
      parseFailure off ("not a constructor: " ++ fromText txt)
    Just (Right tag) -> do
      ns <- P.many parseLocalName
      let bindersNum = length ns
      ci <- lift $ getConstructorInfo tag
      when
        (ci ^. constructorArgsNum /= bindersNum)
        (parseFailure off "wrong number of constructor arguments")
      kwMapsTo
      let vars' =
            fst $
              foldl'
                ( \(vs, k) name ->
                    (HashMap.insert (name ^. nameText) k vs, k + 1)
                )
                (vars, varsNum)
                ns
      br <- expr (varsNum + bindersNum) vars'
      return (CaseBranch tag bindersNum br, ns)
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
  return $ If Info.empty value br1 br2
