{-# LANGUAGE ConstraintKinds #-}
module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (
  module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base,
  module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann
                                                    ) where

import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Prelude
import qualified Data.List.NonEmpty.Extra as NonEmpty
import Prettyprinter hiding (braces, parens)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann

data Options = Options
  { _optOptimizeParens :: Bool,
    _optShowNameId :: Bool,
    _optInlineImports :: Bool,
    _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optOptimizeParens = True,
      _optShowNameId = False,
      _optInlineImports = False,
      _optIndent = 2
    }

type IsStage s = (SingI s, PrettyCode (ExpressionType s), Eq (SymbolType s), HasFixity (ExpressionType s))

-- | Pretty prints a top module.
prettyTopModule :: IsStage s => Options -> Module s 'ModuleTop -> Doc Ann
prettyTopModule opts = run . runReader opts . ppModule

infixl 7 <+?>
(<+?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<+?>) a = maybe a (a <+>)

infixl 7 <?>
(<?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<?>) a = maybe a (a <>)

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

delimiter :: Text -> Doc Ann
delimiter = annotate AnnDelimiter . pretty

kwModule :: Doc Ann
kwModule = keyword "module"

kwEnd :: Doc Ann
kwEnd = keyword "end"

kwInductive :: Doc Ann
kwInductive = keyword "inductive"

kwType :: Doc Ann
kwType = keyword "Type"

kwColon :: Doc Ann
kwColon = keyword ":"

kwArrowR :: Doc Ann
kwArrowR = keyword "→"

kwMatch :: Doc Ann
kwMatch = keyword "match"

kwLambda :: Doc Ann
kwLambda = keyword "λ"

kwWhere :: Doc Ann
kwWhere = keyword "where"

kwLet :: Doc Ann
kwLet = keyword "let"

kwIn :: Doc Ann
kwIn = keyword "in"

kwPublic :: Doc Ann
kwPublic = keyword "public"

kwWildcard :: Doc Ann
kwWildcard = keyword "_"

kwPostfix :: Doc Ann
kwPostfix = keyword "postfix"

kwInfixr :: Doc Ann
kwInfixr = keyword "infixr"

kwInfixl :: Doc Ann
kwInfixl = keyword "infixl"

kwInfix :: Doc Ann
kwInfix = keyword "infix"

kwAssignment :: Doc Ann
kwAssignment = keyword "≔"

kwMapsto :: Doc Ann
kwMapsto = keyword "↦"

kwColonZero :: Doc Ann
kwColonZero = keyword ":0"

kwColonOne :: Doc Ann
kwColonOne = keyword ":1"

kwColonOmega :: Doc Ann
kwColonOmega = keyword ":ω"

kwAxiom :: Doc Ann
kwAxiom = keyword "axiom"

kwEval :: Doc Ann
kwEval = keyword "eval"

kwPrint :: Doc Ann
kwPrint = keyword "print"

kwOpen :: Doc Ann
kwOpen = keyword "open"

kwUsing :: Doc Ann
kwUsing = keyword "using"

kwHiding :: Doc Ann
kwHiding = keyword "hiding"

kwImport :: Doc Ann
kwImport = keyword "import"

kwSemicolon :: Doc Ann
kwSemicolon = delimiter ";"

kwBraceL :: Doc Ann
kwBraceL = delimiter "{"

kwBraceR :: Doc Ann
kwBraceR = delimiter "}"

kwParenL :: Doc Ann
kwParenL = delimiter "("

kwParenR :: Doc Ann
kwParenR = delimiter ")"

kwDot :: Doc Ann
kwDot = delimiter "."

indented :: Members '[Reader Options] r => Doc Ann -> Sem r (Doc Ann)
indented d = do
  ind <- asks _optIndent
  return (indent ind d)

bracesIndent :: Members '[Reader Options] r => Doc Ann -> Sem r (Doc Ann)
bracesIndent d = do
  d' <- indented d
  return $ braces (line <> d' <> line)

braces :: Doc Ann -> Doc Ann
braces = enclose kwBraceL kwBraceR

parens :: Doc Ann -> Doc Ann
parens = enclose kwParenL kwParenR

ppModulePathType :: forall t s r. (SingI t, IsStage s, Members '[Reader Options] r) =>
  ModulePathType s t -> Sem r (Doc Ann)
ppModulePathType x = case sing :: SModuleIsTop t of
  SModuleTop -> case sing :: SStage s of
    SParsed -> ppCTopModulePath x
    SScoped -> annSDef x <$> ppTopModulePath x
  SModuleLocal -> case sing :: SStage s of
    SParsed -> ppCSymbol x
    SScoped -> ppSSymbol x

ppUnkindedSymbol :: Members '[Reader Options] r => Symbol -> Sem r (Doc Ann)
ppUnkindedSymbol = fmap (annotate AnnUnkindedSym) . ppSymbol

ppSymbol :: forall s r. (SingI s, Members '[Reader Options] r) => SymbolType s -> Sem r (Doc Ann)
ppSymbol = case sing :: SStage s of
  SParsed -> ppCSymbol
  SScoped -> ppSSymbol

ppCSymbol :: Members '[Reader Options] r => Symbol -> Sem r (Doc Ann)
ppCSymbol Symbol {..} = return (pretty _symbolText)

groupStatements :: forall s. IsStage s => [Statement s] -> [[Statement s]]
groupStatements = reverse . map reverse . uncurry cons . foldl' aux ([], [])
  where
  aux :: ([Statement s], [[Statement s]]) -> Statement s
      -> ([Statement s], [[Statement s]])
  aux ([], acc) s = ([s], acc)
  aux (gr@(a : _), acc) b
    | g a b = (b : gr, acc)
    | otherwise = ([b], gr : acc)
  -- | Decides if statements a and b should be next to each other without a
  -- blank line
  g :: Statement s -> Statement s -> Bool
  g a b = case (a, b) of
    (StatementOperator _, StatementOperator _) -> True
    (StatementOperator o, s) -> definesSymbol (opSymbol o) s
    (StatementImport _, StatementImport _) -> True
    (StatementImport i, StatementOpenModule o) -> case sing :: SStage s of
      SParsed -> True
      SScoped -> S._nameId (modulePath (importModule i)) == S._nameId (openModuleName o)
    (StatementImport _, _) -> False
    (StatementOpenModule {}, StatementOpenModule {}) -> True
    (StatementOpenModule {}, _) -> False
    (StatementInductive {}, _) -> False
    (StatementModule {}, _) -> False
    (StatementAxiom {}, StatementAxiom {}) -> True
    (StatementAxiom {}, _) -> False
    (StatementEval {}, StatementEval {}) -> True
    (StatementEval {}, _) -> False
    (StatementPrint {}, StatementPrint {}) -> True
    (StatementPrint {}, _) -> False
    (StatementTypeSignature sig, StatementFunctionClause fun) ->
      sigName sig == clauseOwnerFunction fun
    (StatementTypeSignature {}, _) -> False
    (StatementFunctionClause fun1, StatementFunctionClause fun2) ->
      clauseOwnerFunction fun1 == clauseOwnerFunction fun2
    (StatementFunctionClause {}, _) -> False
  definesSymbol :: Symbol -> Statement s -> Bool
  definesSymbol n s = case s of
    StatementTypeSignature sig ->
      let sym = case sing :: SStage s of
            SParsed -> sigName sig
            SScoped -> S._nameConcrete $ sigName sig
      in n == sym
    StatementInductive d -> n `elem` syms d
    _ -> False
    where
    syms :: InductiveDef s -> [Symbol]
    syms InductiveDef {..} = case sing :: SStage s of
      SParsed -> inductiveName : map constructorName inductiveConstructors
      SScoped -> S._nameConcrete inductiveName :
          map (S._nameConcrete . constructorName) inductiveConstructors


ppStatements :: (IsStage s, Members '[Reader Options] r)
   => [Statement s] -> Sem r (Doc Ann)
ppStatements ss = joinGroups <$> mapM (fmap mkGroup . mapM (fmap endSemicolon . ppStatement)) (groupStatements ss)
  where
    mkGroup = vsep
    joinGroups = concatWith (\a b -> a <> line <> line <> b)

ppStatement :: (IsStage s, Members '[Reader Options] r) => Statement s -> Sem r (Doc Ann)
ppStatement s = case s of
  StatementOperator op -> ppOperatorSyntaxDef op
  StatementTypeSignature sig -> ppTypeSignature sig
  StatementImport i -> ppImport i
  StatementInductive d -> ppInductiveDef d
  StatementModule m -> ppModule m
  StatementOpenModule o -> ppOpen o
  StatementFunctionClause c -> ppFunctionClause c
  StatementAxiom a -> ppAxiom a
  StatementEval e -> ppEval e
  StatementPrint p -> ppPrint p

ppTopModulePath :: forall s r. (SingI s, Members '[Reader Options] r) =>
  ModulePathType s 'ModuleTop -> Sem r (Doc Ann)
ppTopModulePath = case sing :: SStage s of
  SParsed -> ppCTopModulePath
  SScoped -> ppSTopModulePath

ppCTopModulePath :: Members '[Reader Options] r => TopModulePath -> Sem r (Doc Ann)
ppCTopModulePath TopModulePath {..} =
  dotted <$> mapM ppSymbol (modulePathDir ++ [modulePathName])

ppSTopModulePath :: Members '[Reader Options] r => S.TopModulePath -> Sem r (Doc Ann)
ppSTopModulePath = ppSName' ppTopModulePath

endSemicolon :: Doc Ann -> Doc Ann
endSemicolon x = x <> kwSemicolon

ppInductiveParameters :: (IsStage s, Members '[Reader Options] r)
    => [InductiveParameter s] -> Sem r (Maybe (Doc Ann))
ppInductiveParameters =
  fmap (fmap (hsep . toList) . nonEmpty) . mapM ppInductiveParameter

ppModule :: forall t r s. (SingI t, IsStage s, Members '[Reader Options] r)
   => Module s t -> Sem r (Doc Ann)
ppModule Module {..} = do
  moduleBody' <- ppStatements moduleBody >>= indented
  modulePath' <- ppModulePathType modulePath
  moduleParameters' <- ppInductiveParameters moduleParameters
  return $
    kwModule <+> modulePath' <+?> moduleParameters' <> kwSemicolon <> line
      <> moduleBody'
      <> line
      <> kwEnd
      <?> lastSemicolon
  where
    lastSemicolon = case sing :: SModuleIsTop t of
      SModuleLocal -> Nothing
      SModuleTop -> Just kwSemicolon

ppPrecedence :: Precedence -> Doc Ann
ppPrecedence p = annotate AnnNumber $ case p of
  PrecMinusOmega -> pretty ("-ω" :: Text)
  PrecNat n -> pretty n
  PrecOmega -> pretty ("ω" :: Text)

ppOperatorSyntaxDef :: Members '[Reader Options] r => OperatorSyntaxDef -> Sem r (Doc Ann)
ppOperatorSyntaxDef OperatorSyntaxDef {..} = do
  opSymbol' <- ppUnkindedSymbol opSymbol
  return $ ppFixity opFixity <+> opSymbol'
  where
    ppFixity :: Fixity -> Doc Ann
    ppFixity Fixity {..} =
      ppArity <+> ppPrecedence fixityPrecedence
      where
        ppArity :: Doc Ann
        ppArity = case fixityArity of
          Unary {} -> kwPostfix
          Binary p -> case p of
            AssocRight -> kwInfixr
            AssocLeft -> kwInfixl
            AssocNone -> kwInfix

ppInductiveConstructorDef :: (IsStage s, Members '[Reader Options] r)
   => InductiveConstructorDef s -> Sem r (Doc Ann)
ppInductiveConstructorDef InductiveConstructorDef {..} = do
  constructorName' <- annDef constructorName <$> ppSymbol constructorName
  constructorType' <- ppExpression constructorType
  return $ constructorName' <+> kwColon <+> constructorType'

ppInductiveDef :: forall r s. (IsStage s, Members '[Reader Options] r) =>
   InductiveDef s -> Sem r (Doc Ann)
ppInductiveDef InductiveDef {..} = do
  inductiveName' <- annDef inductiveName <$> ppSymbol inductiveName
  inductiveParameters' <- ppInductiveParameters inductiveParameters
  inductiveType' <- ppTypeType
  inductiveConstructors' <- ppBlock ppInductiveConstructorDef inductiveConstructors
  return $
    kwInductive <+> inductiveName' <+?> inductiveParameters' <+?> inductiveType'
      <+> inductiveConstructors'
  where
    ppTypeType :: Sem r (Maybe (Doc Ann))
    ppTypeType = case inductiveType of
      Nothing -> return Nothing
      Just e -> Just . (kwColon <+>) <$> ppExpression e

ppInductiveParameter :: (IsStage s, Members '[Reader Options] r) => InductiveParameter s -> Sem r (Doc Ann)
ppInductiveParameter InductiveParameter {..} = do
  inductiveParameterName' <- annDef inductiveParameterName <$> ppSymbol inductiveParameterName
  inductiveParameterType' <- ppExpression inductiveParameterType
  return $ parens (inductiveParameterName' <+> kwColon <+> inductiveParameterType')

dotted :: Foldable f => f (Doc Ann) -> Doc Ann
dotted = concatWith (surround kwDot)

ppQualified :: Members '[Reader Options] r => QualifiedName -> Sem r (Doc Ann)
ppQualified QualifiedName {..} = do
  let symbols = pathParts qualifiedPath NonEmpty.|> qualifiedSymbol
  dotted <$> mapM ppSymbol symbols

ppName :: forall s r. (SingI s, Members '[Reader Options] r) => NameType s -> Sem r (Doc Ann)
ppName = case sing :: SStage s of
  SParsed -> ppCName
  SScoped -> ppSName

ppCName :: Members '[Reader Options] r => Name -> Sem r (Doc Ann)
ppCName n = case n of
  NameUnqualified s -> ppSymbol s
  NameQualified s -> ppQualified s

ppNameId :: S.NameId -> Doc Ann
ppNameId (S.NameId k) = pretty k

ppSSymbol :: Members '[Reader Options] r => S.Symbol -> Sem r (Doc Ann)
ppSSymbol = ppSName' ppSymbol

annDef :: forall s. IsStage s => SymbolType s -> Doc Ann -> Doc Ann
annDef nm = case sing :: SStage s of
  SScoped -> annSDef nm
  SParsed -> id

annSDef :: S.Name' n -> Doc Ann -> Doc Ann
annSDef nm = annotate (AnnDef (S.absTopModulePath (S._nameDefinedIn nm)) (S._nameId nm))

annSRef :: S.Name' n -> Doc Ann -> Doc Ann
annSRef nm = annotate (AnnRef (S.absTopModulePath (S._nameDefinedIn nm)) (S._nameId nm))

annRef :: forall s. IsStage s => SymbolType s -> Doc Ann -> Doc Ann
annRef nm = case sing :: SStage s of
  SParsed -> id
  SScoped -> annSRef nm

ppSName :: Members '[Reader Options] r => S.Name -> Sem r (Doc Ann)
ppSName nm = annSRef nm <$> ppSName' ppName nm

ppSName' :: Members '[Reader Options] r => (s -> Sem r (Doc Ann)) -> S.Name' s -> Sem r (Doc Ann)
ppSName' ppConcrete S.Name' {..} = do
  nameConcrete' <- annotate (AnnKind _nameKind) <$> ppConcrete _nameConcrete
  showNameId <- asks _optShowNameId
  let uid = if showNameId then "@" <> ppNameId _nameId else mempty
  return $ nameConcrete' <> uid

ppAtom :: (IsStage s, Members '[Reader Options] r) => ExpressionType s -> Sem r (Doc Ann)
ppAtom e = parensCond (isAtomic e) <$> ppExpression e

ppOpen :: forall s r. (IsStage s, Members '[Reader Options] r)
    => OpenModule s -> Sem r (Doc Ann)
ppOpen OpenModule {..} = do
  openModuleName' <- ppName openModuleName
  openUsingHiding' <- sequence $ ppUsingHiding <$> openUsingHiding
  openParameters' <- ppOpenParams
  let openPublic' = ppPublic
  return $ keyword "open" <+> openModuleName' <+?> openParameters' <+?> openUsingHiding' <+?> openPublic'
  where
  ppOpenParams :: Sem r (Maybe (Doc Ann))
  ppOpenParams = case openParameters of
    [] -> return Nothing
    _ -> Just . hsep <$> mapM ppAtom openParameters
  ppUsingHiding :: UsingHiding -> Sem r (Doc Ann)
  ppUsingHiding uh = do
    bracedList <- encloseSep kwBraceL kwBraceR kwSemicolon . toList
      <$> mapM ppUnkindedSymbol syms
    return $ kw <+> bracedList
    where
      (kw, syms) = case uh of
        Using s -> (kwUsing, s)
        Hiding s -> (kwHiding, s)
  ppPublic :: Maybe (Doc Ann)
  ppPublic = case openPublic of
    Public -> Just kwPublic
    NoPublic -> Nothing

ppTypeSignature :: (IsStage s, Members '[Reader Options] r) => TypeSignature s -> Sem r (Doc Ann)
ppTypeSignature TypeSignature {..} = do
  sigName' <- annDef sigName <$> ppSymbol sigName
  sigType' <- ppExpression sigType
  return $ sigName' <+> kwColon <+> sigType'

ppFunction :: forall s r. (IsStage s, Members '[Reader Options] r) => Function s -> Sem r (Doc Ann)
ppFunction Function {..} = do
  funParameter' <- ppFunParameter funParameter
  funReturn' <- ppRightExpression funFixity funReturn
  return $ funParameter' <+> kwArrowR <+> funReturn'
  where
    ppUsage :: Maybe Usage -> Doc Ann
    ppUsage m = case m of
      Nothing -> kwColon
      Just u -> case u of
        UsageNone -> kwColonZero
        UsageOnce -> kwColonOne
        UsageOmega -> kwColonOmega
    ppFunParameter :: FunctionParameter s -> Sem r (Doc Ann)
    ppFunParameter FunctionParameter {..} = do
      case paramName of
        Nothing -> ppLeftExpression funFixity paramType
        Just n -> do
          paramName' <- annDef n <$> ppSymbol n
          paramType' <- ppExpression paramType
          return $ parens (paramName' <+> ppUsage paramUsage <+> paramType')

ppUniverse :: Members '[Reader Options] r => Universe -> Sem r (Doc Ann)
ppUniverse (Universe n) = return $ kwType <+?> (pretty <$> n)

ppLetBlock :: forall s r. (IsStage s, Members '[Reader Options] r) => LetBlock s -> Sem r (Doc Ann)
ppLetBlock LetBlock {..} = do
  letClauses' <- ppBlock ppLetClause letClauses
  letExpression' <- ppExpression letExpression
  return $ kwLet <+> letClauses' <+> kwIn <+> letExpression'
  where
    ppLetClause :: LetClause s -> Sem r (Doc Ann)
    ppLetClause c = case c of
      LetTypeSig sig -> ppTypeSignature sig
      LetFunClause cl -> ppFunctionClause cl

ppBlock :: Members '[Reader Options] r => (a -> Sem r (Doc Ann)) -> [a] -> Sem r (Doc Ann)
ppBlock ppItem items = mapM (fmap endSemicolon . ppItem) items >>= bracesIndent . vsep

ppMatch :: forall s r. (IsStage s, Members '[Reader Options] r) => Match s -> Sem r (Doc Ann)
ppMatch Match {..} = do
  matchExpression' <- ppExpression matchExpression
  matchAlts' <- ppBlock ppMatchAlt matchAlts
  return $ kwMatch <+> matchExpression' <+> matchAlts'
  where
    ppMatchAlt :: MatchAlt s -> Sem r (Doc Ann)
    ppMatchAlt MatchAlt {..} = do
      matchAltPattern' <- ppPattern matchAltPattern
      matchAltBody' <- ppExpression matchAltBody
      return $ matchAltPattern' <+> kwMapsto <+> matchAltBody'

ppLambda :: forall r s. (IsStage s, Members '[Reader Options] r) => Lambda s -> Sem r (Doc Ann)
ppLambda Lambda {..} = do
  lambdaClauses' <- ppBlock ppLambdaClause lambdaClauses
  return $ kwLambda <+> lambdaClauses'
  where
    ppLambdaClause :: LambdaClause s -> Sem r (Doc Ann)
    ppLambdaClause LambdaClause {..} = do
      lambdaParameters' <- hsep . toList <$> mapM ppPattern lambdaParameters
      lambdaBody' <- ppExpression lambdaBody
      return $ lambdaParameters' <+> kwMapsto <+> lambdaBody'

ppFunctionClause :: forall r s. (IsStage s, Members '[Reader Options] r)
   => FunctionClause s -> Sem r (Doc Ann)
ppFunctionClause FunctionClause {..} = do
  clauseOwnerFunction' <- annRef clauseOwnerFunction <$> ppSymbol clauseOwnerFunction
  clausePatterns' <- case nonEmpty clausePatterns of
    Nothing -> return Nothing
    Just ne -> Just . hsep . toList <$> mapM ppPattern ne
  clauseBody' <- ppExpression clauseBody
  clauseWhere' <- sequence (ppWhereBlock <$> clauseWhere)
  return $
    clauseOwnerFunction' <+?> clausePatterns' <+> kwAssignment <+> clauseBody'
      <+?> ((line <>) <$> clauseWhere')
  where
    ppWhereBlock :: WhereBlock s -> Sem r (Doc Ann)
    ppWhereBlock WhereBlock {..} =
      ppBlock ppWhereClause whereClauses >>= indented . (kwWhere <+>)
      where
        ppWhereClause :: WhereClause s -> Sem r (Doc Ann)
        ppWhereClause c = case c of
          WhereOpenModule o -> ppOpen o
          WhereTypeSig sig -> ppTypeSignature sig
          WhereFunClause fun -> ppFunctionClause fun

ppAxiom :: (IsStage s, Members '[Reader Options] r) => AxiomDef s -> Sem r (Doc Ann)
ppAxiom AxiomDef {..} = do
  axiomName' <- ppSymbol axiomName
  axiomType' <- ppExpression axiomType
  return $ kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

ppEval :: (IsStage s, Members '[Reader Options] r) => Eval s -> Sem r (Doc Ann)
ppEval (Eval p) = do
  p' <- ppExpression p
  return $ kwEval <+> p'

ppPrint :: (IsStage s, Members '[Reader Options] r) => Print s -> Sem r (Doc Ann)
ppPrint (Print p) = do
  p' <- ppExpression p
  return $ kwPrint <+> p'

ppImport :: forall r s. (IsStage s, Members '[Reader Options] r) => Import s -> Sem r (Doc Ann)
ppImport (Import m) = do
  modulePath' <- ppModulePath
  inlineImport' <- inlineImport
  return $ kwImport <+> modulePath' <+?> inlineImport'
  where
  ppModulePath = case sing :: SStage s of
    SParsed -> ppCTopModulePath m
    SScoped -> annSRef (modulePath m) <$> ppTopModulePath (modulePath m)
  jumpLines :: Doc Ann -> Doc Ann
  jumpLines x = line <> x <> line
  inlineImport :: Sem r (Maybe (Doc Ann))
  inlineImport = do
    b <- asks _optInlineImports
    if b then case sing :: SStage s of
      SParsed -> return Nothing
      SScoped -> ppModule m >>= fmap (Just . braces . jumpLines) . indented
      else return Nothing

ppPattern :: forall s r. (IsStage s, Members '[Reader Options] r) => PatternType s -> Sem r (Doc Ann)
ppPattern = case sing :: SStage s of
  SParsed -> ppCPattern
  SScoped -> ppSPattern

ppCPattern :: forall r. Members '[Reader Options] r => PatternAtom 'Parsed -> Sem r (Doc Ann)
ppCPattern a = case a of
  PatternAtomName n -> ppName n
  PatternAtomWildcard -> return kwWildcard
  PatternAtomEmpty -> return $ parens mempty
  PatternAtomParens p -> parens <$> ppCPatterns p

ppCPatterns :: forall r. Members '[Reader Options] r => PatternAtoms 'Parsed -> Sem r (Doc Ann)
ppCPatterns (PatternAtoms ps) = hsep . toList <$> mapM ppCPattern ps

ppSPattern :: forall r. Members '[Reader Options] r => Pattern -> Sem r (Doc Ann)
ppSPattern pat = do
  p' <- ppNestedPattern pat
  return $ if isAtomicPat pat then p' else parens p'
  where
  isAtomicPat :: Pattern -> Bool
  isAtomicPat p = case p of
    PatternVariable {} -> True
    PatternApplication {} -> False
    PatternConstructor {} -> True
    PatternInfixApplication {} -> False
    PatternPostfixApplication {} -> False
    PatternWildcard -> True
    PatternEmpty -> True


ppNestedPattern :: forall r. Members '[Reader Options] r => Pattern -> Sem r (Doc Ann)
ppNestedPattern = go
  where
    go :: Pattern -> Sem r (Doc Ann)
    go p = case p of
      PatternVariable v -> annDef v <$> ppSSymbol v
      PatternApplication l r -> do
        l' <- ppLeftExpression appFixity l
        r' <- ppRightExpression appFixity r
        return $ l' <+> r'
      PatternWildcard -> return kwWildcard
      PatternEmpty -> return $ parens mempty
      PatternConstructor constr -> ppSName constr
      PatternInfixApplication i -> ppPatternInfixApp i
      PatternPostfixApplication i -> ppPatternPostfixApp i

    ppPatternInfixApp :: PatternInfixApp -> Sem r (Doc Ann)
    ppPatternInfixApp p@PatternInfixApp {..} = do
      patInfixConstructor' <- ppSName patInfixConstructor
      patInfixLeft' <- ppLeftExpression (pinfixFixity p) patInfixLeft
      patInfixRight' <- ppRightExpression (pinfixFixity p) patInfixRight
      return $ patInfixLeft' <+> patInfixConstructor' <+> patInfixRight'

    ppPatternPostfixApp :: PatternPostfixApp -> Sem r (Doc Ann)
    ppPatternPostfixApp p@PatternPostfixApp {..} = do
      patPostfixConstructor' <- ppSName patPostfixConstructor
      patPostfixParameter' <- ppLeftExpression (ppostfixFixity p) patPostfixParameter
      return $ patPostfixParameter' <+> patPostfixConstructor'

isAtomic :: forall s. IsStage s => ExpressionType s -> Bool
isAtomic e = case sing :: SStage s of
  SScoped -> case e of
    ExpressionIdentifier {} -> True
    ExpressionParensIdentifier {} -> True
    ExpressionApplication {} -> False
    ExpressionInfixApplication {} -> False
    ExpressionPostfixApplication {} -> False
    ExpressionLambda {} -> True
    ExpressionMatch {} -> True
    ExpressionLetBlock {} -> True
    ExpressionUniverse {} -> True
    ExpressionFunction {} -> False
  SParsed -> case e of
    ExpressionAtoms (_  :| []) -> True
    ExpressionAtoms (_  :| _) -> False

ppInfixApplication :: forall r. Members '[Reader Options] r => InfixApplication -> Sem r (Doc Ann)
ppInfixApplication i@InfixApplication {..} = do
  infixAppLeft' <- ppLeftExpression (infixFixity i) infixAppLeft
  infixAppOperator' <- ppSName infixAppOperator
  infixAppRight' <- ppRightExpression (infixFixity i) infixAppRight
  return $ infixAppLeft' <+> infixAppOperator' <+> infixAppRight'

ppPostfixApplication :: forall r. Members '[Reader Options] r => PostfixApplication -> Sem r (Doc Ann)
ppPostfixApplication i@PostfixApplication {..} = do
  postfixAppParameter' <- ppPostExpression (postfixFixity i) postfixAppParameter
  postfixAppOperator' <- ppSName postfixAppOperator
  return $ postfixAppParameter' <+> postfixAppOperator'

class PrettyCode a where
  ppCode :: forall r. Members '[Reader Options] r => a -> Sem r (Doc Ann)

instance PrettyCode Expression where
  ppCode = ppExpression

instance PrettyCode Pattern where
  ppCode = ppNestedPattern

instance PrettyCode (ExpressionAtoms 'Parsed) where
  ppCode = goAtoms

class HasFixity a where
  getFixity :: a -> Maybe Fixity

instance HasFixity Expression where
 getFixity e = case e of
    ExpressionIdentifier {} -> Nothing
    ExpressionParensIdentifier {} -> Nothing
    ExpressionApplication {} -> Just appFixity
    ExpressionInfixApplication a -> Just (infixFixity a)
    ExpressionPostfixApplication a -> Just (postfixFixity a)
    ExpressionLambda {} -> Nothing
    ExpressionMatch {} -> Nothing
    ExpressionLetBlock {} -> Nothing
    ExpressionUniverse {} -> Nothing
    ExpressionFunction {} -> Just funFixity

instance HasFixity Pattern where
 getFixity e = case e of
    PatternVariable {} -> Nothing
    PatternConstructor {} -> Nothing
    PatternApplication {} -> Just appFixity
    PatternInfixApplication a -> Just (pinfixFixity a)
    PatternPostfixApplication p -> Just (ppostfixFixity p)
    PatternWildcard -> Nothing
    PatternEmpty -> Nothing

-- TODO never used.
instance HasFixity (ExpressionAtoms 'Parsed) where
  getFixity = const Nothing

pinfixFixity :: PatternInfixApp -> Fixity
pinfixFixity (PatternInfixApp  _ op _) = case S._nameFixity op of
    S.NoFixity -> impossible
    S.SomeFixity s -> s

ppostfixFixity :: PatternPostfixApp -> Fixity
ppostfixFixity (PatternPostfixApp  _ op) = case S._nameFixity op of
    S.NoFixity -> impossible
    S.SomeFixity s -> s

infixFixity :: InfixApplication -> Fixity
infixFixity (InfixApplication  _ op _) = case S._nameFixity op of
    S.NoFixity -> impossible
    S.SomeFixity s -> s

postfixFixity :: PostfixApplication -> Fixity
postfixFixity (PostfixApplication _ op) = case S._nameFixity op of
    S.NoFixity -> impossible
    S.SomeFixity s -> s

appFixity :: Fixity
appFixity = Fixity PrecOmega (Binary AssocLeft)

funFixity :: Fixity
funFixity = Fixity PrecMinusOmega (Binary AssocRight)

isLeftAssoc :: Fixity -> Bool
isLeftAssoc opInf = case fixityArity opInf of
    Binary AssocLeft -> True
    _ -> False

isRightAssoc :: Fixity -> Bool
isRightAssoc opInf = case fixityArity opInf of
    Binary AssocRight -> True
    _ -> False

isPostfixAssoc :: Fixity -> Bool
isPostfixAssoc opInf = case fixityArity opInf of
    Unary AssocPostfix -> True
    _ -> False

atomParens :: (Fixity -> Bool) -> Maybe Fixity -> Fixity -> Bool
atomParens associates argAtom opInf = case argAtom of
  Nothing -> False
  Just argInf
   | argInf > opInf -> False
   | argInf < opInf -> True
   | associates opInf -> False
   | otherwise -> True

parensCond :: Bool -> Doc Ann -> Doc Ann
parensCond t d = if t then parens d else d

ppPostExpression ::(PrettyCode a, HasFixity a, Member (Reader Options) r)  =>
  Fixity -> a -> Sem r (Doc Ann)
ppPostExpression = ppLRExpression isPostfixAssoc

ppRightExpression :: (PrettyCode a, HasFixity a, Member (Reader Options) r ) =>
  Fixity -> a -> Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression :: (PrettyCode a, HasFixity a, Member (Reader Options) r ) =>
  Fixity -> a -> Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression
  :: (HasFixity a, PrettyCode a, Member (Reader Options) r) =>
     (Fixity -> Bool) -> Fixity -> a -> Sem r (Doc Ann)
ppLRExpression associates atom e =
  parensCond (atomParens associates (getFixity e) atom)
      <$> ppCode e

goAtom :: forall r. Members '[Reader Options] r => ExpressionAtom 'Parsed -> Sem r (Doc Ann)
goAtom a = case a of
  AtomIdentifier n -> ppName n
  AtomLambda l -> ppLambda l
  AtomLetBlock lb -> ppLetBlock lb
  AtomUniverse uni -> ppUniverse uni
  AtomFunction fun -> ppFunction fun
  AtomFunArrow -> return kwArrowR
  AtomMatch m -> ppMatch m
  AtomParens e -> parens <$> goAtoms e

goAtoms :: forall r. Members '[Reader Options] r => ExpressionAtoms 'Parsed -> Sem r (Doc Ann)
goAtoms (ExpressionAtoms l) = hsep . toList <$> mapM goAtom l

ppExpression :: forall s r. (IsStage s, Members '[Reader Options] r) => ExpressionType s -> Sem r (Doc Ann)
ppExpression = case sing :: SStage s of
  SScoped -> go
  SParsed -> goAtoms
  where
    ppApplication :: Application -> Sem r (Doc Ann)
    ppApplication (Application l r) = do
      -- Note: parentheses on the left of an application are never necessary,
      -- but I prefer homogeneous code.
      l' <- ppLeftExpression appFixity l
      r' <- ppRightExpression appFixity r
      return $ l' <+> r'
    go :: Expression -> Sem r (Doc Ann)
    go e = case e of
      ExpressionIdentifier n -> ppSName n
      ExpressionParensIdentifier n -> parens <$> ppSName n
      ExpressionApplication a -> ppApplication a
      ExpressionInfixApplication a -> ppInfixApplication a
      ExpressionPostfixApplication a -> ppPostfixApplication a
      ExpressionLambda l -> ppLambda l
      ExpressionMatch m -> ppMatch m
      ExpressionLetBlock lb -> ppLetBlock lb
      ExpressionUniverse u -> ppUniverse u
      ExpressionFunction f -> ppFunction f
