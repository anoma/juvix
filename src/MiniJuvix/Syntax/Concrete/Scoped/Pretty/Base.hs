module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
  ( module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base,
    module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name (AbsModulePath)
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann
import Prettyprinter hiding (braces, parens)

docStream :: PrettyCode c => Options -> c -> SimpleDocStream Ann
docStream opts =
  layoutPretty defaultLayoutOptions
    . run
    . runReader opts
    . ppCode

data Options = Options
  { _optShowNameId :: Bool,
    _optInlineImports :: Bool,
    _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameId = False,
      _optInlineImports = False,
      _optIndent = 2
    }

class PrettyCode a where
  ppCode :: Members '[Reader Options] r => a -> Sem r (Doc Ann)

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

delimiter :: Text -> Doc Ann
delimiter = annotate AnnDelimiter . pretty

kwModule :: Doc Ann
kwModule = keyword Str.module_

kwEnd :: Doc Ann
kwEnd = keyword Str.end

kwInductive :: Doc Ann
kwInductive = keyword Str.inductive

kwType :: Doc Ann
kwType = keyword Str.type_

kwColon :: Doc Ann
kwColon = keyword Str.colon

kwArrowR :: Doc Ann
kwArrowR = keyword Str.toUnicode

kwMatch :: Doc Ann
kwMatch = keyword Str.match

kwLambda :: Doc Ann
kwLambda = keyword Str.lambdaUnicode

kwGhc :: Doc Ann
kwGhc = keyword Str.ghc

kwAgda :: Doc Ann
kwAgda = keyword Str.agda

kwWhere :: Doc Ann
kwWhere = keyword Str.where_

kwLet :: Doc Ann
kwLet = keyword Str.let_

kwIn :: Doc Ann
kwIn = keyword Str.in_

kwPublic :: Doc Ann
kwPublic = keyword Str.public

kwWildcard :: Doc Ann
kwWildcard = keyword Str.underscore

kwPostfix :: Doc Ann
kwPostfix = keyword Str.postfix

kwInfixr :: Doc Ann
kwInfixr = keyword Str.infixr_

kwInfixl :: Doc Ann
kwInfixl = keyword Str.infixl_

kwInfix :: Doc Ann
kwInfix = keyword Str.infix_

kwAssignment :: Doc Ann
kwAssignment = keyword Str.assignUnicode

kwMapsto :: Doc Ann
kwMapsto = keyword Str.mapstoUnicode

kwColonZero :: Doc Ann
kwColonZero = keyword Str.colonZero

kwColonOne :: Doc Ann
kwColonOne = keyword Str.colonOne

kwColonOmega :: Doc Ann
kwColonOmega = keyword Str.colonOmegaUnicode

kwAxiom :: Doc Ann
kwAxiom = keyword Str.axiom

kwEval :: Doc Ann
kwEval = keyword Str.eval

kwPrint :: Doc Ann
kwPrint = keyword Str.print

kwOpen :: Doc Ann
kwOpen = keyword Str.open

kwUsing :: Doc Ann
kwUsing = keyword Str.using

kwHiding :: Doc Ann
kwHiding = keyword Str.hiding

kwImport :: Doc Ann
kwImport = keyword Str.import_

kwSemicolon :: Doc Ann
kwSemicolon = delimiter Str.semicolon

kwCompile :: Doc Ann
kwCompile = keyword Str.compile

kwForeign :: Doc Ann
kwForeign = keyword Str.foreign_

kwBraceL :: Doc Ann
kwBraceL = delimiter "{"

kwBraceR :: Doc Ann
kwBraceR = delimiter "}"

kwParenL :: Doc Ann
kwParenL = delimiter "("

kwParenR :: Doc Ann
kwParenR = delimiter ")"

kwDQuote :: Doc Ann
kwDQuote = pretty ("\"" :: Text)

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

doubleQuotes :: Doc Ann -> Doc Ann
doubleQuotes = enclose kwDQuote kwDQuote

annotateKind :: S.NameKind -> Doc Ann -> Doc Ann
annotateKind = annotate . AnnKind

ppModulePathType ::
  forall t s r.
  (SingI t, SingI s, Members '[Reader Options] r) =>
  ModulePathType s t ->
  Sem r (Doc Ann)
ppModulePathType x = case sing :: SStage s of
  SParsed -> case sing :: SModuleIsTop t of
    SModuleLocal -> ppCode x
    SModuleTop -> ppCode x
  SScoped -> case sing :: SModuleIsTop t of
    SModuleLocal -> annSDef x <$> ppCode x
    SModuleTop -> annSDef x <$> ppCode x

ppUnkindedSymbol :: Members '[Reader Options] r => Symbol -> Sem r (Doc Ann)
ppUnkindedSymbol = fmap (annotate AnnUnkindedSym) . ppSymbol

ppSymbol :: forall s r. (SingI s, Members '[Reader Options] r) => SymbolType s -> Sem r (Doc Ann)
ppSymbol = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

groupStatements :: forall s. SingI s => [Statement s] -> [[Statement s]]
groupStatements = reverse . map reverse . uncurry cons . foldl' aux ([], [])
  where
    aux ::
      ([Statement s], [[Statement s]]) ->
      Statement s ->
      ([Statement s], [[Statement s]])
    aux ([], acc) s = ([s], acc)
    aux (gr@(a : _), acc) b
      | g a b = (b : gr, acc)
      | otherwise = ([b], gr : acc)
    -- Decides if statements a and b should be next to each other without a
    -- blank line
    g :: Statement s -> Statement s -> Bool
    g a b = case (a, b) of
      (StatementForeign _, _) -> False
      (StatementOperator _, StatementOperator _) -> True
      (StatementOperator o, s) -> definesSymbol (opSymbol o) s
      (StatementImport _, StatementImport _) -> True
      (StatementImport i, StatementOpenModule o) -> case sing :: SStage s of
        SParsed -> True
        SScoped ->
          S._nameId (_modulePath (importModule i))
            == S._nameId (projSigma2 _moduleRefName (o ^. openModuleName . unModuleRef'))
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
        case sing :: SStage s of
          SParsed -> _sigName sig == _clauseOwnerFunction fun
          SScoped -> _sigName sig == _clauseOwnerFunction fun
      (StatementTypeSignature {}, _) -> False
      (StatementFunctionClause fun1, StatementFunctionClause fun2) ->
        case sing :: SStage s of
          SParsed -> _clauseOwnerFunction fun1 == _clauseOwnerFunction fun2
          SScoped -> _clauseOwnerFunction fun1 == _clauseOwnerFunction fun2
      (StatementFunctionClause {}, _) -> False
    definesSymbol :: Symbol -> Statement s -> Bool
    definesSymbol n s = case s of
      StatementTypeSignature sig ->
        let sym = case sing :: SStage s of
              SParsed -> _sigName sig
              SScoped -> S._nameConcrete $ _sigName sig
         in n == sym
      StatementInductive d -> n `elem` syms d
      _ -> False
      where
        syms :: InductiveDef s -> [Symbol]
        syms InductiveDef {..} = case sing :: SStage s of
          SParsed -> _inductiveName : map _constructorName _inductiveConstructors
          SScoped ->
            S._nameConcrete _inductiveName :
            map (S._nameConcrete . _constructorName) _inductiveConstructors

instance SingI s => PrettyCode [Statement s] where
  ppCode ss = joinGroups <$> mapM (fmap mkGroup . mapM (fmap endSemicolon . ppCode)) (groupStatements ss)
    where
      mkGroup = vsep
      joinGroups = concatWith (\a b -> a <> line <> line <> b)

instance SingI s => PrettyCode (Statement s) where
  ppCode s = case s of
    StatementOperator op -> ppCode op
    StatementTypeSignature sig -> ppCode sig
    StatementImport i -> ppCode i
    StatementInductive d -> ppCode d
    StatementModule m -> ppCode m
    StatementOpenModule o -> ppCode o
    StatementFunctionClause c -> ppCode c
    StatementAxiom a -> ppCode a
    StatementEval e -> ppCode e
    StatementPrint p -> ppCode p
    StatementForeign p -> ppCode p

instance PrettyCode Backend where
  ppCode = \case
    BackendGhc -> return kwGhc
    BackendAgda -> return kwAgda

instance PrettyCode ForeignBlock where
  ppCode ForeignBlock {..} = do
    _foreignBackend' <- ppCode _foreignBackend
    return $
      kwForeign <+> _foreignBackend' <+> lbrace <> line
        <> pretty _foreignCode
        <> line
        <> rbrace

instance PrettyCode BackendItem where
  ppCode BackendItem {..} = do
    backend <- ppCode _backendItemBackend
    return $
      backend <+> kwMapsto <+> ppStringLit _backendItemCode

ppStringLit :: Text -> Doc Ann
ppStringLit = annotate AnnLiteralString . doubleQuotes . pretty

ppTopModulePath ::
  forall s r.
  (SingI s, Members '[Reader Options] r) =>
  ModulePathType s 'ModuleTop ->
  Sem r (Doc Ann)
ppTopModulePath = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

endSemicolon :: Doc Ann -> Doc Ann
endSemicolon x = x <> kwSemicolon

instance SingI s => PrettyCode (InductiveParameter s) where
  ppCode InductiveParameter {..} = do
    inductiveParameterName' <- annDef _inductiveParameterName <$> ppSymbol _inductiveParameterName
    inductiveParameterType' <- case sing :: SStage s of
      SParsed -> ppCode _inductiveParameterType
      SScoped -> ppCode _inductiveParameterType
    return $ parens (inductiveParameterName' <+> kwColon <+> inductiveParameterType')

instance SingI s => PrettyCode [InductiveParameter s] where
  ppCode = fmap hsep . mapM ppCode

instance PrettyCode AbsModulePath where
  ppCode S.AbsModulePath {..} = do
    absLocalPath' <- mapM ppCode absLocalPath
    absTopModulePath' <- ppCode absTopModulePath
    return $ dotted (absTopModulePath' : absLocalPath')

ppInductiveParameters ::
  (SingI s, Members '[Reader Options] r) =>
  [InductiveParameter s] ->
  Sem r (Maybe (Doc Ann))
ppInductiveParameters ps
  | null ps = return Nothing
  | otherwise = Just <$> ppCode ps

instance (SingI s, SingI t) => PrettyCode (Module s t) where
  ppCode Module {..} = do
    moduleBody' <- ppCode _moduleBody >>= indented
    modulePath' <- ppModulePathType _modulePath
    moduleParameters' <- ppInductiveParameters _moduleParameters
    return $
      kwModule <+> modulePath' <+?> moduleParameters' <> kwSemicolon <> line
        <> moduleBody'
        <> line
        <> kwEnd
        <?> lastSemicolon
    where
      lastSemicolon = case sing :: SModuleIsTop t of
        SModuleLocal -> Nothing
        SModuleTop -> Just (kwSemicolon <> line)

instance PrettyCode Precedence where
  ppCode p = return $ case p of
    PrecMinusOmega -> pretty ("-ω" :: Text)
    PrecNat n -> pretty n
    PrecOmega -> pretty ("ω" :: Text)

instance PrettyCode Fixity where
  ppCode Fixity {..} = do
    fixityPrecedence' <- ppCode fixityPrecedence
    fixityArity' <- ppCode fixityArity
    return $ fixityArity' <+> fixityPrecedence'

instance PrettyCode OperatorArity where
  ppCode fixityArity = return $ case fixityArity of
    Unary {} -> kwPostfix
    Binary p -> case p of
      AssocRight -> kwInfixr
      AssocLeft -> kwInfixl
      AssocNone -> kwInfix

instance PrettyCode OperatorSyntaxDef where
  ppCode OperatorSyntaxDef {..} = do
    opSymbol' <- ppUnkindedSymbol opSymbol
    opFixity' <- ppCode opFixity
    return $ opFixity' <+> opSymbol'

instance SingI s => PrettyCode (InductiveConstructorDef s) where
  ppCode InductiveConstructorDef {..} = do
    constructorName' <- annDef _constructorName <$> ppSymbol _constructorName
    constructorType' <- ppExpression _constructorType
    return $ constructorName' <+> kwColon <+> constructorType'

instance SingI s => PrettyCode (InductiveDef s) where
  ppCode :: forall r. Members '[Reader Options] r => InductiveDef s -> Sem r (Doc Ann)
  ppCode InductiveDef {..} = do
    inductiveName' <- annDef _inductiveName <$> ppSymbol _inductiveName
    inductiveParameters' <- ppInductiveParameters _inductiveParameters
    inductiveType' <- ppTypeType
    inductiveConstructors' <- ppBlock _inductiveConstructors
    return $
      kwInductive <+> inductiveName' <+?> inductiveParameters' <+?> inductiveType'
        <+> inductiveConstructors'
    where
      ppTypeType :: Sem r (Maybe (Doc Ann))
      ppTypeType = case _inductiveType of
        Nothing -> return Nothing
        Just e -> Just . (kwColon <+>) <$> ppExpression e

dotted :: Foldable f => f (Doc Ann) -> Doc Ann
dotted = concatWith (surround kwDot)

instance PrettyCode QualifiedName where
  ppCode QualifiedName {..} = do
    let symbols = pathParts _qualifiedPath NonEmpty.|> _qualifiedSymbol
    dotted <$> mapM ppSymbol symbols

ppName :: forall s r. (SingI s, Members '[Reader Options] r) => IdentifierType s -> Sem r (Doc Ann)
ppName = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

instance PrettyCode S.NameId where
  ppCode (S.NameId k) = return (pretty k)

annDef :: forall s. SingI s => SymbolType s -> Doc Ann -> Doc Ann
annDef nm = case sing :: SStage s of
  SScoped -> annSDef nm
  SParsed -> id

annSDef :: S.Name' n -> Doc Ann -> Doc Ann
annSDef S.Name' {..} = annotate (AnnDef (S.absTopModulePath _nameDefinedIn) _nameId)

instance PrettyCode TopModulePath where
  ppCode TopModulePath {..} =
    dotted <$> mapM ppSymbol (_modulePathDir ++ [_modulePathName])

instance PrettyCode Symbol where
  ppCode = return . pretty . _symbolText

instance PrettyCode Name where
  ppCode n = case n of
    NameUnqualified s -> ppSymbol s
    NameQualified s -> ppCode s

instance PrettyCode n => PrettyCode (S.Name' n) where
  ppCode S.Name' {..} = do
    nameConcrete' <- annotateKind _nameKind <$> ppCode _nameConcrete
    showNameId <- asks _optShowNameId
    uid <- if showNameId then ("@" <>) <$> ppCode _nameId else return mempty
    return $ annSRef (nameConcrete' <> uid)
    where
      annSRef :: Doc Ann -> Doc Ann
      annSRef = annotate (AnnRef (S.absTopModulePath _nameDefinedIn) _nameId)

instance PrettyCode ModuleRef where
  ppCode = ppCode . projSigma2 _moduleRefName . (^. unModuleRef')

instance SingI s => PrettyCode (OpenModule s) where
  ppCode :: forall r. Members '[Reader Options] r => OpenModule s -> Sem r (Doc Ann)
  ppCode OpenModule {..} = do
    openModuleName' <- case sing :: SStage s of
      SParsed -> ppCode _openModuleName
      SScoped -> ppCode _openModuleName
    openUsingHiding' <- sequence $ ppUsingHiding <$> _openUsingHiding
    openParameters' <- ppOpenParams
    let openPublic' = ppPublic
    return $ keyword "open" <+> openModuleName' <+?> openParameters' <+?> openUsingHiding' <+?> openPublic'
    where
      ppAtom' = case sing :: SStage s of
        SParsed -> ppCodeAtom
        SScoped -> ppCodeAtom
      ppOpenParams :: Sem r (Maybe (Doc Ann))
      ppOpenParams = case _openParameters of
        [] -> return Nothing
        _ -> Just . hsep <$> mapM ppAtom' _openParameters
      ppUsingHiding :: UsingHiding -> Sem r (Doc Ann)
      ppUsingHiding uh = do
        bracedList <-
          encloseSep kwBraceL kwBraceR kwSemicolon . toList
            <$> mapM ppUnkindedSymbol syms
        return $ kw <+> bracedList
        where
          (kw, syms) = case uh of
            Using s -> (kwUsing, s)
            Hiding s -> (kwHiding, s)
      ppPublic :: Maybe (Doc Ann)
      ppPublic = case _openPublic of
        Public -> Just kwPublic
        NoPublic -> Nothing

instance SingI s => PrettyCode (TypeSignature s) where
  ppCode TypeSignature {..} = do
    sigName' <- annDef _sigName <$> ppSymbol _sigName
    sigType' <- ppExpression _sigType
    return $ sigName' <+> kwColon <+> sigType'

instance SingI s => PrettyCode (Function s) where
  ppCode :: forall r. Members '[Reader Options] r => Function s -> Sem r (Doc Ann)
  ppCode Function {..} = do
    funParameter' <- ppFunParameter funParameter
    funReturn' <- ppRightExpression' funFixity funReturn
    return $ funParameter' <+> kwArrowR <+> funReturn'
    where
      ppRightExpression' = case sing :: SStage s of
        SParsed -> ppRightExpression
        SScoped -> ppRightExpression
      ppLeftExpression' = case sing :: SStage s of
        SParsed -> ppLeftExpression
        SScoped -> ppLeftExpression
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
          Nothing -> ppLeftExpression' funFixity paramType
          Just n -> do
            paramName' <- annDef n <$> ppSymbol n
            paramType' <- ppExpression paramType
            return $ parens (paramName' <+> ppUsage paramUsage <+> paramType')

instance PrettyCode Universe where
  ppCode (Universe n) = return $ kwType <+?> (pretty <$> n)

instance SingI s => PrettyCode (LetBlock s) where
  ppCode LetBlock {..} = do
    letClauses' <- ppBlock letClauses
    letExpression' <- ppExpression letExpression
    return $ kwLet <+> letClauses' <+> kwIn <+> letExpression'

instance SingI s => PrettyCode (LetClause s) where
  ppCode c = case c of
    LetTypeSig sig -> ppCode sig
    LetFunClause cl -> ppCode cl

ppBlock :: (PrettyCode a, Members '[Reader Options] r, Traversable t) => t a -> Sem r (Doc Ann)
ppBlock items = mapM (fmap endSemicolon . ppCode) items >>= bracesIndent . vsep . toList

instance SingI s => PrettyCode (MatchAlt s) where
  ppCode MatchAlt {..} = do
    matchAltPattern' <- ppPatternAtom matchAltPattern
    matchAltBody' <- ppExpression matchAltBody
    return $ matchAltPattern' <+> kwMapsto <+> matchAltBody'

instance SingI s => PrettyCode (Match s) where
  ppCode Match {..} = do
    matchExpression' <- ppExpression matchExpression
    matchAlts' <- ppBlock matchAlts
    return $ kwMatch <+> matchExpression' <+> matchAlts'

instance SingI s => PrettyCode (LambdaClause s) where
  ppCode LambdaClause {..} = do
    lambdaParameters' <- hsep . toList <$> mapM ppPatternAtom lambdaParameters
    lambdaBody' <- ppExpression lambdaBody
    return $ lambdaParameters' <+> kwMapsto <+> lambdaBody'

instance SingI s => PrettyCode (Lambda s) where
  ppCode Lambda {..} = do
    lambdaClauses' <- ppBlock lambdaClauses
    return $ kwLambda <+> lambdaClauses'

instance SingI s => PrettyCode (FunctionClause s) where
  ppCode FunctionClause {..} = do
    clauseOwnerFunction' <- ppSymbol _clauseOwnerFunction
    clausePatterns' <- case nonEmpty _clausePatterns of
      Nothing -> return Nothing
      Just ne -> Just . hsep . toList <$> mapM ppPatternAtom ne
    clauseBody' <- ppExpression _clauseBody
    clauseWhere' <- sequence (ppCode <$> _clauseWhere)
    return $
      clauseOwnerFunction' <+?> clausePatterns' <+> kwAssignment <+> clauseBody'
        <+?> ((line <>) <$> clauseWhere')

instance SingI s => PrettyCode (WhereBlock s) where
  ppCode WhereBlock {..} = ppBlock whereClauses >>= indented . (kwWhere <+>)

instance SingI s => PrettyCode (WhereClause s) where
  ppCode c = case c of
    WhereOpenModule o -> ppCode o
    WhereTypeSig sig -> ppCode sig
    WhereFunClause fun -> ppCode fun

instance SingI s => PrettyCode (AxiomDef s) where
  ppCode AxiomDef {..} = do
    axiomName' <- ppSymbol _axiomName
    axiomType' <- ppExpression _axiomType
    axiomBackendItems' <- case _axiomBackendItems of
      [] -> return Nothing
      bs -> Just <$> ppBlock bs
    return $ kwAxiom <+> axiomName' <+> kwColon <+> axiomType' <+?> axiomBackendItems'

instance SingI s => PrettyCode (Eval s) where
  ppCode (Eval p) = do
    p' <- ppExpression p
    return $ kwEval <+> p'

instance SingI s => PrettyCode (Print s) where
  ppCode (Print p) = do
    p' <- ppExpression p
    return $ kwPrint <+> p'

instance SingI s => PrettyCode (Import s) where
  ppCode :: forall r. Members '[Reader Options] r => Import s -> Sem r (Doc Ann)
  ppCode (Import m) = do
    modulePath' <- ppModulePath
    inlineImport' <- inlineImport
    return $ kwImport <+> modulePath' <+?> inlineImport'
    where
      ppModulePath = case sing :: SStage s of
        SParsed -> ppCode m
        SScoped -> ppTopModulePath (m ^. modulePath)
      jumpLines :: Doc Ann -> Doc Ann
      jumpLines x = line <> x <> line
      inlineImport :: Sem r (Maybe (Doc Ann))
      inlineImport = do
        b <- asks _optInlineImports
        if b
          then case sing :: SStage s of
            SParsed -> return Nothing
            SScoped -> ppCode m >>= fmap (Just . braces . jumpLines) . indented
          else return Nothing

instance PrettyCode PatternScopedIden where
  ppCode = \case
    PatternScopedVar v -> ppCode v
    PatternScopedConstructor c -> ppCode c

instance SingI s => PrettyCode (PatternAtom s) where
  ppCode a = case a of
    PatternAtomIden n -> case sing :: SStage s of
      SParsed -> ppCode n
      SScoped -> ppCode n
    PatternAtomWildcard -> return kwWildcard
    PatternAtomEmpty -> return $ parens mempty
    PatternAtomParens p -> parens <$> ppCode p

instance SingI s => PrettyCode (PatternAtoms s) where
  ppCode (PatternAtoms ps) = hsep . toList <$> mapM ppCode ps

ppPattern :: forall s r. (SingI s, Members '[Reader Options] r) => PatternType s -> Sem r (Doc Ann)
ppPattern = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtom :: forall s r. (SingI s, Members '[Reader Options] r) => PatternType s -> Sem r (Doc Ann)
ppPatternAtom = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

instance PrettyCode InfixApplication where
  ppCode i@InfixApplication {..} = do
    infixAppLeft' <- ppLeftExpression (getFixity i) infixAppLeft
    infixAppOperator' <- ppCode infixAppOperator
    infixAppRight' <- ppRightExpression (getFixity i) infixAppRight
    return $ infixAppLeft' <+> infixAppOperator' <+> infixAppRight'

instance PrettyCode PostfixApplication where
  ppCode i@PostfixApplication {..} = do
    postfixAppParameter' <- ppPostExpression (getFixity i) postfixAppParameter
    postfixAppOperator' <- ppCode postfixAppOperator
    return $ postfixAppParameter' <+> postfixAppOperator'

instance PrettyCode Application where
  ppCode (Application l r) = do
    l' <- ppLeftExpression appFixity l
    r' <- ppRightExpression appFixity r
    return $ l' <+> r'

instance PrettyCode Literal where
  ppCode = \case
    LitInteger n -> return $ annotate AnnLiteralInteger (pretty n)
    LitString s -> return $ ppStringLit s

instance PrettyCode LiteralLoc where
  ppCode l = ppCode (l ^. literalLocLiteral)

instance PrettyCode AxiomRef where
  ppCode a = ppCode (a ^. axiomRefName)

instance PrettyCode InductiveRef where
  ppCode a = ppCode (a ^. inductiveRefName)

instance PrettyCode FunctionRef where
  ppCode a = ppCode (a ^. functionRefName)

instance PrettyCode ConstructorRef where
  ppCode a = ppCode (a ^. constructorRefName)

instance PrettyCode ScopedIden where
  ppCode = \case
    ScopedAxiom a -> ppCode a
    ScopedInductive i -> ppCode i
    ScopedVar n -> ppCode n
    ScopedFunction f -> ppCode f
    ScopedConstructor c -> ppCode c

instance PrettyCode Expression where
  ppCode e = case e of
    ExpressionIdentifier n -> ppCode n
    ExpressionParensIdentifier n -> parens <$> ppCode n
    ExpressionApplication a -> ppCode a
    ExpressionInfixApplication a -> ppCode a
    ExpressionPostfixApplication a -> ppCode a
    ExpressionLambda l -> ppCode l
    ExpressionMatch m -> ppCode m
    ExpressionLetBlock lb -> ppCode lb
    ExpressionUniverse u -> ppCode u
    ExpressionLiteral l -> ppCode l
    ExpressionFunction f -> ppCode f

instance PrettyCode Pattern where
  ppCode :: forall r. Members '[Reader Options] r => Pattern -> Sem r (Doc Ann)
  ppCode pat = case pat of
    PatternVariable v -> annDef v <$> ppCode v
    PatternApplication (PatternApp l r) -> do
      l' <- ppLeftExpression appFixity l
      r' <- ppRightExpression appFixity r
      return $ l' <+> r'
    PatternWildcard -> return kwWildcard
    PatternEmpty -> return $ parens mempty
    PatternConstructor constr -> ppCode constr
    PatternInfixApplication i -> ppPatternInfixApp i
    PatternPostfixApplication i -> ppPatternPostfixApp i
    where
      ppPatternInfixApp :: PatternInfixApp -> Sem r (Doc Ann)
      ppPatternInfixApp p@PatternInfixApp {..} = do
        patInfixConstructor' <- ppCode _patInfixConstructor
        patInfixLeft' <- ppLeftExpression (getFixity p) _patInfixLeft
        patInfixRight' <- ppRightExpression (getFixity p) _patInfixRight
        return $ patInfixLeft' <+> patInfixConstructor' <+> patInfixRight'

      ppPatternPostfixApp :: PatternPostfixApp -> Sem r (Doc Ann)
      ppPatternPostfixApp p@PatternPostfixApp {..} = do
        patPostfixConstructor' <- ppCode _patPostfixConstructor
        patPostfixParameter' <- ppLeftExpression (getFixity p) _patPostfixParameter
        return $ patPostfixParameter' <+> patPostfixConstructor'

parensCond :: Bool -> Doc Ann -> Doc Ann
parensCond t d = if t then parens d else d

ppPostExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppPostExpression = ppLRExpression isPostfixAssoc

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensCond (atomParens associates (atomicity e) fixlr)
    <$> ppCode e

ppCodeAtom :: (HasAtomicity c, PrettyCode c, Members '[Reader Options] r) => c -> Sem r (Doc Ann)
ppCodeAtom c = do
  p' <- ppCode c
  return $ if isAtomic c then p' else parens p'

instance SingI s => PrettyCode (ExpressionAtom s) where
  ppCode a = case a of
    AtomIdentifier n -> ppName n
    AtomLambda l -> ppCode l
    AtomLetBlock lb -> ppCode lb
    AtomUniverse uni -> ppCode uni
    AtomFunction fun -> ppCode fun
    AtomLiteral lit -> ppCode lit
    AtomFunArrow -> return kwArrowR
    AtomMatch m -> ppCode m
    AtomParens e -> parens <$> ppExpression e

instance SingI s => PrettyCode (ExpressionAtoms s) where
  ppCode (ExpressionAtoms l) = hsep . toList <$> mapM ppCode l

ppExpression :: forall s r. (SingI s, Members '[Reader Options] r) => ExpressionType s -> Sem r (Doc Ann)
ppExpression = case sing :: SStage s of
  SScoped -> ppCode
  SParsed -> ppCode

instance PrettyCode SymbolEntry where
  ppCode ent =
    return
      ( kindTag <+> pretty (entryName ent ^. S.nameVerbatim)
          <+> "defined at"
          <+> pretty (getLoc ent)
      )
    where
      pretty' :: Text -> Doc a
      pretty' = pretty
      kindTag = case ent of
        EntryAxiom _ -> annotateKind S.KNameAxiom (pretty' Str.axiom)
        EntryInductive _ -> annotateKind S.KNameInductive (pretty' Str.inductive)
        EntryFunction _ -> annotateKind S.KNameFunction (pretty' Str.function)
        EntryConstructor _ -> annotateKind S.KNameConstructor (pretty' Str.constructor)
        EntryModule (ModuleRef' (isTop :&: _))
          | SModuleTop <- isTop -> annotateKind S.KNameTopModule (pretty' Str.topModule)
          | SModuleLocal <- isTop -> annotateKind S.KNameLocalModule (pretty' Str.localModule)
