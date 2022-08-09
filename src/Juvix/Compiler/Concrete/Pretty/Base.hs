module Juvix.Compiler.Concrete.Pretty.Base
  ( module Juvix.Compiler.Concrete.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as T
import Juvix.Compiler.Concrete.Data.ScopedName (AbsModulePath)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode a where
  ppCode :: Members '[Reader Options] r => a -> Sem r (Doc Ann)

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

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
      (StatementCompile _, _) -> False
      (StatementOperator _, StatementOperator _) -> True
      (StatementOperator o, s) -> definesSymbol (o ^. opSymbol) s
      (StatementImport _, StatementImport _) -> True
      (StatementImport i, StatementOpenModule o) -> case sing :: SStage s of
        SParsed -> True
        SScoped ->
          i
            ^. importModule
            . modulePath
            . S.nameId
            == projSigma2 (^. moduleRefName) (o ^. openModuleName . unModuleRef')
            ^. S.nameId
      (StatementImport _, _) -> False
      (StatementOpenModule {}, StatementOpenModule {}) -> True
      (StatementOpenModule {}, _) -> False
      (StatementInductive {}, _) -> False
      (StatementModule {}, _) -> False
      (StatementAxiom {}, StatementAxiom {}) -> True
      (StatementAxiom {}, _) -> False
      (StatementTypeSignature sig, StatementFunctionClause fun) ->
        case sing :: SStage s of
          SParsed -> sig ^. sigName == fun ^. clauseOwnerFunction
          SScoped -> sig ^. sigName == fun ^. clauseOwnerFunction
      (StatementTypeSignature {}, _) -> False
      (StatementFunctionClause fun1, StatementFunctionClause fun2) ->
        case sing :: SStage s of
          SParsed -> fun1 ^. clauseOwnerFunction == fun2 ^. clauseOwnerFunction
          SScoped -> fun1 ^. clauseOwnerFunction == fun2 ^. clauseOwnerFunction
      (StatementFunctionClause {}, _) -> False
    definesSymbol :: Symbol -> Statement s -> Bool
    definesSymbol n s = case s of
      StatementTypeSignature sig -> n == symbolParsed (sig ^. sigName)
      StatementInductive d -> n `elem` syms d
      StatementAxiom d -> n == symbolParsed (d ^. axiomName)
      _ -> False
      where
        symbolParsed :: SymbolType s -> Symbol
        symbolParsed sym = case sing :: SStage s of
          SParsed -> sym
          SScoped -> sym ^. S.nameConcrete
        syms :: InductiveDef s -> [Symbol]
        syms InductiveDef {..} = case sing :: SStage s of
          SParsed -> _inductiveName : map (^. constructorName) _inductiveConstructors
          SScoped ->
            _inductiveName
              ^. S.nameConcrete
              : map (^. constructorName . S.nameConcrete) _inductiveConstructors

instance SingI s => PrettyCode [Statement s] where
  ppCode ss = vsep2 <$> mapM (fmap vsep . mapM (fmap endSemicolon . ppCode)) (groupStatements ss)

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
    StatementForeign p -> ppCode p
    StatementCompile c -> ppCode c

instance PrettyCode Backend where
  ppCode = \case
    BackendGhc -> return kwGhc
    BackendC -> return kwC

instance SingI s => PrettyCode (Compile s) where
  ppCode Compile {..} = do
    compileName' <- ppSymbol _compileName
    compileBackendItems' <- ppBlock _compileBackendItems
    return $ kwCompile <+> compileName' <+> compileBackendItems'

instance PrettyCode ForeignBlock where
  ppCode ForeignBlock {..} = do
    _foreignBackend' <- ppCode _foreignBackend
    return $
      kwForeign
        <+> _foreignBackend'
        <+> lbrace
          <> line
          <> pretty (escape _foreignCode)
          <> line
          <> rbrace
    where
      escape :: Text -> Text
      escape = T.replace "}" "\\}"

instance PrettyCode BackendItem where
  ppCode BackendItem {..} = do
    backend <- ppCode _backendItemBackend
    return $
      backend <+> kwMapsto <+> ppStringLit _backendItemCode

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
    absLocalPath' <- mapM ppCode _absLocalPath
    absTopModulePath' <- ppCode _absTopModulePath
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
    moduleBody' <- indent' <$> ppCode _moduleBody
    modulePath' <- ppModulePathType _modulePath
    moduleParameters' <- ppInductiveParameters _moduleParameters
    moduleDoc' <- mapM ppCode _moduleDoc
    return $
      moduleDoc'
        ?<> kwModule
        <+> modulePath'
        <+?> moduleParameters'
        <> kwSemicolon
        <> line
        <> moduleBody'
        <> line
        <> kwEnd
        <>? lastSemicolon
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
    fixityPrecedence' <- ppCode _fixityPrecedence
    fixityArity' <- ppCode _fixityArity
    return $ fixityArity' <+> fixityPrecedence'

instance PrettyCode OperatorArity where
  ppCode a = return $ case a of
    Unary {} -> kwPostfix
    Binary p -> case p of
      AssocRight -> kwInfixr
      AssocLeft -> kwInfixl
      AssocNone -> kwInfix

instance PrettyCode OperatorSyntaxDef where
  ppCode OperatorSyntaxDef {..} = do
    opSymbol' <- ppUnkindedSymbol _opSymbol
    opFixity' <- ppCode _opFixity
    return $ opFixity' <+> opSymbol'

instance SingI s => PrettyCode (InductiveConstructorDef s) where
  ppCode InductiveConstructorDef {..} = do
    constructorName' <- annDef _constructorName <$> ppSymbol _constructorName
    constructorType' <- ppExpression _constructorType
    doc' <- mapM ppCode _constructorDoc
    return $ doc' ?<> constructorName' <+> kwColon <+> constructorType'

instance PrettyCode BuiltinInductive where
  ppCode i = return (kwBuiltin <+> keyword (prettyText i))

instance PrettyCode BuiltinFunction where
  ppCode i = return (kwBuiltin <+> keyword (prettyText i))

instance PrettyCode BuiltinAxiom where
  ppCode i = return (kwBuiltin <+> keyword (prettyText i))

ppInductiveSignature :: forall r s. (SingI s, Members '[Reader Options] r) => InductiveDef s -> Sem r (Doc Ann)
ppInductiveSignature InductiveDef {..} = do
  inductivebuiltin' <- traverse ppCode _inductiveBuiltin
  inductiveName' <- annDef _inductiveName <$> ppSymbol _inductiveName
  inductiveParameters' <- ppInductiveParameters _inductiveParameters
  inductiveType' <- ppTypeType
  return $
    inductivebuiltin'
      <?+> kwInductive
      <+> inductiveName'
      <+?> inductiveParameters'
      <+?> inductiveType'
  where
    ppTypeType :: Sem r (Maybe (Doc Ann))
    ppTypeType = case _inductiveType of
      Nothing -> return Nothing
      Just e -> Just . (kwColon <+>) <$> ppExpression e

instance PrettyCode (Doc Ann) where
  ppCode d = return d

instance SingI s => PrettyCode (InductiveDef s) where
  ppCode :: forall r. Members '[Reader Options] r => InductiveDef s -> Sem r (Doc Ann)
  ppCode d@InductiveDef {..} = do
    doc' <- mapM ppCode _inductiveDoc
    sig' <- ppInductiveSignature d
    inductiveConstructors' <- ppBlock _inductiveConstructors
    return $ doc' ?<> sig' <+> inductiveConstructors'

dotted :: Foldable f => f (Doc Ann) -> Doc Ann
dotted = concatWith (surround kwDot)

instance PrettyCode QualifiedName where
  ppCode QualifiedName {..} = do
    let symbols = _qualifiedPath ^. pathParts NonEmpty.|> _qualifiedSymbol
    dotted <$> mapM ppSymbol symbols

bracesIf :: Bool -> Doc Ann -> Doc Ann
bracesIf t = if t then braces else id

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
annSDef S.Name' {..} = annotate (AnnDef (_nameDefinedIn ^. S.absTopModulePath) _nameId)

instance PrettyCode TopModulePath where
  ppCode TopModulePath {..} =
    dotted <$> mapM ppSymbol (_modulePathDir ++ [_modulePathName])

instance PrettyCode Name where
  ppCode n = case n of
    NameUnqualified s -> ppSymbol s
    NameQualified s -> ppCode s

nameIdSuffix :: Members '[Reader Options] r => S.NameId -> Sem r (Maybe (Doc Ann))
nameIdSuffix nid = do
  showNameId <- asks (^. optShowNameIds)
  if
      | showNameId -> Just . ("@" <>) <$> ppCode nid
      | otherwise -> return Nothing

instance PrettyCode n => PrettyCode (S.Name' n) where
  ppCode S.Name' {..} = do
    nameConcrete' <- annotateKind _nameKind <$> ppCode _nameConcrete
    uid <- nameIdSuffix _nameId
    return $ annSRef (nameConcrete' <>? uid)
    where
      annSRef :: Doc Ann -> Doc Ann
      annSRef = annotate (AnnRef (_nameDefinedIn ^. S.absTopModulePath) _nameId)

instance PrettyCode ModuleRef where
  ppCode = ppCode . projSigma2 (^. moduleRefName) . (^. unModuleRef')

instance SingI s => PrettyCode (OpenModule s) where
  ppCode :: forall r. Members '[Reader Options] r => OpenModule s -> Sem r (Doc Ann)
  ppCode OpenModule {..} = do
    openModuleName' <- case sing :: SStage s of
      SParsed -> ppCode _openModuleName
      SScoped -> ppCode _openModuleName
    openUsingHiding' <- mapM ppUsingHiding _openUsingHiding
    openParameters' <- ppOpenParams
    let openPublic' = ppPublic
        import_
          | _openModuleImport = Just kwImport
          | otherwise = Nothing
    return $ kwOpen <+?> import_ <+> openModuleName' <+?> openParameters' <+?> openUsingHiding' <+?> openPublic'
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

ppJudocStart :: Doc Ann
ppJudocStart = pretty (Str.judocStart :: Text)

ppJudocExampleStart :: Doc Ann
ppJudocExampleStart = pretty (Str.judocExample :: Text)

instance SingI s => PrettyCode (Example s) where
  ppCode e = do
    e' <- ppExpression (e ^. exampleExpression)
    return (ppJudocStart <+> ppJudocExampleStart <+> e' <> kwSemicolon <> line)

instance SingI s => PrettyCode (JudocBlock s) where
  ppCode = \case
    JudocParagraph l -> vsep <$> mapM ppCode l
    JudocExample e -> ppCode e

instance SingI s => PrettyCode (JudocParagraphLine s) where
  ppCode (JudocParagraphLine atoms) = do
    atoms' <- mconcatMap ppCode atoms
    let prefix = pretty (Str.judocStart :: Text) :: Doc Ann
    return (prefix <> atoms' <> line)

instance SingI s => PrettyCode (Judoc s) where
  ppCode :: forall r. Members '[Reader Options] r => Judoc s -> Sem r (Doc Ann)
  ppCode (Judoc blocks) = mconcatMapM ppCode blocks

instance SingI s => PrettyCode (JudocAtom s) where
  ppCode :: forall r. (Members '[Reader Options] r) => JudocAtom s -> Sem r (Doc Ann)
  ppCode = \case
    JudocExpression e -> goExpression e
    JudocText t -> return (annotate AnnComment (pretty t))
    where
      goExpression :: ExpressionType s -> Sem r (Doc Ann)
      goExpression e = do
        e' <- ppExpression e
        return $ semiDelim e'
      semiDelim :: Doc Ann -> Doc Ann
      semiDelim = enclose1 (annotate AnnComment ";")

instance SingI s => PrettyCode (TypeSignature s) where
  ppCode TypeSignature {..} = do
    let sigTerminating' = if _sigTerminating then kwTerminating <> line else mempty
    sigName' <- annDef _sigName <$> ppSymbol _sigName
    sigType' <- ppExpression _sigType
    builtin' <- traverse ppCode _sigBuiltin
    doc' <- mapM ppCode _sigDoc
    return $ doc' ?<> builtin' <?+> sigTerminating' <> sigName' <+> kwColon <+> sigType'

instance SingI s => PrettyCode (Function s) where
  ppCode :: forall r. Members '[Reader Options] r => Function s -> Sem r (Doc Ann)
  ppCode Function {..} = do
    funParameter' <- ppCode _funParameter
    funReturn' <- ppRightExpression' funFixity _funReturn
    return $ funParameter' <+> kwArrowR <+> funReturn'
    where
      ppRightExpression' = case sing :: SStage s of
        SParsed -> ppRightExpression
        SScoped -> ppRightExpression

instance SingI s => PrettyCode (FunctionParameter s) where
  ppCode FunctionParameter {..} = do
    case _paramName of
      Nothing -> ppLeftExpression' funFixity _paramType
      Just n -> do
        paramName' <- annDef n <$> ppSymbol n
        paramType' <- ppExpression _paramType
        return $ implicitDelim _paramImplicit (paramName' <+> ppUsage _paramUsage <+> paramType')
    where
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

instance PrettyCode Universe where
  ppCode (Universe n _) = return $ kwType <+?> (pretty <$> n)

instance SingI s => PrettyCode (LetBlock s) where
  ppCode LetBlock {..} = do
    letClauses' <- ppBlock _letClauses
    letExpression' <- ppExpression _letExpression
    return $ kwLet <+> letClauses' <+> kwIn <+> letExpression'

instance SingI s => PrettyCode (LetClause s) where
  ppCode c = case c of
    LetTypeSig sig -> ppCode sig
    LetFunClause cl -> ppCode cl

ppBlock :: (PrettyCode a, Members '[Reader Options] r, Traversable t) => t a -> Sem r (Doc Ann)
ppBlock items = bracesIndent . vsep . toList <$> mapM (fmap endSemicolon . ppCode) items

instance SingI s => PrettyCode (LambdaClause s) where
  ppCode LambdaClause {..} = do
    lambdaParameters' <- hsep . toList <$> mapM ppPatternAtom lambdaParameters
    lambdaBody' <- ppExpression lambdaBody
    return $ lambdaParameters' <+> kwMapsto <+> lambdaBody'

instance SingI s => PrettyCode (Lambda s) where
  ppCode Lambda {..} = do
    lambdaClauses' <- ppBlock _lambdaClauses
    return $ kwLambda <+> lambdaClauses'

instance SingI s => PrettyCode (FunctionClause s) where
  ppCode FunctionClause {..} = do
    clauseOwnerFunction' <- ppSymbol _clauseOwnerFunction
    clausePatterns' <- case nonEmpty _clausePatterns of
      Nothing -> return Nothing
      Just ne -> Just . hsep . toList <$> mapM ppPatternAtom ne
    clauseBody' <- ppExpression _clauseBody
    clauseWhere' <- mapM ppCode _clauseWhere
    return $
      clauseOwnerFunction'
        <+?> clausePatterns'
        <+> kwAssignment
        <+> clauseBody'
        <+?> ((line <>) <$> clauseWhere')

instance SingI s => PrettyCode (WhereBlock s) where
  ppCode WhereBlock {..} = indent' . (kwWhere <+>) <$> ppBlock whereClauses

instance SingI s => PrettyCode (WhereClause s) where
  ppCode c = case c of
    WhereOpenModule o -> ppCode o
    WhereTypeSig sig -> ppCode sig
    WhereFunClause fun -> ppCode fun

instance SingI s => PrettyCode (AxiomDef s) where
  ppCode AxiomDef {..} = do
    axiomName' <- annDef _axiomName <$> ppSymbol _axiomName
    axiomDoc' <- mapM ppCode _axiomDoc
    axiomType' <- ppExpression _axiomType
    builtin' <- traverse ppCode _axiomBuiltin
    return $ axiomDoc' ?<> builtin' <?+> kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

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
        b <- asks (^. optInlineImports)
        if b
          then case sing :: SStage s of
            SParsed -> return Nothing
            SScoped -> Just . braces . jumpLines . indent' <$> ppCode m
          else return Nothing

instance PrettyCode PatternScopedIden where
  ppCode = \case
    PatternScopedVar v -> ppCode v
    PatternScopedConstructor c -> ppCode c

instance PrettyCode PatternArg where
  ppCode a = do
    p <- ppCode (a ^. patternArgPattern)
    return (bracesIf (Implicit == a ^. patternArgIsImplicit) p)

instance PrettyCode PatternApp where
  ppCode (PatternApp l r) = do
    l' <- ppLeftExpression appFixity l
    r' <- ppRightExpression appFixity r
    return $ l' <+> r'

ppPatternParenType :: forall s r. (SingI s, Member (Reader Options) r) => PatternParensType s -> Sem r (Doc Ann)
ppPatternParenType p = case sing :: SStage s of
  SParsed -> ppCode p
  SScoped -> ppCode p

instance SingI s => PrettyCode (PatternAtom s) where
  ppCode a = case a of
    PatternAtomIden n -> case sing :: SStage s of
      SParsed -> ppCode n
      SScoped -> ppCode n
    PatternAtomWildcard {} -> return kwWildcard
    PatternAtomEmpty {} -> return $ parens mempty
    PatternAtomParens p -> parens <$> ppPatternParenType p
    PatternAtomBraces p -> braces <$> ppPatternParenType p

instance SingI s => PrettyCode (PatternAtoms s) where
  ppCode (PatternAtoms ps _) = hsep . toList <$> mapM ppCode ps

ppPattern :: forall s r. (SingI s, Members '[Reader Options] r) => PatternType s -> Sem r (Doc Ann)
ppPattern = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtom :: forall s r. (SingI s, Members '[Reader Options] r) => PatternType s -> Sem r (Doc Ann)
ppPatternAtom = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

instance PrettyCode Text where
  ppCode = return . pretty

instance PrettyCode InfixApplication where
  ppCode i@InfixApplication {..} = do
    infixAppLeft' <- ppLeftExpression (getFixity i) _infixAppLeft
    infixAppOperator' <- ppCode _infixAppOperator
    infixAppRight' <- ppRightExpression (getFixity i) _infixAppRight
    return $ infixAppLeft' <+> infixAppOperator' <+> infixAppRight'

instance PrettyCode PostfixApplication where
  ppCode i@PostfixApplication {..} = do
    postfixAppParameter' <- ppPostExpression (getFixity i) _postfixAppParameter
    postfixAppOperator' <- ppCode _postfixAppOperator
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

instance PrettyCode c => PrettyCode (WithLoc c) where
  ppCode = ppCode . (^. withLocParam)

instance PrettyCode Expression where
  ppCode e = case e of
    ExpressionIdentifier n -> ppCode n
    ExpressionHole w -> ppHole w
    ExpressionParensIdentifier n -> parens <$> ppCode n
    ExpressionBraces b -> braces <$> ppCode b
    ExpressionApplication a -> ppCode a
    ExpressionInfixApplication a -> ppCode a
    ExpressionPostfixApplication a -> ppCode a
    ExpressionLambda l -> ppCode l
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
    PatternWildcard {} -> return kwWildcard
    PatternEmpty {} -> return $ parens mempty
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

ppHole :: forall s r. (Members '[Reader Options] r, SingI s) => HoleType s -> Sem r (Doc Ann)
ppHole w = case sing :: SStage s of
  SParsed -> return kwWildcard
  SScoped -> ppCode w

instance PrettyCode Hole where
  ppCode h = do
    suff <- nameIdSuffix (h ^. holeId)
    return (kwWildcard <>? suff)

instance SingI s => PrettyCode (ExpressionAtom s) where
  ppCode = \case
    AtomIdentifier n -> ppName n
    AtomLambda l -> ppCode l
    AtomLetBlock lb -> ppCode lb
    AtomUniverse uni -> ppCode uni
    AtomFunction fun -> ppCode fun
    AtomLiteral lit -> ppCode lit
    AtomFunArrow -> return kwArrowR
    AtomParens e -> parens <$> ppExpression e
    AtomBraces e -> braces <$> ppExpression (e ^. withLocParam)
    AtomHole w -> ppHole w

instance SingI s => PrettyCode (ExpressionAtoms s) where
  ppCode as = hsep . toList <$> mapM ppCode (as ^. expressionAtoms)

ppExpression :: forall s r. (SingI s, Members '[Reader Options] r) => ExpressionType s -> Sem r (Doc Ann)
ppExpression = case sing :: SStage s of
  SScoped -> ppCode
  SParsed -> ppCode

instance PrettyCode SymbolEntry where
  ppCode ent =
    return
      ( kindTag
          <+> pretty (entryName ent ^. S.nameVerbatim)
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
