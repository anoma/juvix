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
  {
    _optShowNameId :: Bool,
    _optInlineImports :: Bool,
    _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    {
      _optShowNameId = False,
      _optInlineImports = False,
      _optIndent = 2
    }

class PrettyCode a where
  ppCode :: Members '[Reader Options] r => a -> Sem r (Doc Ann)

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

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

ppModulePathType :: forall t s r. (SingI t, SingI s, Members '[Reader Options] r) =>
  ModulePathType s t -> Sem r (Doc Ann)
ppModulePathType x = case sing :: SStage s of
  SParsed -> case sing :: SModuleIsTop t of
    SModuleLocal -> ppCode x
    SModuleTop -> ppCode x
  SScoped -> case sing :: SModuleIsTop t of
    SModuleLocal -> annSDef x <$> ppCode x
    SModuleTop ->  annSDef x <$> ppCode x

ppUnkindedSymbol :: Members '[Reader Options] r => Symbol -> Sem r (Doc Ann)
ppUnkindedSymbol = fmap (annotate AnnUnkindedSym) . ppSymbol

ppSymbol :: forall s r. (SingI s, Members '[Reader Options] r) => SymbolType s -> Sem r (Doc Ann)
ppSymbol = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

groupStatements :: forall s. SingI s => [Statement s] -> [[Statement s]]
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
      case sing :: SStage s of
        SParsed -> sigName sig == clauseOwnerFunction fun
        SScoped -> sigName sig == clauseOwnerFunction fun
    (StatementTypeSignature {}, _) -> False
    (StatementFunctionClause fun1, StatementFunctionClause fun2) ->
      case sing :: SStage s of
        SParsed -> clauseOwnerFunction fun1 == clauseOwnerFunction fun2
        SScoped -> clauseOwnerFunction fun1 == clauseOwnerFunction fun2
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

ppTopModulePath :: forall s r. (SingI s, Members '[Reader Options] r) =>
  ModulePathType s 'ModuleTop -> Sem r (Doc Ann)
ppTopModulePath = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

endSemicolon :: Doc Ann -> Doc Ann
endSemicolon x = x <> kwSemicolon

instance SingI s => PrettyCode (InductiveParameter s) where
  ppCode InductiveParameter {..} = do
    inductiveParameterName' <- annDef inductiveParameterName <$> ppSymbol inductiveParameterName
    inductiveParameterType' <- case sing :: SStage s of
      SParsed -> ppCode inductiveParameterType
      SScoped -> ppCode inductiveParameterType
    return $ parens (inductiveParameterName' <+> kwColon <+> inductiveParameterType')

instance SingI s => PrettyCode [InductiveParameter s] where
  ppCode = fmap hsep . mapM ppCode

ppInductiveParameters :: (SingI s, Members '[Reader Options] r)
    => [InductiveParameter s] -> Sem r (Maybe (Doc Ann))
ppInductiveParameters ps
  | null ps = return Nothing
  | otherwise = Just <$> ppCode ps

instance (SingI s, SingI t) => PrettyCode (Module s t) where
  ppCode Module {..} = do
      moduleBody' <- ppCode moduleBody >>= indented
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

instance PrettyCode Precedence where
  ppCode p = return $ annotate AnnNumber $ case p of
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
    constructorName' <- annDef constructorName <$> ppSymbol constructorName
    constructorType' <- ppExpression constructorType
    return $ constructorName' <+> kwColon <+> constructorType'

instance SingI s => PrettyCode (InductiveDef s) where
  ppCode :: forall r. Members '[Reader Options] r => InductiveDef s -> Sem r (Doc Ann)
  ppCode InductiveDef {..} = do
    inductiveName' <- annDef inductiveName <$> ppSymbol inductiveName
    inductiveParameters' <- ppInductiveParameters inductiveParameters
    inductiveType' <- ppTypeType
    inductiveConstructors' <- ppBlock inductiveConstructors
    return $
      kwInductive <+> inductiveName' <+?> inductiveParameters' <+?> inductiveType'
        <+> inductiveConstructors'
    where
      ppTypeType :: Sem r (Maybe (Doc Ann))
      ppTypeType = case inductiveType of
        Nothing -> return Nothing
        Just e -> Just . (kwColon <+>) <$> ppExpression e

dotted :: Foldable f => f (Doc Ann) -> Doc Ann
dotted = concatWith (surround kwDot)

instance PrettyCode QualifiedName where
  ppCode QualifiedName {..} = do
    let symbols = pathParts qualifiedPath NonEmpty.|> qualifiedSymbol
    dotted <$> mapM ppSymbol symbols

ppName :: forall s r. (SingI s, Members '[Reader Options] r) => NameType s -> Sem r (Doc Ann)
ppName = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

instance PrettyCode S.NameId where
  ppCode (S.NameId k) = return $ pretty k

annDef :: forall s. SingI s => SymbolType s -> Doc Ann -> Doc Ann
annDef nm = case sing :: SStage s of
  SScoped -> annSDef nm
  SParsed -> id

annSDef :: S.Name' n -> Doc Ann -> Doc Ann
annSDef nm = annotate (AnnDef (S.absTopModulePath (S._nameDefinedIn nm)) (S._nameId nm))

annSRef :: S.Name' n -> Doc Ann -> Doc Ann
annSRef nm = annotate (AnnRef (S.absTopModulePath (S._nameDefinedIn nm)) (S._nameId nm))

annRef :: forall s. SingI s => SymbolType s -> Doc Ann -> Doc Ann
annRef nm = case sing :: SStage s of
  SParsed -> id
  SScoped -> annSRef nm

instance PrettyCode TopModulePath where
  ppCode TopModulePath {..} =
    dotted <$> mapM ppSymbol (modulePathDir ++ [modulePathName])

instance PrettyCode Symbol where
  ppCode = return . pretty . _symbolText

instance PrettyCode Name where
  ppCode n = case n of
    NameUnqualified s -> ppSymbol s
    NameQualified s -> ppCode s

instance PrettyCode n => PrettyCode (S.Name' n) where
  ppCode S.Name' {..} = do
    nameConcrete' <- annotate (AnnKind _nameKind) <$> ppCode _nameConcrete
    showNameId <- asks _optShowNameId
    uid <- if showNameId then ("@" <>) <$> ppCode _nameId else return mempty
    return $ nameConcrete' <> uid

instance SingI s => PrettyCode (OpenModule s) where
  ppCode :: forall r. Members '[Reader Options] r => OpenModule s -> Sem r (Doc Ann)
  ppCode OpenModule {..} = do
    openModuleName' <- ppName openModuleName
    openUsingHiding' <- sequence $ ppUsingHiding <$> openUsingHiding
    openParameters' <- ppOpenParams
    let openPublic' = ppPublic
    return $ keyword "open" <+> openModuleName' <+?> openParameters' <+?> openUsingHiding' <+?> openPublic'
    where
    ppAtom' = case sing :: SStage s of
      SParsed -> ppCodeAtom
      SScoped -> ppCodeAtom
    ppOpenParams :: Sem r (Maybe (Doc Ann))
    ppOpenParams = case openParameters of
      [] -> return Nothing
      _ -> Just . hsep <$> mapM ppAtom' openParameters
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

instance SingI s => PrettyCode (TypeSignature s) where
  ppCode TypeSignature {..} = do
    sigName' <- annDef sigName <$> ppSymbol sigName
    sigType' <- ppExpression sigType
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
    matchAltPattern' <- ppPattern matchAltPattern
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
    clauseOwnerFunction' <- annRef clauseOwnerFunction <$> ppSymbol clauseOwnerFunction
    clausePatterns' <- case nonEmpty clausePatterns of
      Nothing -> return Nothing
      Just ne -> Just . hsep . toList <$> mapM ppPatternAtom ne
    clauseBody' <- ppExpression clauseBody
    clauseWhere' <- sequence (ppCode <$> clauseWhere)
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
    axiomName' <- ppSymbol axiomName
    axiomType' <- ppExpression axiomType
    return $ kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

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
      SScoped -> annSRef (modulePath m) <$> ppTopModulePath (modulePath m)
    jumpLines :: Doc Ann -> Doc Ann
    jumpLines x = line <> x <> line
    inlineImport :: Sem r (Maybe (Doc Ann))
    inlineImport = do
      b <- asks _optInlineImports
      if b then case sing :: SStage s of
        SParsed -> return Nothing
        SScoped -> ppCode m >>= fmap (Just . braces . jumpLines) . indented
        else return Nothing

instance SingI s => PrettyCode (PatternAtom s) where
  ppCode a = case a of
    PatternAtomName n -> ppName n
    PatternAtomWildcard -> return kwWildcard
    PatternAtomEmpty -> return $ parens mempty
    PatternAtomParens p -> parens <$> ppCode p

instance SingI s => PrettyCode (PatternAtoms s) where
  ppCode (PatternAtoms ps) = hsep . toList <$> mapM ppCode ps

instance HasAtoms Pattern where
  isAtomic p = case p of
    PatternVariable {} -> True
    PatternApplication {} -> False
    PatternConstructor {} -> True
    PatternInfixApplication {} -> False
    PatternPostfixApplication {} -> False
    PatternWildcard -> True
    PatternEmpty -> True

ppPattern :: forall s r. (SingI s,  Members '[Reader Options] r) => PatternType s -> Sem r (Doc Ann)
ppPattern = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtom :: forall s r. (SingI s,  Members '[Reader Options] r) => PatternType s -> Sem r (Doc Ann)
ppPatternAtom = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

ppCodeAtom :: (HasAtoms c, PrettyCode c, Members '[Reader Options] r) => c -> Sem r (Doc Ann)
ppCodeAtom c = do
  p' <- ppCode c
  return $ if isAtomic c then p' else parens p'

instance HasAtoms (PatternAtom s) where
  isAtomic = const True

class HasAtoms c where
  isAtomic :: c -> Bool

instance HasAtoms Expression where
  isAtomic e = case e of
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

instance HasAtoms (ExpressionAtoms s) where
  isAtomic e = case e of
    ExpressionAtoms (_  :| []) -> True
    ExpressionAtoms (_  :| _) -> False

instance PrettyCode InfixApplication where
  ppCode i@InfixApplication {..} = do
    infixAppLeft' <- ppLeftExpression (infixFixity i) infixAppLeft
    infixAppOperator' <- ppCode infixAppOperator
    infixAppRight' <- ppRightExpression (infixFixity i) infixAppRight
    return $ infixAppLeft' <+> infixAppOperator' <+> infixAppRight'

instance PrettyCode PostfixApplication where
  ppCode i@PostfixApplication {..} = do
    postfixAppParameter' <- ppPostExpression (postfixFixity i) postfixAppParameter
    postfixAppOperator' <- ppCode postfixAppOperator
    return $ postfixAppParameter' <+> postfixAppOperator'

instance PrettyCode Application where
  ppCode (Application l r) = do
    -- Note: parentheses on the left of an application are never necessary,
    -- but I prefer homogeneous code.
    l' <- ppLeftExpression appFixity l
    r' <- ppRightExpression appFixity r
    return $ l' <+> r'

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
    ExpressionFunction f -> ppCode f

instance PrettyCode Pattern where
  ppCode :: forall r. Members '[Reader Options] r => Pattern -> Sem r (Doc Ann)
  ppCode pat = case pat of
    PatternVariable v -> annDef v <$> ppCode v
    PatternApplication l r -> do
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
      patInfixConstructor' <- ppCode patInfixConstructor
      patInfixLeft' <- ppLeftExpression (pinfixFixity p) patInfixLeft
      patInfixRight' <- ppRightExpression (pinfixFixity p) patInfixRight
      return $ patInfixLeft' <+> patInfixConstructor' <+> patInfixRight'

    ppPatternPostfixApp :: PatternPostfixApp -> Sem r (Doc Ann)
    ppPatternPostfixApp p@PatternPostfixApp {..} = do
      patPostfixConstructor' <- ppCode patPostfixConstructor
      patPostfixParameter' <- ppLeftExpression (ppostfixFixity p) patPostfixParameter
      return $ patPostfixParameter' <+> patPostfixConstructor'

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

ppRightExpression :: (PrettyCode a, HasFixity a, Member (Reader Options) r) =>
  Fixity -> a -> Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression :: (PrettyCode a, HasFixity a, Member (Reader Options) r) =>
  Fixity -> a -> Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression
  :: (HasFixity a, PrettyCode a, Member (Reader Options) r) =>
     (Fixity -> Bool) -> Fixity -> a -> Sem r (Doc Ann)
ppLRExpression associates atom e =
  parensCond (atomParens associates (getFixity e) atom)
      <$> ppCode e

instance SingI s => PrettyCode (ExpressionAtom s) where
  ppCode a = case a of
    AtomIdentifier n -> ppName n
    AtomLambda l -> ppCode l
    AtomLetBlock lb -> ppCode lb
    AtomUniverse uni -> ppCode uni
    AtomFunction fun -> ppCode fun
    AtomFunArrow -> return kwArrowR
    AtomMatch m -> ppCode m
    AtomParens e -> parens <$> ppExpression e

instance SingI s => PrettyCode (ExpressionAtoms s) where
  ppCode (ExpressionAtoms l) = hsep . toList <$> mapM ppCode l

ppExpression :: forall s r. (SingI s, Members '[Reader Options] r) => ExpressionType s -> Sem r (Doc Ann)
ppExpression = case sing :: SStage s of
  SScoped -> ppCode
  SParsed -> ppCode
