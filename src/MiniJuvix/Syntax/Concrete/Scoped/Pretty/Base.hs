module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base (
  module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base,
  module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann
                                                    ) where

import Data.Singletons
import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Utils.Prelude
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
      _optInlineImports = True,
      _optIndent = 2
    }

-- | Pretty prints a top module.
prettyTopModule :: Options -> Module 'Scoped 'ModuleTop -> Doc Ann
prettyTopModule opts = run . runReader opts . ppModule

infixl 7 <+?>

(<+?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<+?>) a = maybe a (a <+>)

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

ppModulePathType :: forall t r. (SingI t, Members '[Reader Options] r) => ModulePathType 'Scoped t -> Sem r (Doc Ann)
ppModulePathType x = case sing :: SModuleIsTop t of
  SModuleTop -> ppSTopModulePath x
  SModuleLocal -> ppSSymbol x

ppSymbol :: Members '[Reader Options] r => Symbol -> Sem r (Doc Ann)
ppSymbol (Sym t) = return (pretty t)

groupStatements :: [Statement 'Scoped] -> [[Statement 'Scoped]]
groupStatements = reverse . map reverse . uncurry cons . foldl' aux ([], [])
  where
  aux :: ([Statement 'Scoped], [[Statement 'Scoped]]) -> Statement 'Scoped
      -> ([Statement 'Scoped], [[Statement 'Scoped]])
  aux ([], acc) s = ([s], acc)
  aux (gr@(a : _), acc) b
    | g a b = (b : gr, acc)
    | otherwise = ([b], gr : acc)
  -- | Decides if statements a and b should be next to each other without a
  -- blank line
  g :: Statement 'Scoped -> Statement 'Scoped -> Bool
  g a b = case (a, b) of
    (StatementOperator _, StatementOperator _) -> True
    (StatementOperator o, s) -> definesSymbol (opSymbol o) s
    (StatementImport _, StatementImport _) -> True
    (StatementImport i, StatementOpenModule o) ->
      S._nameId (modulePath (importModule i)) == S._nameId (openModuleName o)
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
  definesSymbol :: Symbol -> Statement 'Scoped -> Bool
  definesSymbol n s = case s of
    StatementTypeSignature sig -> n == S._nameConcrete (sigName sig)
    StatementInductive InductiveDef{..} ->
      n == S._nameConcrete inductiveName ||
      elem n (map (S._nameConcrete . constructorName) inductiveConstructors)
    _ -> False

ppStatements :: Members '[Reader Options] r => [Statement 'Scoped] -> Sem r (Doc Ann)
ppStatements ss = joinGroups <$> mapM (fmap mkGroup . mapM (fmap endSemicolon . ppStatement)) (groupStatements ss)
  where
    mkGroup = vsep
    joinGroups = concatWith (\a b -> a <> line <> line <> b)

ppStatement :: Members '[Reader Options] r => Statement 'Scoped -> Sem r (Doc Ann)
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

ppTopModulePath :: Members '[Reader Options] r => TopModulePath -> Sem r (Doc Ann)
ppTopModulePath TopModulePath {..} =
  dotted <$> mapM ppSymbol (modulePathDir ++ [modulePathName])

ppSTopModulePath :: Members '[Reader Options] r => S.TopModulePath -> Sem r (Doc Ann)
ppSTopModulePath = ppSName' ppTopModulePath

endSemicolon :: Doc Ann -> Doc Ann
endSemicolon x = x <> kwSemicolon

ppModule :: forall t r. (SingI t, Members '[Reader Options] r) => Module 'Scoped t -> Sem r (Doc Ann)
ppModule Module {..} = do
  moduleBody' <- ppStatements moduleBody >>= indented
  modulePath' <- ppModulePathType modulePath
  return $
    kwModule <+> modulePath' <> kwSemicolon <> line
      <> moduleBody'
      <> line
      <> kwEnd
      <?> lastSemicolon
  where
    lastSemicolon = case sing :: SModuleIsTop t of
      SModuleLocal -> Nothing
      SModuleTop -> Just kwSemicolon

ppPrecedence :: Precedence -> Doc Ann
ppPrecedence p = case p of
  PrecMinusOmega -> pretty ("-ω" :: Text)
  PrecNat n -> pretty n
  PrecOmega -> pretty ("ω" :: Text)

ppOperatorSyntaxDef :: Members '[Reader Options] r => OperatorSyntaxDef -> Sem r (Doc Ann)
ppOperatorSyntaxDef OperatorSyntaxDef {..} = do
  opSymbol' <- ppSymbol opSymbol
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

ppInductiveConstructorDef :: Members '[Reader Options] r => InductiveConstructorDef 'Scoped -> Sem r (Doc Ann)
ppInductiveConstructorDef InductiveConstructorDef {..} = do
  constructorName' <- ppSSymbol constructorName
  constructorType' <- ppExpression constructorType
  return $ constructorName' <+> kwColon <+> constructorType'

ppInductiveDef :: forall r. Members '[Reader Options] r => InductiveDef 'Scoped -> Sem r (Doc Ann)
ppInductiveDef InductiveDef {..} = do
  inductiveName' <- ppSSymbol inductiveName
  inductiveParameters' <- hsep <$> mapM ppInductiveParameter inductiveParameters
  inductiveType' <- ppTypeType
  inductiveConstructors' <- ppBlock ppInductiveConstructorDef inductiveConstructors
  return $
    kwInductive <+> inductiveName' <+> inductiveParameters' <+?> inductiveType'
      <+> inductiveConstructors'
  where
    ppTypeType :: Sem r (Maybe (Doc Ann))
    ppTypeType = case inductiveType of
      Nothing -> return Nothing
      Just e -> Just . (kwColon <+>) <$> ppExpression e
    ppInductiveParameter :: InductiveParameter 'Scoped -> Sem r (Doc Ann)
    ppInductiveParameter InductiveParameter {..} = do
      inductiveParameterName' <- ppSSymbol inductiveParameterName
      inductiveParameterType' <- ppExpression inductiveParameterType
      return $ parens (inductiveParameterName' <+> kwColon <+> inductiveParameterType')

dotted :: Foldable f => f (Doc Ann) -> Doc Ann
dotted = concatWith (surround kwDot)

ppQualified :: Members '[Reader Options] r => QualifiedName -> Sem r (Doc Ann)
ppQualified QualifiedName {..} = do
  let symbols = pathParts qualifiedPath NonEmpty.|> qualifiedSymbol
  dotted <$> mapM ppSymbol symbols

ppName :: Members '[Reader Options] r => Name -> Sem r (Doc Ann)
ppName n = case n of
  NameUnqualified s -> ppSymbol s
  NameQualified s -> ppQualified s

ppNameId :: S.NameId -> Doc Ann
ppNameId (S.NameId k) = pretty k

ppSSymbol :: Members '[Reader Options] r => S.Symbol -> Sem r (Doc Ann)
ppSSymbol = ppSName' ppSymbol

ppSName :: Members '[Reader Options] r => S.Name -> Sem r (Doc Ann)
ppSName = ppSName' ppName

ppSName' :: Members '[Reader Options] r => (s -> Sem r (Doc Ann)) -> S.Name' s -> Sem r (Doc Ann)
ppSName' ppConcrete S.Name' {..} = do
  nameConcrete' <- annotate (AnnKind _nameKind) <$> ppConcrete _nameConcrete
  showNameId <- asks _optShowNameId
  let uid = if showNameId then "@" <> ppNameId _nameId else mempty
  return $ nameConcrete' <> uid

ppOpen :: forall r. Members '[Reader Options] r => OpenModule 'Scoped -> Sem r (Doc Ann)
ppOpen OpenModule {..} = do
  openModuleName' <- ppSName openModuleName
  openUsingHiding' <- sequence $ ppUsingHiding <$> openUsingHiding
  let openPublic' = ppPublic
  return $ keyword "open" <+> openModuleName' <+?> openUsingHiding' <+?> openPublic'
  where
    ppUsingHiding :: UsingHiding -> Sem r (Doc Ann)
    ppUsingHiding uh = do
      bracedList <- encloseSep lbrace rbrace kwSemicolon . toList <$> mapM ppSymbol syms
      return $ kw <+> bracedList
      where
        (kw, syms) = case uh of
          Using s -> (kwUsing, s)
          Hiding s -> (kwHiding, s)
    ppPublic :: Maybe (Doc Ann)
    ppPublic = case openPublic of
      Public -> Just kwPublic
      NoPublic -> Nothing

ppTypeSignature :: Members '[Reader Options] r => TypeSignature 'Scoped -> Sem r (Doc Ann)
ppTypeSignature TypeSignature {..} = do
  sigName' <- ppSSymbol sigName
  sigType' <- ppExpression sigType
  return $ sigName' <+> kwColon <+> sigType'

ppFunction :: forall r. Members '[Reader Options] r => Function 'Scoped -> Sem r (Doc Ann)
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
    ppFunParameter :: FunctionParameter 'Scoped -> Sem r (Doc Ann)
    ppFunParameter FunctionParameter {..} = do
      case paramName of
        Nothing -> ppLeftExpression funFixity paramType
        Just n -> do
          paramName' <- ppSSymbol n
          paramType' <- ppExpression paramType
          return $ parens (paramName' <+> ppUsage paramUsage <+> paramType')

ppUniverse :: Members '[Reader Options] r => Universe -> Sem r (Doc Ann)
ppUniverse (Universe n) = return $ kwType <+?> (pretty <$> n)

ppLetBlock :: forall r. Members '[Reader Options] r => LetBlock 'Scoped -> Sem r (Doc Ann)
ppLetBlock LetBlock {..} = do
  letClauses' <- ppBlock ppLetClause letClauses
  letExpression' <- ppExpression letExpression
  return $ kwLet <+> letClauses' <+> kwIn <+> letExpression'
  where
    ppLetClause :: LetClause 'Scoped -> Sem r (Doc Ann)
    ppLetClause c = case c of
      LetTypeSig sig -> ppTypeSignature sig
      LetFunClause cl -> ppFunctionClause cl

ppBlock :: Members '[Reader Options] r => (a -> Sem r (Doc Ann)) -> [a] -> Sem r (Doc Ann)
ppBlock ppItem items = mapM (fmap endSemicolon . ppItem) items >>= bracesIndent . vsep

ppMatch :: forall r. Members '[Reader Options] r => Match 'Scoped -> Sem r (Doc Ann)
ppMatch Match {..} = do
  matchExpression' <- ppExpression matchExpression
  matchAlts' <- ppBlock ppMatchAlt matchAlts
  return $ kwMatch <+> matchExpression' <+> matchAlts'
  where
    ppMatchAlt :: MatchAlt 'Scoped -> Sem r (Doc Ann)
    ppMatchAlt MatchAlt {..} = do
      matchAltPattern' <- ppPattern matchAltPattern
      matchAltBody' <- ppExpression matchAltBody
      return $ matchAltPattern' <+> kwMapsto <+> matchAltBody'

ppLambda :: forall r. Members '[Reader Options] r => Lambda 'Scoped -> Sem r (Doc Ann)
ppLambda Lambda {..} = do
  lambdaClauses' <- ppBlock ppLambdaClause lambdaClauses
  return $ kwLambda <+> lambdaClauses'
  where
    ppLambdaClause :: LambdaClause 'Scoped -> Sem r (Doc Ann)
    ppLambdaClause LambdaClause {..} = do
      lambdaParameters' <- hsep . toList <$> mapM ppPattern lambdaParameters
      lambdaBody' <- ppExpression lambdaBody
      return $ lambdaParameters' <+> kwMapsto <+> lambdaBody'

ppFunctionClause :: forall r. Members '[Reader Options] r => FunctionClause 'Scoped -> Sem r (Doc Ann)
ppFunctionClause FunctionClause {..} = do
  clauseOwnerFunction' <- ppSSymbol clauseOwnerFunction
  clausePatterns' <- case nonEmpty clausePatterns of
    Nothing -> return Nothing
    Just ne -> Just . hsep . toList <$> mapM ppPattern ne
  clauseBody' <- ppExpression clauseBody
  clauseWhere' <- sequence (ppWhereBlock <$> clauseWhere)
  return $
    clauseOwnerFunction' <+?> clausePatterns' <+> kwAssignment <+> clauseBody'
      <+?> ((line <>) <$> clauseWhere')
  where
    ppWhereBlock :: WhereBlock 'Scoped -> Sem r (Doc Ann)
    ppWhereBlock WhereBlock {..} =
      ppBlock ppWhereClause whereClauses >>= indented . (kwWhere <+>)
      where
        ppWhereClause :: WhereClause 'Scoped -> Sem r (Doc Ann)
        ppWhereClause c = case c of
          WhereOpenModule o -> ppOpen o
          WhereTypeSig sig -> ppTypeSignature sig
          WhereFunClause fun -> ppFunctionClause fun

ppAxiom :: Members '[Reader Options] r => AxiomDef 'Scoped -> Sem r (Doc Ann)
ppAxiom AxiomDef {..} = do
  axiomName' <- ppSSymbol axiomName
  axiomType' <- ppExpression axiomType
  return $ kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

ppEval :: Members '[Reader Options] r => Eval 'Scoped -> Sem r (Doc Ann)
ppEval (Eval p) = do
  p' <- ppExpression p
  return $ kwEval <+> p'

ppPrint :: Members '[Reader Options] r => Print 'Scoped -> Sem r (Doc Ann)
ppPrint (Print p) = do
  p' <- ppExpression p
  return $ kwPrint <+> p'

ppImport :: forall r. Members '[Reader Options] r => Import 'Scoped -> Sem r (Doc Ann)
ppImport (Import m@Module {..}) = do
  modulePath' <- ppSTopModulePath modulePath
  inlineImport' <- inlineImport
  return $ kwImport <+> modulePath' <+?> inlineImport'
  where
  jumpLines :: Doc Ann -> Doc Ann
  jumpLines x = line <> x <> line
  inlineImport :: Sem r (Maybe (Doc Ann))
  inlineImport = do
    b <- asks _optInlineImports
    if b then do
      ppModule m >>= fmap (Just . braces . jumpLines) . indented
      else return Nothing

ppPattern :: forall r. Members '[Reader Options] r => Pattern -> Sem r (Doc Ann)
ppPattern pat = do
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
      PatternVariable v -> ppSSymbol v
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

isAtomic :: Expression -> Bool
isAtomic e = case e of
  ExpressionIdentifier {} -> True
  ExpressionApplication {} -> False
  ExpressionInfixApplication {} -> False
  ExpressionPostfixApplication {} -> False
  ExpressionLambda {} -> True
  ExpressionMatch {} -> True
  ExpressionLetBlock {} -> True
  ExpressionUniverse {} -> True
  ExpressionFunction {} -> False

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

class HasFixity a where
  getFixity :: a -> Maybe Fixity

instance HasFixity Expression where
 getFixity e = case e of
    ExpressionIdentifier {} -> Nothing
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

ppExpression :: forall r. Members '[Reader Options] r => Expression -> Sem r (Doc Ann)
ppExpression = go
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
      ExpressionApplication a -> ppApplication a
      ExpressionInfixApplication a -> ppInfixApplication a
      ExpressionPostfixApplication a -> ppPostfixApplication a
      ExpressionLambda l -> ppLambda l
      ExpressionMatch m -> ppMatch m
      ExpressionLetBlock lb -> ppLetBlock lb
      ExpressionUniverse u -> ppUniverse u
      ExpressionFunction f -> ppFunction f
