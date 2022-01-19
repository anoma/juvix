module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base where


import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Utils.Prelude hiding (Reader, runReader, asks)
import MiniJuvix.Syntax.Concrete.Language
import Polysemy
import Polysemy.Reader
import Prettyprinter hiding (braces, parens)
import Data.Singletons


data Ann = AnnKind S.NameKind
  | AnnKeyword
  | AnnDelimiter

data Options = Options {
  _optOptimizeParens :: Bool,
  _optShowNameId :: Bool,
  _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions = Options {
  _optOptimizeParens = True,
  _optShowNameId = False,
  _optIndent = 2
  }

-- | Pretty prints a top module.
prettyTopModule :: Options -> Module 'Scoped 'ModuleTop -> Doc Ann
prettyTopModule opts = run . runReader opts . ppModule

infixl 7 <+?>
(<+?>) :: Doc ann -> Maybe (Doc ann) -> Doc ann
(<+?>) a = maybe a (a <+>)

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

kwWildcard :: Doc Ann
kwWildcard = keyword "_"

kwPrefix :: Doc Ann
kwPrefix = keyword "prefix"

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
  SModuleTop -> ppTopModulePath x
  SModuleLocal -> ppSSymbol x

ppSymbol :: Members '[Reader Options] r => Symbol -> Sem r (Doc Ann)
ppSymbol (Sym t) = return (pretty t)

ppStatement :: Members '[Reader Options] r => Statement 'Scoped -> Sem r (Doc Ann)
ppStatement s = case s of
  StatementOperator op -> ppOperatorSyntaxDef op
  StatementTypeSignature sig -> ppTypeSignature sig
  StatementImport i -> ppImport i
  StatementDataType d -> ppDataTypeDef d
  StatementModule m -> ppModule m
  StatementOpenModule o -> ppOpen o
  StatementFunctionClause c -> ppFunctionClause c
  StatementAxiom a -> ppAxiom a
  StatementEval e -> ppEval e
  StatementPrint p -> ppPrint p

ppTopModulePath :: Members '[Reader Options] r => TopModulePath -> Sem r (Doc Ann)
ppTopModulePath TopModulePath {..} = dotted <$> mapM ppSymbol (pathParts modulePathDir ++ [modulePathName])

endSemicolon :: Doc Ann -> Doc Ann
endSemicolon x = x <> kwSemicolon

ppModule :: (SingI t, Members '[Reader Options] r) => Module 'Scoped t -> Sem r (Doc Ann)
ppModule Module {..} = do
  moduleBody' <-  mapM (fmap endSemicolon . ppStatement) moduleBody >>= indented . vsep
  modulePath' <- ppModulePathType modulePath
  return $ kwModule <+> modulePath' <> kwSemicolon <> line
   <> moduleBody' <> line
   <> kwEnd <> kwSemicolon

ppOperatorSyntaxDef :: Members '[Reader Options] r => OperatorSyntaxDef -> Sem r (Doc Ann)
ppOperatorSyntaxDef OperatorSyntaxDef {..} = do
  opSymbol' <- ppSymbol opSymbol
  return $ ppFixity opFixity <+> opSymbol'
  where
  ppFixity :: Fixity -> Doc Ann
  ppFixity Fixity {..} =
    ppArity <+> pretty fixityPrecedence
    where
      ppArity :: Doc Ann
      ppArity = case fixityArity of
        Unary p -> case p of
          AssocPrefix -> kwPrefix
          AssocPostfix -> kwPostfix
        Binary p -> case p of
          AssocRight -> kwInfixr
          AssocLeft -> kwInfixl
          AssocNone -> kwInfix

ppDataConstructorDef :: Members '[Reader Options] r => DataConstructorDef 'Scoped -> Sem r (Doc Ann)
ppDataConstructorDef DataConstructorDef {..} = do
  constructorName' <- ppSSymbol constructorName
  constructorType' <- ppExpression constructorType
  return $ constructorName' <+> kwColon <+> constructorType'

ppDataTypeDef :: forall r. Members '[Reader Options] r => DataTypeDef 'Scoped -> Sem r (Doc Ann)
ppDataTypeDef DataTypeDef {..} = do
  dataTypeName' <- ppSSymbol dataTypeName
  dataTypeParameters' <- hsep <$> mapM ppDataTypeParameter dataTypeParameters
  dataTypeType' <- ppTypeType
  dataTypeConstructors' <- ppBlock ppDataConstructorDef dataTypeConstructors
  return $ kwInductive <+> dataTypeName' <+> dataTypeParameters' <+?> dataTypeType'
    <+> dataTypeConstructors'
  where
  ppTypeType :: Sem r (Maybe (Doc Ann))
  ppTypeType = case dataTypeType of
    Nothing -> return Nothing
    Just e -> Just . (kwColon <+>) <$> ppExpression e
  ppDataTypeParameter :: DataTypeParameter 'Scoped -> Sem r (Doc Ann)
  ppDataTypeParameter DataTypeParameter {..} = do
    dataTypeParameterName' <- ppSSymbol dataTypeParameterName
    dataTypeParameterType' <- ppExpression dataTypeParameterType
    return $ parens (dataTypeParameterName' <+> kwColon <+> dataTypeParameterType')

dotted :: [Doc Ann] -> Doc Ann
dotted = concatWith (surround kwDot)

ppQualified :: Members '[Reader Options] r => QualifiedName -> Sem r (Doc Ann)
ppQualified QualifiedName {..} = do
  qualifiedPath' <- ppPath qualifiedPath
  qualifiedSymbol' <- ppSymbol qualifiedSymbol
  return (dotted [qualifiedPath', qualifiedSymbol'])

ppPath :: Members '[Reader Options] r => Path -> Sem r (Doc Ann)
ppPath = fmap dotted . mapM ppSymbol . pathParts

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

ppOpen :: forall r. Members '[Reader Options] r => OpenModule -> Sem r (Doc Ann)
ppOpen OpenModule {..} = do
  openModuleName' <- ppQualified openModuleName
  openUsingHiding' <- ppUsingHiding
  return $ keyword "open" <+> openModuleName' <+> openUsingHiding'
  where
  ppUsingHiding :: Sem r (Doc Ann)
  ppUsingHiding = return $ pretty ("TODO" :: Text)

ppTypeSignature :: Members '[Reader Options] r => TypeSignature 'Scoped -> Sem r (Doc Ann)
ppTypeSignature TypeSignature {..} = do
  sigName' <- ppSSymbol sigName
  sigType' <- ppExpression sigType
  return $ sigName' <+> kwColon <+> sigType'

ppFunction :: forall r. Members '[Reader Options] r => Function 'Scoped -> Sem r (Doc Ann)
ppFunction Function {..} = do
  funParameter' <- ppFunParameter funParameter
  funReturn' <- ppExpressionAtom funReturn
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
      Nothing -> ppExpressionAtom paramType
      Just n -> do
        paramName' <- ppSSymbol n
        paramType' <- ppExpression paramType
        return $ parens (paramName' <+> ppUsage paramUsage <+> paramType')

ppUniverse :: Members '[Reader Options] r => Universe -> Sem r (Doc Ann)
ppUniverse (Universe n) = return $ kwType <+> pretty n

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
  clausePatterns' <- hsep <$> mapM ppPattern clausePatterns
  clauseBody' <- ppExpression clauseBody
  clauseWhere' <- sequence $ ppWhereBlock <$> clauseWhere
  return $ clauseOwnerFunction' <+> clausePatterns' <+> kwAssignment <+> clauseBody' 
   <+?> (((line <> kwWhere)  <+>) <$> clauseWhere')
  where
  ppWhereBlock :: WhereBlock 'Scoped -> Sem r (Doc Ann)
  ppWhereBlock WhereBlock {..} = ppBlock ppWhereClause whereClauses
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

ppImport :: Members '[Reader Options] r => Import 'Scoped -> Sem r (Doc Ann)
ppImport (Import (Module {..})) = do
  modulePath' <- ppTopModulePath modulePath
  return $ kwImport <+> modulePath'

ppPattern :: forall r. Members '[Reader Options] r => Pattern -> Sem r (Doc Ann)
ppPattern = goAtom
  where
  isAtomicPat :: Pattern -> Bool
  isAtomicPat p = case p of
    PatternVariable {} -> True
    PatternApplication {} -> False
    PatternConstructor {} -> True
    PatternInfixApplication {} -> False
    PatternPostfixApplication {} -> False
    PatternPrefixApplication {} -> False
    PatternWildcard -> True
    PatternEmpty -> True
  goAtom :: Pattern -> Sem r (Doc Ann)
  goAtom p = do 
    p' <- go p
    return $ if isAtomicPat p then p' else parens p'
  go :: Pattern -> Sem r (Doc Ann)
  go p = case p of
    PatternVariable v -> ppSSymbol v
    PatternApplication l r -> do
      l' <- goAtom l
      r' <- goAtom r
      return $ l' <+> r'
    PatternWildcard -> return kwWildcard
    PatternEmpty -> return $ parens mempty
    PatternConstructor constr -> ppSName constr
    PatternInfixApplication i -> ppPatternInfixApp i
    PatternPrefixApplication i -> ppPatternPrefixApp i
    PatternPostfixApplication i -> ppPatternPostfixApp i

  ppPatternInfixApp :: PatternInfixApp -> Sem r (Doc Ann)
  ppPatternInfixApp PatternInfixApp {..} = do
    patInfixConstructor' <- ppSName patInfixConstructor
    patInfixLeft' <- goAtom patInfixLeft
    patInfixRight' <- goAtom patInfixRight
    return $ patInfixLeft' <+> patInfixConstructor' <+> patInfixRight'

  ppPatternPrefixApp :: PatternPrefixApp -> Sem r (Doc Ann)
  ppPatternPrefixApp PatternPrefixApp {..} = do
    patPrefixConstructor' <- ppSName patPrefixConstructor
    patPrefixParameter' <- goAtom patPrefixParameter
    return $ patPrefixConstructor' <+> patPrefixParameter'

  ppPatternPostfixApp :: PatternPostfixApp -> Sem r (Doc Ann)
  ppPatternPostfixApp PatternPostfixApp {..} = do
    patPostfixConstructor' <- ppSName patPostfixConstructor
    patPostfixParameter' <- goAtom patPostfixParameter
    return $ patPostfixConstructor' <+> patPostfixParameter'

ppExpressionAtom :: forall r. Members '[Reader Options] r => Expression -> Sem r (Doc Ann)
ppExpressionAtom e = do 
    e' <- ppExpression e
    return $ if isAtomic e then e' else parens e'

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
ppInfixApplication InfixApplication {..} = do 
  infixAppLeft' <- ppExpressionAtom infixAppLeft
  infixAppOperator' <- ppSName infixAppOperator 
  infixAppRight' <- ppExpressionAtom infixAppRight
  return $ infixAppLeft' <+> infixAppOperator' <+> infixAppRight'

ppPostfixApplication :: forall r. Members '[Reader Options] r => PostfixApplication -> Sem r (Doc Ann)
ppPostfixApplication PostfixApplication {..} = do 
  postfixAppParameter' <- ppExpressionAtom postfixAppParameter
  postfixAppOperator' <- ppSName postfixAppOperator 
  return $ postfixAppParameter' <+> postfixAppOperator'


ppExpression :: forall r. Members '[Reader Options] r => Expression -> Sem r (Doc Ann)
ppExpression = go
  where
  ppApplication :: Application -> Sem r (Doc Ann)
  ppApplication (Application l r) = do
    l' <- goAtom l
    r' <- goAtom r
    return $ l' <+> r'
  goAtom :: Expression -> Sem r (Doc Ann)
  goAtom e = do 
    e' <- go e
    return $ if isAtomic e then e' else parens e'
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
