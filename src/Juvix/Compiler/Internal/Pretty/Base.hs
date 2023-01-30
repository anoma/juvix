module Juvix.Compiler.Internal.Pretty.Base
  ( module Juvix.Compiler.Internal.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Internal.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Prelude

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

runPrettyCode :: (PrettyCode c) => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

instance PrettyCode NameId where
  ppCode (NameId k) = return (pretty k)

instance PrettyCode Name where
  ppCode n = do
    showNameId <- asks (^. optShowNameIds)
    return (prettyName showNameId n)

instance PrettyCode Iden where
  ppCode :: (Member (Reader Options) r) => Iden -> Sem r (Doc Ann)
  ppCode i = case i of
    IdenFunction na -> ppCode na
    IdenConstructor na -> ppCode na
    IdenVar na -> ppCode na
    IdenAxiom a -> ppCode a
    IdenInductive a -> ppCode a

instance PrettyCode SimpleLambda where
  ppCode l = do
    b' <- ppCode (l ^. slambdaBody)
    v' <- ppCode (l ^. slambdaVar)
    return $ kwLambda <+> braces (v' <+> kwAssign <+> b')

instance PrettyCode Application where
  ppCode a = do
    l' <- ppLeftExpression appFixity (a ^. appLeft)
    r' <- case a ^. appImplicit of
      Explicit -> ppRightExpression appFixity (a ^. appRight)
      Implicit -> braces <$> ppCode (a ^. appRight)
    return $ l' <+> r'

instance PrettyCode TypedExpression where
  ppCode e = ppCode (e ^. typedExpression)

instance PrettyCode SmallUniverse where
  ppCode _ = return kwType

instance PrettyCode Expression where
  ppCode = \case
    ExpressionIden i -> ppCode i
    ExpressionHole h -> ppCode h
    ExpressionApplication a -> ppCode a
    ExpressionFunction f -> ppCode f
    ExpressionUniverse u -> ppCode u
    ExpressionLiteral l -> return (pretty l)
    ExpressionSimpleLambda l -> ppCode l
    ExpressionLambda l -> ppCode l
    ExpressionLet l -> ppCode l

instance PrettyCode Let where
  ppCode l = do
    letClauses' <- ppBlock (l ^. letClauses)
    letExpression' <- ppCode (l ^. letExpression)
    return $ kwLet <+> letClauses' <+> kwIn <+> letExpression'

instance PrettyCode LetClause where
  ppCode = \case
    LetFunDef f -> ppCode f

ppPipeBlock :: (PrettyCode a, Members '[Reader Options] r, Traversable t) => t a -> Sem r (Doc Ann)
ppPipeBlock items = vsep <$> mapM (fmap (kwPipe <+>) . ppCode) items

instance PrettyCode LambdaClause where
  ppCode LambdaClause {..} = do
    lambdaParameters' <- hsep <$> mapM ppCodeAtom _lambdaPatterns
    lambdaBody' <- ppCode _lambdaBody
    return $ lambdaParameters' <+> kwAssign <+> lambdaBody'

instance PrettyCode Lambda where
  ppCode Lambda {..} = do
    lambdaClauses' <- ppPipeBlock _lambdaClauses
    return $ kwLambda <+> lambdaClauses'

instance PrettyCode a => PrettyCode (WithLoc a) where
  ppCode = ppCode . (^. withLocParam)

instance PrettyCode BackendItem where
  ppCode BackendItem {..} = do
    backend <- ppCode _backendItemBackend
    return $
      backend <+> kwMapsto <+> pretty _backendItemCode

instance PrettyCode FunctionParameter where
  ppCode FunctionParameter {..} = do
    case _paramName of
      Nothing -> ppLeftExpression funFixity _paramType
      Just n -> do
        paramName' <- ppCode n
        paramType' <- ppCode _paramType
        return $ implicitDelim _paramImplicit (paramName' <+> kwColon <+> paramType')

instance PrettyCode Function where
  ppCode (Function l r) = do
    funParameter' <- ppCode l
    funReturn' <- ppRightExpression funFixity r
    return $ funParameter' <+> kwArrow <+> funReturn'

instance PrettyCode Hole where
  ppCode h = do
    showNameId <- asks (^. optShowNameIds)
    return (addNameIdTag showNameId (h ^. holeId) kwHole)

instance PrettyCode InductiveConstructorDef where
  ppCode c = do
    constructorName' <- ppCode (c ^. inductiveConstructorName)
    constructorParameters' <- mapM ppCodeAtom (c ^. inductiveConstructorParameters)
    return (hsep $ constructorName' : constructorParameters')

ppBlock ::
  (PrettyCode a, Members '[Reader Options] r, Traversable t) =>
  t a ->
  Sem r (Doc Ann)
ppBlock items = bracesIndent . vsep . toList <$> mapM ppCode items

instance PrettyCode InductiveParameter where
  ppCode (InductiveParameter v) = do
    v' <- ppCode v
    return $ parens (v' <+> kwColon <+> kwType)

instance PrettyCode InductiveDef where
  ppCode d = do
    inductiveName' <- ppCode (d ^. inductiveName)
    params <- hsepMaybe <$> mapM ppCode (d ^. inductiveParameters)
    inductiveConstructors' <- mapM ppCode (d ^. inductiveConstructors)
    let rhs = indent' $ align $ concatWith (\a b -> a <> line <> kwPipe <+> b) inductiveConstructors'
    return $ kwData <+> inductiveName' <+?> params <+> kwEquals <> line <> rhs

instance PrettyCode PatternArg where
  ppCode (PatternArg i n p) = do
    n' <- traverse ppCode n
    p' <- ppCode p
    return $ (n' <&> (<> kwAt)) ?<> delimIf i (isJust n && not (isAtomic p)) p'

instance PrettyCode ConstructorApp where
  ppCode c = do
    constr' <- ppCode (c ^. constrAppConstructor)
    params' <- mapM ppCodeAtom (c ^. constrAppParameters)
    return $ hsep $ constr' : params'

instance PrettyCode Pattern where
  ppCode p = case p of
    PatternVariable v -> ppCode v
    PatternConstructorApp a -> ppCode a

instance PrettyCode FunctionDef where
  ppCode f = do
    funDefName' <- ppCode (f ^. funDefName)
    funDefType' <- ppCode (f ^. funDefType)
    clauses' <- mapM ppCode (f ^. funDefClauses)
    return $
      funDefName'
        <+> kwColonColon
        <+> funDefType'
          <> line
          <> vsep (toList clauses')

instance PrettyCode FunctionClause where
  ppCode c = do
    funName <- ppCode (c ^. clauseName)
    clausePatterns' <- hsepMaybe <$> mapM ppCodeAtom (c ^. clausePatterns)
    clauseBody' <- ppCode (c ^. clauseBody)
    return $ funName <+?> clausePatterns' <+> kwAssign <+> clauseBody'

instance PrettyCode Backend where
  ppCode = \case
    BackendGhc -> return kwGhc
    BackendC -> return kwC

instance PrettyCode ForeignBlock where
  ppCode ForeignBlock {..} = do
    _foreignBackend' <- ppCode _foreignBackend
    return $
      kwForeign
        <+> _foreignBackend'
        <+> lbrace
          <> line
          <> pretty _foreignCode
          <> line
          <> rbrace

instance PrettyCode Include where
  ppCode i = do
    name' <- ppCode (i ^. includeModule . moduleName)
    return $ kwInclude <+> name'

instance PrettyCode AxiomDef where
  ppCode AxiomDef {..} = do
    axiomName' <- ppCode _axiomName
    axiomType' <- ppCode _axiomType
    return $ kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

instance PrettyCode MutualBlock where
  ppCode (MutualBlock funs) =
    vsep2 <$> mapM ppCode funs

instance PrettyCode Statement where
  ppCode = \case
    StatementForeign f -> ppCode f
    StatementFunction f -> ppCode f
    StatementInductive f -> ppCode f
    StatementAxiom f -> ppCode f
    StatementInclude i -> ppCode i

instance PrettyCode ModuleBody where
  ppCode m = do
    everything <- mapM ppCode (m ^. moduleStatements)
    return $ vsep2 everything

instance PrettyCode Module where
  ppCode m = do
    name' <- ppCode (m ^. moduleName)
    body' <- ppCode (m ^. moduleBody)
    return $
      kwModule
        <+> name'
        <+> kwWhere
          <> line
          <> line
          <> body'
          <> line

instance PrettyCode TypeCallIden where
  ppCode = \case
    InductiveIden n -> ppCode n
    FunctionIden n -> ppCode n

instance PrettyCode Caller where
  ppCode = \case
    CallerInductive n -> ppCode n
    CallerAxiom n -> ppCode n
    CallerFunction n -> ppCode n

instance PrettyCode ConcreteTypeCall where
  ppCode = ppCode . fmap (^. unconcreteType)

instance PrettyCode TypeCall where
  ppCode (TypeCall' f args) = do
    f' <- ppCode f
    args' <- mapM ppCodeAtom args
    return $ f' <+> hsep args'

instance PrettyCode TypeCallsMap where
  ppCode m = do
    let title = keyword "Type Calls Map:"
        elems :: [(Caller, TypeCall)]
        elems =
          [(caller, t) | (caller, l) <- HashMap.toList (m ^. typeCallsMap), t <- toList l]
    elems' <- mapM ppCodeElem (sortOn fst elems)
    return $ title <> line <> vsep elems' <> line
    where
      ppCodeElem :: (Member (Reader Options) r) => (Caller, TypeCall) -> Sem r (Doc Ann)
      ppCodeElem (caller, t) = do
        caller' <- ppCode caller
        t' <- ppCode t
        return $ caller' <+> kwMapsto <+> t'

instance PrettyCode TypeCalls where
  ppCode m = do
    let title = keyword "Propagated Type Calls:"
        elems = sortOn (^. typeCallIden) (concatMap HashMap.keys (toList (m ^. typeCallSet)))
    elems' <- mapM ppCode elems
    return $ title <> line <> vsep elems' <> line

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

instance (PrettyCode a) => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode (toList x)
    return $ encloseSep "(" ")" ", " cs

instance (PrettyCode a) => PrettyCode (NonEmpty a) where
  ppCode x = ppCode (toList x)

instance PrettyCode ConcreteType where
  ppCode ConcreteType {..} = ppCode _unconcreteType
