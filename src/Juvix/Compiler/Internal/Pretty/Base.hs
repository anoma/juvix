module Juvix.Compiler.Internal.Pretty.Base
  ( module Juvix.Compiler.Internal.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Internal.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Data.InstanceInfo (instanceInfoResult, instanceTableMap)
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Data.NameDependencyInfo
import Juvix.Compiler.Internal.Data.TypedHole
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty.Options
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types (Arity)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew.Arity qualified as New
import Juvix.Compiler.Store.Internal.Data.InfoTable
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
  ppCode = return . pretty

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
    v' <- ppCode (l ^. slambdaBinder . sbinderVar)
    return $ kwSimpleLambda <+> braces (v' <+> kwAssign <+> b')

instance PrettyCode Application where
  ppCode a = do
    l' <- ppLeftExpression appFixity (a ^. appLeft)
    r' <- case a ^. appImplicit of
      Explicit -> ppRightExpression appFixity (a ^. appRight)
      Implicit -> braces <$> ppCode (a ^. appRight)
      ImplicitInstance -> doubleBraces <$> ppCode (a ^. appRight)
    return $ l' <+> r'

instance PrettyCode TypedExpression where
  ppCode e = do
    e' <- ppCode (e ^. typedExpression)
    ty' <- ppCode (e ^. typedType)
    return (e' <+> kwColon <+> ty')

instance PrettyCode SmallUniverse where
  ppCode _ = return kwType

instance PrettyCode Expression where
  ppCode = \case
    ExpressionIden i -> ppCode i
    ExpressionHole h -> ppCode h
    ExpressionInstanceHole h -> ppCode h
    ExpressionApplication a -> ppCode a
    ExpressionFunction f -> ppCode f
    ExpressionUniverse u -> ppCode u
    ExpressionLiteral l -> ppCode l
    ExpressionSimpleLambda l -> ppCode l
    ExpressionLambda l -> ppCode l
    ExpressionLet l -> ppCode l
    ExpressionCase c -> ppCode c

instance PrettyCode CaseBranch where
  ppCode CaseBranch {..} = do
    pat <- ppCode _caseBranchPattern
    e <- ppCode _caseBranchExpression
    return $ kwPipe <+> pat <+> kwAssign <+> e

instance PrettyCode Case where
  ppCode Case {..} = do
    exp <- ppCode _caseExpression
    branches <- indent' . vsep <$> mapM ppCode _caseBranches
    return $ parensIf _caseParens (kwCase <+> exp <> line <> branches)

instance PrettyCode Let where
  ppCode l = do
    letClauses' <- blockIndent <$> ppBlock (l ^. letClauses)
    letExpression' <- ppCode (l ^. letExpression)
    return $ kwLet <+> letClauses' <+> kwIn <+> letExpression'

ppMutual :: (Member (Reader Options) r, PrettyCode a) => NonEmpty a -> Sem r (Doc Ann)
ppMutual l = do
  defs' <- case l of
    b :| [] -> ppCode b
    t -> do
      b' <- vsep2 <$> mapM ppCode t
      return (braces (line <> indent' b' <> line))
  return (kwMutual <+> defs')

instance PrettyCode Arity where
  ppCode = return . pretty

instance PrettyCode ApplicationArg where
  ppCode ApplicationArg {..} =
    implicitDelim _appArgIsImplicit <$> ppCode _appArg

instance PrettyCode LetClause where
  ppCode :: forall r. (Member (Reader Options) r) => LetClause -> Sem r (Doc Ann)
  ppCode = \case
    LetFunDef f -> do
      fun' <- ppCode f
      return (kwNotMutual <+> fun')
    LetMutualBlock b -> ppMutual (b ^. mutualLet)

instance PrettyCode Literal where
  ppCode =
    return . \case
      LitNumeric n -> pretty n
      LitNatural n -> pretty n
      LitInteger n -> pretty n
      LitString s -> ppStringLit s

ppPipeBlock :: (PrettyCode a, Members '[Reader Options] r, Traversable t) => t a -> Sem r (Doc Ann)
ppPipeBlock items = vsep <$> mapM (fmap (kwPipe <+>) . ppCode) items

instance (PrettyCode a, PrettyCode b, PrettyCode c) => PrettyCode (a, b, c) where
  ppCode (a, b, c) = do
    a' <- ppCode a
    b' <- ppCode b
    c' <- ppCode c
    return $ tuple [a', b', c']

header :: Text -> Doc Ann
header = annotate AnnImportant . pretty

instance PrettyCode NameDependencyInfo where
  ppCode DependencyInfo {..} = do
    edges' <- vsep <$> mapM ppCode _depInfoEdgeList
    reachable' <- ppCode (toList _depInfoReachable)
    topsort' <- ppCode _depInfoTopSort
    return $
      header "Edges:"
        <> edges'
        <> line
        <> header "Reachable:"
        <> reachable'
        <> line
        <> header "Topologically sorted:"
        <> topsort'
        <> line

instance PrettyCode LambdaClause where
  ppCode LambdaClause {..} = do
    lambdaParameters' <- hsep <$> mapM ppCodeAtom _lambdaPatterns
    lambdaBody' <- ppCode _lambdaBody
    return $ lambdaParameters' <+> kwAssign <+> lambdaBody'

instance PrettyCode Lambda where
  ppCode Lambda {..} = do
    lambdaClauses' <- ppPipeBlock _lambdaClauses
    lambdaType' <- mapM ppCode _lambdaType
    return $ kwLambda <+> (fmap (kwColon <+>) lambdaType') <?+> braces lambdaClauses'

instance (PrettyCode a) => PrettyCode (WithLoc a) where
  ppCode = ppCode . (^. withLocParam)

instance PrettyCode FunctionParameter where
  ppCode FunctionParameter {..} = do
    case _paramName of
      Nothing -> delimIf _paramImplicit False <$> ppLeftExpression funFixity _paramType
      Just n -> do
        paramName' <- ppCode n
        paramType' <- ppCode _paramType
        return $ implicitDelim _paramImplicit (paramName' <+> kwColon <+> paramType')

instance PrettyCode Function where
  ppCode (Function l r) = do
    funParameter' <- ppCode l
    funReturn' <- ppRightExpression funFixity r
    return $ funParameter' <+> kwArrow <+> funReturn'

instance PrettyCode InstanceHole where
  ppCode h = do
    showNameId <- asks (^. optShowNameIds)
    return (addNameIdTag showNameId (h ^. iholeId) kwHole)

instance PrettyCode Hole where
  ppCode h = do
    showNameId <- asks (^. optShowNameIds)
    return (addNameIdTag showNameId (h ^. holeId) kwHole)

instance PrettyCode ConstructorDef where
  ppCode c = do
    constructorName' <- ppCode (c ^. inductiveConstructorName)
    ty' <- ppCode (c ^. inductiveConstructorType)
    return (constructorName' <+> kwColon <+> ty')

ppBlock ::
  (PrettyCode a, Members '[Reader Options] r, Traversable t) =>
  t a ->
  Sem r (Doc Ann)
ppBlock items = vsep . toList <$> mapM ppCode items

instance PrettyCode InductiveParameter where
  ppCode InductiveParameter {..} = do
    v' <- ppCode _inductiveParamName
    ty' <- ppCode _inductiveParamType
    return $ parens (v' <+> kwColon <+> ty')

instance PrettyCode BuiltinInductive where
  ppCode = return . annotate AnnKeyword . pretty

instance PrettyCode InductiveDef where
  ppCode d = do
    inductiveName' <- ppCode (d ^. inductiveName)
    builtin' <- fmap (kwBuiltin <+>) <$> mapM ppCode (d ^. inductiveBuiltin)
    params <- hsepMaybe <$> mapM ppCode (d ^. inductiveParameters)
    inductiveConstructors' <- mapM ppCode (d ^. inductiveConstructors)
    let rhs = indent' $ align $ concatWith (\a b -> a <> line <> kwPipe <+> b) inductiveConstructors'
    return $ builtin' <?+> kwInductive <+> inductiveName' <+?> params <+> kwEquals <> line <> rhs

instance PrettyCode PatternArg where
  ppCode (PatternArg i n p) = do
    n' <- traverse ppCode n
    p' <- ppCode p
    return $ (n' <&> (<> kwAt)) ?<> delimIf i (isJust n && not (isAtomic p)) p'

instance PrettyCode ConstructorApp where
  ppCode c = do
    constr' <- ppCode (c ^. constrAppConstructor)
    params' <- mapM ppCodeAtom (c ^. constrAppParameters)
    mty' <- mapM ppCode (c ^. constrAppType)
    case mty' of
      Nothing -> return $ hsep (constr' : params')
      Just ty' -> return $ parens (hsep (constr' : params') <+> kwColon <+> ty')

instance PrettyCode WildcardConstructor where
  ppCode p = do
    constr' <- ppCode (p ^. wildcardConstructor)
    return $ constr' <> kwAt <> braces mempty

instance PrettyCode Pattern where
  ppCode p = case p of
    PatternVariable v -> ppCode v
    PatternWildcardConstructor v -> ppCode v
    PatternConstructorApp a -> ppCode a

instance PrettyCode BuiltinFunction where
  ppCode = return . annotate AnnKeyword . pretty

instance PrettyCode FunctionDef where
  ppCode f = do
    builtin' <- fmap (kwBuiltin <+>) <$> mapM ppCode (f ^. funDefBuiltin)
    funDefName' <- ppCode (f ^. funDefName)
    funDefType' <- ppCode (f ^. funDefType)
    body' <- ppCode (f ^. funDefBody)
    return $
      builtin'
        <?+> funDefName'
          <+> kwColon
          <+> funDefType'
            <> oneLineOrNext (kwAssign <+> body')

instance PrettyCode PreLetStatement where
  ppCode = \case
    PreLetFunctionDef f -> ppCode f

instance PrettyCode Import where
  ppCode i = do
    name' <- ppCode (i ^. importModuleName)
    return $ kwImport <+> name'

instance PrettyCode BuiltinAxiom where
  ppCode = return . annotate AnnKeyword . pretty

instance PrettyCode AxiomDef where
  ppCode AxiomDef {..} = do
    axiomName' <- ppCode _axiomName
    builtin' <- fmap (kwBuiltin <+>) <$> mapM ppCode _axiomBuiltin
    axiomType' <- ppCode _axiomType
    return $ builtin' <?+> kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

instance PrettyCode MutualStatement where
  ppCode = \case
    StatementInductive d -> ppCode d
    StatementFunction d -> ppCode d
    StatementAxiom d -> ppCode d

instance PrettyCode MutualBlock where
  ppCode (MutualBlock funs) = ppMutual funs

instance PrettyCode MutualBlockLet where
  ppCode (MutualBlockLet funs) =
    vsep2 <$> mapM ppCode funs

instance PrettyCode ModuleBody where
  ppCode m = do
    includes <- mapM ppCode (m ^. moduleImports)
    everything <- mapM ppCode (m ^. moduleStatements)
    return (vsep includes <> line <> line <> vsep2 everything)

instance PrettyCode Module where
  ppCode :: (Member (Reader Options) r) => Module -> Sem r (Doc Ann)
  ppCode m = do
    name' <- ppCode (m ^. moduleName)
    body' <- ppCode (m ^. moduleBody)
    return $
      kwModule
        <+> name'
          <> kwSemicolon
          <> line
          <> line
          <> body'
          <> line

instance PrettyCode Interval where
  ppCode = return . annotate AnnCode . pretty

instance PrettyCode New.ArgId where
  ppCode a = case a ^. New.argIdName . unIrrelevant of
    Nothing -> do
      f' <- ppCode (a ^. New.argIdFunctionName)
      return (ordinal (a ^. New.argIdIx) <+> "argument of" <+> f')
    Just n -> do
      n' <- ppCode n
      loc' <- ppCode (getLoc n)
      return (n' <+> "at" <+> loc')

instance PrettyCode New.ArityParameter where
  ppCode = return . pretty

instance (PrettyCode a, PrettyCode b) => PrettyCode (Either a b) where
  ppCode = \case
    Left l -> do
      l' <- ppCode l
      return ("Left" <+> l')
    Right r -> do
      r' <- ppCode r
      return ("Right" <+> r')

instance PrettyCode LocalVars where
  ppCode LocalVars {..} = ppCode (HashMap.toList _localTypes)

instance PrettyCode TypedHole where
  ppCode TypedHole {..} = do
    h <- ppCode _typedHoleHole
    ty <- ppCode _typedHoleType
    vars <- ppCode _typedHoleLocalVars
    return (h <+> kwColon <+> ty <> kwAt <> vars)

instance PrettyCode InfoTable where
  ppCode tbl = do
    inds <- ppCode (HashMap.keys (tbl ^. infoInductives))
    constrs <- ppCode (HashMap.keys (tbl ^. infoConstructors))
    funs <- ppCode (HashMap.keys (tbl ^. infoFunctions))
    insts <- ppCode $ map (map (^. instanceInfoResult)) $ HashMap.elems (tbl ^. infoInstances . instanceTableMap)
    return $
      header "InfoTable"
        <> "\n========="
        <> header "\nInductives: "
        <> inds
        <> header "\nConstructors: "
        <> constrs
        <> header "\nFunctions: "
        <> funs
        <> header "\nInstances: "
        <> insts

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

instance (PrettyCode a) => PrettyCode (Maybe a) where
  ppCode = \case
    Nothing -> return "Nothing"
    Just p -> ("Just" <+>) <$> ppCode p

tuple :: [Doc ann] -> Doc ann
tuple = encloseSep "(" ")" ", "

instance (PrettyCode a, PrettyCode b) => PrettyCode (a, b) where
  ppCode (x, y) = do
    x' <- ppCode x
    y' <- ppCode y
    return $ tuple [x', y']

instance (PrettyCode a) => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode (toList x)
    return $ encloseSep "[" "]" ", " cs

instance (PrettyCode a) => PrettyCode (NonEmpty a) where
  ppCode x = ppCode (toList x)

instance PrettyCode IsImplicit where
  ppCode = return . pretty
