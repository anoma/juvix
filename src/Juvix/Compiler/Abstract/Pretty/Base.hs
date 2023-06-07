module Juvix.Compiler.Abstract.Pretty.Base
  ( module Juvix.Compiler.Abstract.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Abstract.Pretty.Options,
  )
where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Abstract.Pretty.Options
import Juvix.Compiler.Concrete.Print.Base qualified as S
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

toSOptions :: Options -> S.Options
toSOptions Options {..} =
  S.defaultOptions
    { S._optShowNameIds = _optShowNameIds
    }

class PrettyCode c where
  ppCode :: (Members '[Reader Options] r) => c -> Sem r (Doc Ann)

ppSCode :: (Members '[Reader Options] r, S.PrettyPrint c) => c -> Sem r (Doc Ann)
ppSCode c = do
  opts <- asks toSOptions
  return (S.docNoComments opts c)

ppDefault :: (PrettyCode c) => c -> Doc Ann
ppDefault = runPrettyCode defaultOptions

runPrettyCode :: (PrettyCode c) => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

kwQuestion :: Doc Ann
kwQuestion = keyword Str.questionMark

kwWaveArrow :: Doc Ann
kwWaveArrow = keyword Str.waveArrow

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

ppCodeAtom ::
  (HasAtomicity c, PrettyCode c, Members '[Reader Options] r) =>
  c ->
  Sem r (Doc Ann)
ppCodeAtom c = do
  p' <- ppCode c
  return $ if isAtomic c then p' else parens p'

instance PrettyCode Iden where
  ppCode = ppCode . idenName

instance PrettyCode Application where
  ppCode (Application l r i) = do
    l' <- ppLeftExpression appFixity l
    r' <- case i of
      Explicit -> ppRightExpression appFixity r
      Implicit -> implicitDelim i <$> ppCode r
    return $ l' <+> r'

instance PrettyCode Universe where
  ppCode Universe {..} = return $ kwType <+?> (pretty <$> _universeLevel)

instance PrettyCode PatternArg where
  ppCode (PatternArg i n p) = do
    n' <- traverse ppCode n
    p' <- ppCode p
    return $ (n' <&> (<> kwAt)) ?<> delimIf i (isJust n && not (isAtomic p)) p'

instance PrettyCode LambdaClause where
  ppCode LambdaClause {..} = do
    lambdaParameters' <- hsep . toList <$> mapM ppCode _lambdaParameters
    lambdaBody' <- ppCode _lambdaBody
    return $ lambdaParameters' <+> kwAssign <+> lambdaBody'

instance PrettyCode Lambda where
  ppCode Lambda {..} = do
    lambdaClauses' <- ppPipeBlock _lambdaClauses
    return $ kwLambda <+> lambdaClauses'

ppPipeBlock :: (PrettyCode a, Members '[Reader Options] r, Traversable t) => t a -> Sem r (Doc Ann)
ppPipeBlock items = vsep <$> mapM (fmap (kwPipe <+>) . ppCode) items

ppBlock :: (PrettyCode a, Members '[Reader Options] r, Traversable t) => t a -> Sem r (Doc Ann)
ppBlock items = vsep . toList <$> mapM (fmap endSemicolon . ppCode) items

instance PrettyCode ConstructorApp where
  ppCode (ConstructorApp ctr args) = do
    ctr' <- ppCode ctr
    if
        | null args -> do
            args' <- hsep <$> mapM ppCode args
            return (parens (ctr' <+> args'))
        | otherwise -> return ctr'

instance PrettyCode Pattern where
  ppCode = \case
    PatternVariable v -> ppCode v
    PatternWildcard {} -> return kwWildcard
    PatternEmpty {} -> return $ parens mempty
    PatternConstructorApp constr -> ppCode constr

instance PrettyCode Expression where
  ppCode e = case e of
    ExpressionIden i -> ppCode i
    ExpressionUniverse u -> ppCode u
    ExpressionApplication a -> ppCode a
    ExpressionFunction f -> ppCode f
    ExpressionLiteral l -> ppSCode l
    ExpressionHole h -> ppSCode h
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

instance PrettyCode FunctionClause where
  ppCode c = do
    funName <- ppCode (c ^. clauseName)
    clausePatterns' <- hsepMaybe <$> mapM ppCodeAtom (c ^. clausePatterns)
    clauseBody' <- ppCode (c ^. clauseBody)
    return $ funName <+?> clausePatterns' <+> kwAssign <+> clauseBody'

instance PrettyCode FunctionDef where
  ppCode f = do
    funDefName' <- ppCode (f ^. funDefName)
    funDefType' <- ppCode (f ^. funDefTypeSig)
    clauses' <- mapM ppCode (f ^. funDefClauses)
    return $
      funDefName'
        <+> kwColonColon
        <+> funDefType'
          <> line
          <> vsep (toList clauses')

instance PrettyCode LetClause where
  ppCode = \case
    LetFunDef f -> ppCode f

instance PrettyCode Let where
  ppCode l = do
    letClauses' <- blockIndent <$> ppBlock (l ^. letClauses)
    letExpression' <- ppCode (l ^. letExpression)
    return $ kwLet <+> letClauses' <+> kwIn <+> letExpression'

instance PrettyCode FunctionParameter where
  ppCode FunctionParameter {..} = do
    case _paramName of
      Nothing -> ppLeftExpression funFixity _paramType
      Just n -> do
        paramName' <- ppCode n
        paramType' <- ppCode _paramType
        return $ implicitDelim _paramImplicit (paramName' <+> kwColon <+> paramType')

instance PrettyCode NameId where
  ppCode (NameId k) = return (pretty k)

instance PrettyCode Name where
  ppCode n = do
    showNameId <- asks (^. optShowNameIds)
    return (prettyName showNameId n)

instance PrettyCode Function where
  ppCode Function {..} = do
    funParameter' <- ppCode _funParameter
    funReturn' <- ppRightExpression funFixity _funReturn
    return $ funParameter' <+> kwArrow <+> funReturn'

instance PrettyCode FunctionRef where
  ppCode FunctionRef {..} = ppCode _functionRefName

instance PrettyCode ConstructorRef where
  ppCode ConstructorRef {..} = ppCode _constructorRefName

instance PrettyCode InductiveRef where
  ppCode InductiveRef {..} = ppCode _inductiveRefName

instance PrettyCode AxiomRef where
  ppCode AxiomRef {..} = ppCode _axiomRefName
