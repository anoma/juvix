module Juvix.Syntax.Abstract.Pretty.Base
  ( module Juvix.Syntax.Abstract.Pretty.Base,
    module Juvix.Syntax.Abstract.Pretty.Ann,
    module Juvix.Syntax.Abstract.Pretty.Options,
  )
where

import Juvix.Internal.Strings qualified as Str
import Juvix.Prelude
import Juvix.Syntax.Abstract.Language.Extra
import Juvix.Syntax.Abstract.Pretty.Ann
import Juvix.Syntax.Abstract.Pretty.Options
import Juvix.Syntax.Concrete.Scoped.Pretty.Base qualified as S
import Prettyprinter

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

toSOptions :: Options -> S.Options
toSOptions Options {..} =
  S.defaultOptions
    { S._optShowNameIds = _optShowNameIds,
      S._optIndent = _optIndent
    }

class PrettyCode c where
  ppCode :: Members '[Reader Options] r => c -> Sem r (Doc Ann)

ppSCode :: (Members '[Reader Options] r, S.PrettyCode c) => c -> Sem r (Doc Ann)
ppSCode c = do
  opts <- asks toSOptions
  return $ alterAnnotations (maybeToList . fromScopedAnn) (S.runPrettyCode opts c)

ppDefault :: PrettyCode c => c -> Doc Ann
ppDefault = runPrettyCode defaultOptions

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

kwType :: Doc Ann
kwType = keyword Str.type_

kwQuestion :: Doc Ann
kwQuestion = keyword Str.questionMark

kwWaveArrow :: Doc Ann
kwWaveArrow = keyword Str.waveArrow

kwColon :: Doc Ann
kwColon = keyword Str.colon

kwTo :: Doc Ann
kwTo = keyword Str.toUnicode

kwColonZero :: Doc Ann
kwColonZero = keyword Str.colonZero

kwColonOne :: Doc Ann
kwColonOne = keyword Str.colonOne

kwColonOmega :: Doc Ann
kwColonOmega = keyword Str.colonOmegaUnicode

parensCond :: Bool -> Doc Ann -> Doc Ann
parensCond t d = if t then parens d else d

implicitDelim :: IsImplicit -> Doc Ann -> Doc Ann
implicitDelim = \case
  Implicit -> braces
  Explicit -> parens

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
  ppCode (Universe n _) = return $ kwType <+?> (pretty <$> n)

instance PrettyCode Expression where
  ppCode e = case e of
    ExpressionIden i -> ppCode i
    ExpressionUniverse u -> ppCode u
    ExpressionApplication a -> ppCode a
    ExpressionFunction f -> ppCode f
    ExpressionLiteral l -> ppSCode l
    ExpressionHole h -> ppSCode h

instance PrettyCode Usage where
  ppCode u = return $ case u of
    UsageNone -> kwColonZero
    UsageOnce -> kwColonOne
    UsageOmega -> kwColon

instance PrettyCode FunctionParameter where
  ppCode FunctionParameter {..} = do
    case _paramName of
      Nothing -> ppLeftExpression funFixity _paramType
      Just n -> do
        paramName' <- ppCode n
        paramType' <- ppCode _paramType
        paramUsage' <- ppCode _paramUsage
        return $ implicitDelim _paramImplicit (paramName' <+> paramUsage' <+> paramType')

instance PrettyCode NameId where
  ppCode (NameId k) = return (pretty k)

instance PrettyCode Name where
  ppCode n = do
    showNameId <- asks (^. optShowNameIds)
    uid <-
      if
          | showNameId -> Just . ("@" <>) <$> ppCode (n ^. nameId)
          | otherwise -> return Nothing
    return
      $ annotate (AnnKind (n ^. nameKind))
      $ pretty (n ^. nameText)
      <?> uid

instance PrettyCode Function where
  ppCode Function {..} = do
    funParameter' <- ppCode _funParameter
    funReturn' <- ppRightExpression funFixity _funReturn
    return $ funParameter' <+> kwTo <+> funReturn'

instance PrettyCode FunctionRef where
  ppCode FunctionRef {..} = ppCode _functionRefName

instance PrettyCode ConstructorRef where
  ppCode ConstructorRef {..} = ppCode _constructorRefName

instance PrettyCode InductiveRef where
  ppCode InductiveRef {..} = ppCode _inductiveRefName

instance PrettyCode AxiomRef where
  ppCode AxiomRef {..} = ppCode _axiomRefName
