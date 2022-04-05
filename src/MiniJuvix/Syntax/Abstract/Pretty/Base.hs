module MiniJuvix.Syntax.Abstract.Pretty.Base
  ( module MiniJuvix.Syntax.Abstract.Pretty.Base,
    module MiniJuvix.Syntax.Abstract.Pretty.Ann,
  )
where

import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Syntax.Abstract.Pretty.Ann
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base qualified as S
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.Universe
import MiniJuvix.Syntax.Usage
import Prettyprinter

data Options = Options
  { _optShowNameId :: Bool,
    _optIndent :: Int,
    _optShowDecreasingArgs :: ShowDecrArgs
  }

data ShowDecrArgs = OnlyArg | OnlyRel | ArgRel

toSOptions :: Options -> S.Options
toSOptions Options {..} =
  S.defaultOptions
    { S._optShowNameId = _optShowNameId,
      S._optIndent = _optIndent
    }

class PrettyCode c where
  ppCode :: Members '[Reader Options] r => c -> Sem r (Doc Ann)

ppSCode :: (Members '[Reader Options] r, S.PrettyCode c) => c -> Sem r (Doc Ann)
ppSCode c = do
  opts <- asks toSOptions
  return $ alterAnnotations (maybeToList . fromScopedAnn) (S.runPrettyCode opts c)

defaultOptions :: Options
defaultOptions =
  Options
    { _optShowNameId = False,
      _optIndent = 2,
      _optShowDecreasingArgs = OnlyRel
    }

ppDefault :: PrettyCode c => c -> Doc Ann
ppDefault = runPrettyCode defaultOptions

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

instance PrettyCode Iden where
  ppCode = ppSCode . idenName

instance PrettyCode Application where
  ppCode (Application l r) = do
    l' <- ppLeftExpression appFixity l
    r' <- ppRightExpression appFixity r
    return $ l' <+> r'

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

kwType :: Doc Ann
kwType = keyword Str.type_

instance PrettyCode Universe where
  ppCode (Universe n) = return $ kwType <+?> (pretty <$> n)

instance PrettyCode Expression where
  ppCode e = case e of
    ExpressionIden i -> ppCode i
    ExpressionUniverse u -> ppCode u
    ExpressionApplication a -> ppCode a
    ExpressionFunction f -> ppCode f
    ExpressionLiteral l -> ppSCode l

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
        paramName' <- ppSCode n
        paramType' <- ppCode _paramType
        paramUsage' <- ppCode _paramUsage
        return $ parens (paramName' <+> paramUsage' <+> paramType')

instance PrettyCode Function where
  ppCode Function {..} = do
    funParameter' <- ppCode _funParameter
    funReturn' <- ppRightExpression funFixity _funReturn
    return $ funParameter' <+> kwTo <+> funReturn'

instance PrettyCode FunctionRef where
  ppCode FunctionRef {..} = ppSCode _functionRefName

instance PrettyCode ConstructorRef where
  ppCode ConstructorRef {..} = ppSCode _constructorRefName

instance PrettyCode InductiveRef where
  ppCode InductiveRef {..} = ppSCode _inductiveRefName

instance PrettyCode AxiomRef where
  ppCode AxiomRef {..} = ppSCode _axiomRefName

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
