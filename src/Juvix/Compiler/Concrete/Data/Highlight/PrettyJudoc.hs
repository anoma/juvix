module Juvix.Compiler.Concrete.Data.Highlight.PrettyJudoc where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as Scoped
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty qualified as Scoped
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Data.CodeAnn
import Juvix.Prelude

-- | placeholder
data Options = Options

defaultOptions :: Options
defaultOptions = Options

ppDocDefault :: Scoped.AName -> Internal.Expression -> Maybe (Judoc 'Scoped) -> Doc CodeAnn
ppDocDefault n ty = run . runReader defaultOptions . ppDoc n ty

ppInternal :: Members '[Reader Options] r => Internal.PrettyCode c => c -> Sem r (Doc CodeAnn)
ppInternal c = do
  iopts <- mkOpts <$> ask
  return (Internal.runPrettyCode iopts c)
  where
    mkOpts :: Options -> Internal.Options
    mkOpts = const (Internal.defaultOptions)

ppScoped :: Members '[Reader Options] r => Scoped.PrettyCode c => c -> Sem r (Doc CodeAnn)
ppScoped c = do
  iopts <- mkOpts <$> ask
  return (Scoped.runPrettyCode iopts c)
  where
    mkOpts :: Options -> Scoped.Options
    mkOpts = const (Scoped.defaultOptions)

ppDoc :: Members '[Reader Options] r => Scoped.AName -> Internal.Expression -> Maybe (Judoc 'Scoped) -> Sem r (Doc CodeAnn)
ppDoc n ty j = do
  ty' <- ppInternal ty
  n' <- ppScoped n
  j' <- join <$> mapM ppJudoc j
  return $
    n' <+> kwColon <+> ty' <+?> fmap ((hardline <> hardline) <>) j'

ppJudoc :: forall r. Members '[Reader Options] r => Judoc 'Scoped -> Sem r (Maybe (Doc CodeAnn))
ppJudoc (Judoc bs) = do
  void (ask @Options) -- to suppress redundant constraint warning
  mapM (mconcatMap ppBlock) (nonEmpty bs)
  where
    ppBlock :: JudocBlock 'Scoped -> Sem r (Doc CodeAnn)
    ppBlock = \case
      JudocParagraph ls -> mconcatMapM ppLine (toList ls)
      JudocExample {} -> return mempty

    ppLine :: JudocParagraphLine 'Scoped -> Sem r (Doc CodeAnn)
    ppLine (JudocParagraphLine as) = mconcatMapM (ppAtom . (^. withLocParam)) (toList as)

    ppAtom :: JudocAtom 'Scoped -> Sem r (Doc CodeAnn)
    ppAtom = \case
      JudocText t -> return (pretty t)
      JudocExpression e -> ppScoped e
