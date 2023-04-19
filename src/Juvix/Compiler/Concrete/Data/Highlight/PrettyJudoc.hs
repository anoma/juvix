module Juvix.Compiler.Concrete.Data.Highlight.PrettyJudoc where

import Data.Text qualified as Text
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
    n' <+> kwColon <+> ty' <+?> fmap ((line <> line) <>) j'

ppJudoc :: forall r. Members '[Reader Options] r => Judoc 'Scoped -> Sem r (Maybe (Doc CodeAnn))
ppJudoc (Judoc bs) = do
  void (ask @Options) -- to suppress redundant constraint warning
  mapM ppBlocks (nonEmpty bs)
  where
    ppBlocks :: NonEmpty (JudocBlock 'Scoped) -> Sem r (Doc CodeAnn)
    ppBlocks = fmap vsep2 . mapM ppBlock

    ppBlock :: JudocBlock 'Scoped -> Sem r (Doc CodeAnn)
    ppBlock = \case
      JudocParagraph ls -> hsep <$> mapM ppLine (toList ls)
      JudocExample {} -> return mempty

    ppLine :: JudocParagraphLine 'Scoped -> Sem r (Doc CodeAnn)
    ppLine (JudocParagraphLine as) = mconcatMapM (ppAtom . (^. withLocParam)) (toList as)

    ppAtom :: JudocAtom 'Scoped -> Sem r (Doc CodeAnn)
    ppAtom = \case
      -- We reflow the text so that newlines may be inserted if the contents do not fit a line.
      -- We must add space at both ends if the original text had space there
      JudocText t
        | Text.null t -> return mempty
        | otherwise -> return (mkSpace (Text.head t) <> reflow t <> mkSpace (Text.last t))
        where
          mkSpace :: Char -> Doc CodeAnn
          mkSpace = \case
            ' ' -> pretty ' '
            _ -> mempty
      JudocExpression e -> ppScoped e
