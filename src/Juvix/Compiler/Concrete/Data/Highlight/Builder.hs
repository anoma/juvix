module Juvix.Compiler.Concrete.Data.Highlight.Builder
  ( module Juvix.Compiler.Concrete.Data.Highlight.Input,
    module Juvix.Compiler.Concrete.Data.ParsedItem,
    module Juvix.Compiler.Concrete.Data.Highlight.Builder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Compiler.Internal.Language qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context
import Juvix.Compiler.Store.Scoped.Data.InfoTable
import Juvix.Prelude

data HighlightBuilder :: Effect where
  HighlightError :: Interval -> HighlightBuilder m ()
  HighlightDoc :: NameId -> Maybe (Judoc 'Scoped) -> HighlightBuilder m ()
  HighlightName :: AName -> HighlightBuilder m ()
  HighlightParsedItem :: ParsedItem -> HighlightBuilder m ()
  HighlightType :: NameId -> Internal.Expression -> HighlightBuilder m ()
  HighlightMergeDocTable :: DocTable -> HighlightBuilder m ()
  GetDocTable :: ModuleId -> HighlightBuilder m DocTable

makeSem ''HighlightBuilder

runHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput, a)
runHighlightBuilder = reinterpret (runStateShared emptyHighlightInput) $ \case
  HighlightError e -> modifyShared (over highlightErrors (e :))
  HighlightName a -> modifyShared (over (highlightNames) (a :))
  HighlightParsedItem p -> modifyShared (over (highlightParsedItems) (p :))
  HighlightDoc k md -> modifyShared (set (highlightDocTable . at k) md)
  HighlightType uid ty -> modifyShared (set (highlightTypes . typesTable . at uid) (Just ty))
  HighlightMergeDocTable tbl -> modifyShared (over highlightDocTable (HashMap.union tbl))
  GetDocTable uid -> filterByTopModule uid <$> getsShared (^. highlightDocTable)

ignoreHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r a
ignoreHighlightBuilder = fmap snd . runHighlightBuilder

runJuvixError :: (Members '[HighlightBuilder] r) => Sem (Error JuvixError ': r) a -> Sem r (Either JuvixError a)
runJuvixError m = do
  x <- runError m
  case x of
    r@Right {} -> return r
    l@(Left err) -> do
      let errs =
            (^. genericErrorIntervals)
              . run
              . runReader defaultGenericOptions
              $ genericError err
      mapM_ highlightError errs
      return l
