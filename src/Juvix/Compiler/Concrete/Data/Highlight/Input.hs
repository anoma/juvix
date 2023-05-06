module Juvix.Compiler.Concrete.Data.Highlight.Input
  ( module Juvix.Compiler.Concrete.Data.Highlight.Input,
    module Juvix.Compiler.Concrete.Data.ParsedItem,
  )
where

import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoped
import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Prelude

data HighlightInput = HighlightInput
  { _highlightParsed :: [ParsedItem],
    _highlightDoc :: Scoped.DocTable,
    _highlightNames :: [AName],
    _highlightTypes :: Internal.TypesTable,
    _highlightErrors :: [Interval]
  }

makeLenses ''HighlightInput

emptyHighlightInput :: HighlightInput
emptyHighlightInput =
  HighlightInput
    { _highlightParsed = [],
      _highlightDoc = mempty,
      _highlightNames = [],
      _highlightTypes = mempty,
      _highlightErrors = []
    }

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightNames = filterByLoc absPth _highlightNames,
      _highlightParsed = filterByLoc absPth _highlightParsed,
      _highlightErrors = filterByLoc absPth _highlightErrors,
      _highlightTypes,
      _highlightDoc
    }

type HighlightBuilder = State HighlightInput

runHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput, a)
runHighlightBuilder = runState emptyHighlightInput

ignoreHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r a
ignoreHighlightBuilder = evalState emptyHighlightInput

runJuvixError :: Members '[HighlightBuilder] r => Sem (Error JuvixError ': r) a -> Sem r (Either JuvixError a)
runJuvixError m = do
  x <- runError m
  case x of
    r@Right {} -> return r
    l@(Left err) -> do
      let errs = run (runReader defaultGenericOptions (genericError err)) ^. genericErrorIntervals
      modify (set highlightErrors errs)
      return l
