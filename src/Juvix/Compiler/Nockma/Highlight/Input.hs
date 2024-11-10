module Juvix.Compiler.Nockma.Highlight.Input where

import Juvix.Compiler.Nockma.Language hiding (Path)
import Juvix.Data.CodeAnn
import Juvix.Prelude

data HighlightInput = HighlightInput
  { _highlightSemanticItems :: [SemanticItem],
    _highlightNockOps :: [WithLoc NockOp],
    _highlightErrors :: [Interval]
  }

makeLenses ''HighlightInput

emptyHighlightInput :: HighlightInput
emptyHighlightInput =
  HighlightInput
    { _highlightSemanticItems = [],
      _highlightNockOps = [],
      _highlightErrors = []
    }

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightSemanticItems = filterByLoc absPth _highlightSemanticItems,
      _highlightNockOps = filterByLoc absPth _highlightNockOps,
      _highlightErrors = _highlightErrors
    }

data HighlightBuilder :: Effect where
  HighlightItem :: SemanticItem -> HighlightBuilder m ()

makeSem ''HighlightBuilder

ignoreHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r a
ignoreHighlightBuilder = interpret $ \case
  HighlightItem {} -> return ()

execHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput)
execHighlightBuilder = fmap fst . runHighlightBuilder

runHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput, a)
runHighlightBuilder = reinterpret (runState emptyHighlightInput) $ \case
  HighlightItem i -> modify (over highlightSemanticItems (i :))
