module Juvix.Compiler.Nockma.Highlight.Input where

import Juvix.Data.CodeAnn
import Juvix.Prelude

newtype HighlightInput = HighlightInput
  { _highlightSemanticItems :: [SemanticItem]
  }

makeLenses ''HighlightInput

emptyHighlightInput :: HighlightInput
emptyHighlightInput =
  HighlightInput
    { _highlightSemanticItems = []
    }

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightSemanticItems = filterByLoc absPth _highlightSemanticItems
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
