module Juvix.Compiler.Nockma.Highlight.Input where

import Juvix.Prelude

newtype HighlightInput = HighlightInput
  { _highlightParsedItems :: [ParsedItem]
  }

makeLenses ''HighlightInput

emptyHighlightInput :: HighlightInput
emptyHighlightInput =
  HighlightInput
    { _highlightParsedItems = []
    }

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightParsedItems = filterByLoc absPth _highlightParsedItems
    }

data HighlightBuilder :: Effect where
  HighlightItem :: ParsedItem -> HighlightBuilder m ()

makeSem ''HighlightBuilder

ignoreHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r a
ignoreHighlightBuilder = interpret $ \case
  HighlightItem {} -> return ()

execHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput)
execHighlightBuilder = fmap fst . runHighlightBuilder

runHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput, a)
runHighlightBuilder = reinterpret (runState emptyHighlightInput) $ \case
  HighlightItem i -> modify (over highlightParsedItems (i :))
