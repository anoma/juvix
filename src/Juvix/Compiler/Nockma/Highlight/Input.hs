module Juvix.Compiler.Nockma.Highlight.Input where

import Juvix.Compiler.Nockma.Language hiding (Path)
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Data.CodeAnn
import Juvix.Prelude

data HighlightInput = HighlightInput
  { _highlightSemanticItems :: [SemanticItem],
    _highlightNockOps :: [WithLoc NockOp],
    _highlightPaths :: [WithLoc Nockma.Path],
    _highlightErrors :: [Interval]
  }

makeLenses ''HighlightInput

emptyHighlightInput :: HighlightInput
emptyHighlightInput =
  HighlightInput
    { _highlightSemanticItems = [],
      _highlightNockOps = [],
      _highlightPaths = [],
      _highlightErrors = []
    }

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightSemanticItems = filterByLoc absPth _highlightSemanticItems,
      _highlightNockOps = filterByLoc absPth _highlightNockOps,
      _highlightPaths = filterByLoc absPth _highlightPaths,
      _highlightErrors = _highlightErrors
    }

data HighlightBuilder :: Effect where
  HighlightItem :: SemanticItem -> HighlightBuilder m ()
  HighlightNockOp :: WithLoc NockOp -> HighlightBuilder m ()
  HighlightPath :: WithLoc Nockma.Path -> HighlightBuilder m ()

makeSem ''HighlightBuilder

evalHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r a
evalHighlightBuilder = interpret $ \case
  HighlightItem {} -> return ()
  HighlightNockOp {} -> return ()
  HighlightPath {} -> return ()

execHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput)
execHighlightBuilder = fmap fst . runHighlightBuilder

runHighlightBuilder :: Sem (HighlightBuilder ': r) a -> Sem r (HighlightInput, a)
runHighlightBuilder = reinterpret (runState emptyHighlightInput) $ \case
  HighlightItem i -> modify (over highlightSemanticItems (i :))
  HighlightNockOp i -> modify (over highlightNockOps (i :))
  HighlightPath i -> modify (over highlightPaths (i :))
