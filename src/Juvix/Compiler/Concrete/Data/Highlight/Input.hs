module Juvix.Compiler.Concrete.Data.Highlight.Input
  ( module Juvix.Compiler.Concrete.Data.Highlight.Input,
    module Juvix.Compiler.Concrete.Data.ParsedItem,
  )
where

import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Prelude

data HighlightInput = HighlightInput
  { _highlightParsed :: [ParsedItem],
    _highlightNames :: [AName]
  }

makeLenses ''HighlightInput

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightNames = filterByLoc absPth _highlightNames,
      _highlightParsed = filterByLoc absPth _highlightParsed
    }

filterByLoc :: (HasLoc p) => Path Abs File -> [p] -> [p]
filterByLoc p = filter ((== toFilePath p) . (^. intervalFile) . getLoc)
