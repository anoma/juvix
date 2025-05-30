module Juvix.Compiler.Concrete.Data.Highlight.Input where

import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Store.Scoped.Data.InfoTable qualified as Scoped
import Juvix.Prelude

data HighlightInput = HighlightInput
  { _highlightParsedItems :: [ParsedItem],
    _highlightDocTable :: Scoped.DocTable,
    _highlightNames :: [AName],
    _highlightPrecItems :: [WithLoc Int],
    _highlightTypes :: Internal.TypesTable,
    _highlightErrors :: [Interval]
  }

makeLenses ''HighlightInput

emptyHighlightInput :: HighlightInput
emptyHighlightInput =
  HighlightInput
    { _highlightParsedItems = [],
      _highlightDocTable = mempty,
      _highlightNames = [],
      _highlightPrecItems = [],
      _highlightTypes = mempty,
      _highlightErrors = []
    }

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightNames = filterByLoc absPth _highlightNames,
      _highlightParsedItems = filterByLoc absPth _highlightParsedItems,
      _highlightPrecItems = filterByLoc absPth _highlightPrecItems,
      _highlightErrors = filterByLoc absPth _highlightErrors,
      _highlightTypes,
      _highlightDocTable
    }
