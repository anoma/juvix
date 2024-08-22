module Juvix.Compiler.Concrete.Data.Highlight.Input
  ( module Juvix.Compiler.Concrete.Data.Highlight.Input,
    module Juvix.Compiler.Concrete.Data.ParsedItem,
  )
where

import Juvix.Compiler.Concrete.Data.ParsedItem
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Store.Scoped.Data.InfoTable qualified as Scoped
import Juvix.Prelude

data HighlightInput = HighlightInput
  { _highlightParsedItems :: [ParsedItem],
    _highlightDocTable :: Scoped.DocTable,
    _highlightNames :: [AName],
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
      _highlightTypes = mempty,
      _highlightErrors = []
    }

filterInput :: Path Abs File -> HighlightInput -> HighlightInput
filterInput absPth HighlightInput {..} =
  HighlightInput
    { _highlightNames = filterByLoc absPth _highlightNames,
      _highlightParsedItems = filterByLoc absPth _highlightParsedItems,
      _highlightErrors = filterByLoc absPth _highlightErrors,
      _highlightTypes,
      _highlightDocTable
    }
