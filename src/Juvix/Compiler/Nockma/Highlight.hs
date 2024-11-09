module Juvix.Compiler.Nockma.Highlight
  ( module Juvix.Compiler.Nockma.Highlight.Input,
    module Juvix.Compiler.Nockma.Highlight.Base,
    module Juvix.Compiler.Nockma.Highlight,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight (goFaceParsedItem)
import Juvix.Compiler.Nockma.Highlight.Base
import Juvix.Compiler.Nockma.Highlight.Input
import Juvix.Data.Emacs.SExp
import Juvix.Prelude

highlight :: HighlightInput -> ByteString
highlight = encodeUtf8 . renderSExp . toSExp . buildProperties

buildProperties :: HighlightInput -> LocProperties
buildProperties HighlightInput {..} =
  LocProperties
    { _propertiesFace = map goFaceParsedItem _highlightParsedItems,
      _propertiesGoto = [],
      _propertiesDoc = []
    }
