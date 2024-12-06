module Juvix.Compiler.Nockma.Highlight
  ( module Juvix.Compiler.Nockma.Highlight.Input,
    module Juvix.Compiler.Nockma.Highlight.Base,
    module Juvix.Compiler.Nockma.Highlight,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight (goFaceError, goFaceSemanticItem)
import Juvix.Compiler.Nockma.Highlight.Base
import Juvix.Compiler.Nockma.Highlight.Doc
import Juvix.Compiler.Nockma.Highlight.Input
import Juvix.Compiler.Nockma.Language as Nockma
import Juvix.Data.CodeAnn
import Juvix.Emacs.Render
import Juvix.Emacs.SExp
import Juvix.Prelude

highlight :: HighlightInput -> ByteString
highlight = encodeUtf8 . renderSExp . withDocTable . toSExp . buildProperties

buildProperties :: HighlightInput -> LocProperties
buildProperties HighlightInput {..} =
  LocProperties
    { _propertiesFace =
        mapMaybe goFaceSemanticItem _highlightSemanticItems
          <> map goFaceError _highlightErrors,
      _propertiesGoto = [],
      _propertiesTopDef = [],
      _propertiesInfo = map goInfoNockOp _highlightNockOps <> map goInfoPath _highlightPaths
    }

-- | Used in nockma-mode
nockOpKey :: NockOp -> Text
nockOpKey = \case
  OpAddress -> "OpAddress"
  OpQuote -> "OpQuote"
  OpApply -> "OpApply"
  OpIsCell -> "OpIsCell"
  OpInc -> "OpInc"
  OpEq -> "OpEq"
  OpIf -> "OpIf"
  OpSequence -> "OpSequence"
  OpPush -> "OpPush"
  OpCall -> "OpCall"
  OpReplace -> "OpReplace"
  OpHint -> "OpHint"
  OpScry -> "OpScry"

-- | nockma-mode depends on this
docTableVarName :: Text
docTableVarName = "nockma-doc-table"

-- | NockOp ↦ (txt, init)
withDocTable :: SExp -> SExp
withDocTable body =
  progn
    [ mkHashTable
        docTableVarName
        [ (Quote (Symbol (nockOpKey op)), Quote (Pair (String docTxt) initExpr)) | op <- allElements, let (docTxt, initExpr) = renderEmacs (nockOpDoc op)
        ],
      body
    ]

goInfoPath :: WithLoc Nockma.Path -> WithLoc PropertyInfo
goInfoPath = fmap toProperty
  where
    toProperty :: Nockma.Path -> PropertyInfo
    toProperty p =
      PropertyInfo
        { _infoInfo = String txt,
          _infoInit = format
        }
      where
        (txt, format) = renderEmacs msg

        msg :: Doc CodeAnn
        msg =
          ppCodeAnn p
            <+> kwEquals
            <+> pretty (encodePath p ^. encodedPath)

goInfoNockOp :: WithLoc NockOp -> WithLoc PropertyInfo
goInfoNockOp = fmap toProperty
  where
    toProperty :: NockOp -> PropertyInfo
    toProperty o =
      PropertyInfo
        { _infoInfo = Symbol (nockOpKey o),
          _infoInit = nil
        }
