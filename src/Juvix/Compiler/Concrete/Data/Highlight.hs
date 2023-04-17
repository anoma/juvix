module Juvix.Compiler.Concrete.Data.Highlight
  ( module Juvix.Compiler.Concrete.Data.Highlight,
    module Juvix.Compiler.Concrete.Data.Highlight.Input,
    module Juvix.Compiler.Concrete.Data.Highlight.Properties,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString
import Data.Text.Encoding qualified as Text
import Juvix.Compiler.Concrete.Data.Highlight.Input
import Juvix.Compiler.Concrete.Data.Highlight.Properties
import Juvix.Compiler.Concrete.Data.Highlight.RenderEmacs
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Data.CodeAnn
import Juvix.Data.Emacs
import Juvix.Prelude as Prelude hiding (show)
import Prelude qualified

data HighlightBackend
  = Emacs
  | Json
  deriving stock (Bounded, Enum, Data)

instance Show HighlightBackend where
  show = \case
    Emacs -> "emacs"
    Json -> "json"

highlight :: HighlightBackend -> HighlightInput -> ByteString
highlight = \case
  Emacs -> Text.encodeUtf8 . renderSExp . toSExp . buildProperties
  Json -> ByteString.toStrict . Aeson.encode . rawProperties . buildProperties

goErrors :: HighlightBackend -> [Interval] -> ByteString
goErrors = \case
  Emacs -> Text.encodeUtf8 . renderSExp . toSExp . errorProperties
  Json -> ByteString.toStrict . Aeson.encode . rawProperties . errorProperties

errorProperties :: [Interval] -> LocProperties
errorProperties l =
  LocProperties
    { _propertiesFace = map goFaceError l,
      _propertiesGoto = [],
      _propertiesDoc = []
    }

buildProperties :: HighlightInput -> LocProperties
buildProperties HighlightInput {..} =
  LocProperties
    { _propertiesFace =
        map goFaceParsedItem _highlightParsed
          <> mapMaybe goFaceName _highlightNames,
      _propertiesGoto = map goGotoProperty _highlightNames,
      _propertiesDoc = mapMaybe (goDocProperty _highlightTypes) _highlightNames
    }

goFaceError :: Interval -> WithLoc PropertyFace
goFaceError i = WithLoc i (PropertyFace FaceError)

goFaceParsedItem :: ParsedItem -> WithLoc PropertyFace
goFaceParsedItem i = WithLoc (i ^. parsedLoc) (PropertyFace f)
  where
    f = case i ^. parsedTag of
      ParsedTagKeyword -> FaceKeyword
      ParsedTagLiteralInt -> FaceNumber
      ParsedTagLiteralString -> FaceString
      ParsedTagComment -> FaceComment

goFaceName :: AName -> Maybe (WithLoc PropertyFace)
goFaceName n = do
  f <- nameKindFace (getNameKind n)
  return (WithLoc (getLoc n) (PropertyFace f))

goGotoProperty :: AName -> WithLoc PropertyGoto
goGotoProperty (AName n) = WithLoc (getLoc n) PropertyGoto {..}
  where
    _gotoPos = n ^. nameDefined . intervalStart
    _gotoFile = n ^. nameDefined . intervalFile

goDocProperty :: Internal.TypesTable -> AName -> Maybe (WithLoc PropertyDoc)
goDocProperty tbl (AName n) = do
  ty <- tbl ^. at (n ^. nameId)
  let (_docText, _docSExp) = renderEmacs (layoutPretty defaultLayoutOptions (Internal.doc Internal.defaultOptions ty))
  return (WithLoc (getLoc n) PropertyDoc {..})
