module Juvix.Compiler.Concrete.Data.Highlight.Properties where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH
import Juvix.Data.Emacs
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Lens.Micro.Platform qualified as Lens

data GenericProperty = GenericProperty
  { _gpropProperty :: Text,
    _gpropValue :: SExp
  }

makeLenses ''GenericProperty

class IsProperty a where
  toProperty :: a -> GenericProperty

instance IsProperty GenericProperty where
  toProperty = id

data Face
  = FaceConstructor
  | FaceInductive
  | FaceFunction
  | FaceModule
  | FaceAxiom
  | FaceKeyword
  | FaceString
  | FaceNumber
  | FaceComment
  | FaceError

faceSymbolStr :: Face -> Text
faceSymbolStr = \case
  FaceAxiom -> Str.axiom
  FaceInductive -> Str.inductive
  FaceConstructor -> Str.constructor
  FaceModule -> Str.module_
  FaceKeyword -> Str.keyword
  FaceFunction -> Str.function
  FaceNumber -> Str.number
  FaceComment -> Str.comment
  FaceString -> Str.string
  FaceError -> Str.error

instance ToSExp Face where
  toSExp = faceSymbol . faceSymbolStr
    where
      faceSymbol :: Text -> SExp
      faceSymbol faceSymbolTxt = Symbol ("juvix-highlight-" <> faceSymbolTxt <> "-face")

instance ToJSON Face where
  toJSON = Aeson.String . faceSymbolStr

data EmacsProperty
  = EPropertyGoto PropertyGoto
  | EPropertyFace PropertyFace
  | EPropertyType PropertyType

type LocEmacsProperty = WithLoc EmacsProperty

data PropertyGoto = PropertyGoto
  { _gotoFile :: Path Abs File,
    _gotoPos :: FileLoc
  }

newtype PropertyFace = PropertyFace
  { _faceFace :: Face
  }

newtype PropertyType = PropertyType
  { _typeType :: Text
  }

data LocProperties = LocProperties
  { _propertiesGoto :: [WithLoc PropertyGoto],
    _propertiesFace :: [WithLoc PropertyFace],
    _propertiesType :: [WithLoc PropertyType]
  }

data RawProperties = RawProperties
  { _rawPropertiesFace :: [RawWithLoc RawFace],
    _rawPropertiesGoto :: [RawWithLoc RawGoto],
    _rawPropertiesType :: [RawWithLoc RawType]
  }

-- | (File, Row, Col, Length)
type RawInterval = (Path Abs File, Int, Int, Int)

type RawWithLoc a = (RawInterval, a)

type RawFace = Face

-- | (TargetFile, TargetLine, TargetColumn)
type RawGoto = (Path Abs File, Int, Int)

-- | (Type)
type RawType = Text

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier = over Lens._head toLower . dropPrefix "_rawProperties",
         constructorTagModifier = map toLower
       }
     ''RawProperties
 )

rawProperties :: LocProperties -> RawProperties
rawProperties LocProperties {..} =
  RawProperties
    { _rawPropertiesGoto = map (rawWithLoc rawGoto) _propertiesGoto,
      _rawPropertiesFace = map (rawWithLoc rawFace) _propertiesFace,
      _rawPropertiesType = map (rawWithLoc rawType) _propertiesType
    }
  where
    rawInterval :: Interval -> RawInterval
    rawInterval i =
      ( i ^. intervalFile,
        fromIntegral (i ^. intervalStart . locLine),
        fromIntegral (i ^. intervalStart . locCol),
        intervalLength i
      )

    rawWithLoc :: (a -> b) -> WithLoc a -> RawWithLoc b
    rawWithLoc f x = (rawInterval (getLoc x), f (x ^. withLocParam))

    rawType :: PropertyType -> RawType
    rawType PropertyType {..} = _typeType

    rawFace :: PropertyFace -> RawFace
    rawFace PropertyFace {..} = _faceFace

    rawGoto :: PropertyGoto -> RawGoto
    rawGoto PropertyGoto {..} =
      ( _gotoFile,
        fromIntegral (_gotoPos ^. locLine),
        fromIntegral (_gotoPos ^. locCol)
      )

instance IsProperty EmacsProperty where
  toProperty = \case
    EPropertyFace p -> toProperty p
    EPropertyType p -> toProperty p
    EPropertyGoto p -> toProperty p

putProperty :: IsProperty a => WithLoc a -> SExp
putProperty (WithLoc i a) =
  App [Symbol "put-text-property", start, end, Quote (Symbol _gpropProperty), Quote _gpropValue]
  where
    GenericProperty {..} = toProperty a
    start = Int (fileLocToPoint (i ^. intervalStart))
    end = Int (fileLocToPoint (i ^. intervalEnd))

instance IsProperty PropertyFace where
  toProperty PropertyFace {..} =
    GenericProperty
      { _gpropProperty = "face",
        _gpropValue = toSExp _faceFace
      }

instance IsProperty PropertyGoto where
  toProperty PropertyGoto {..} =
    GenericProperty
      { _gpropProperty = "juvix-goto",
        _gpropValue = gotoPair
      }
    where
      gotoPair = Pair (String (pack (toFilePath _gotoFile))) (Int (_gotoPos ^. locOffset . to (succ . fromIntegral)))

instance IsProperty PropertyType where
  toProperty PropertyType {..} =
    GenericProperty
      { _gpropProperty = "help-echo",
        _gpropValue = String _typeType
      }

instance ToSExp LocProperties where
  toSExp LocProperties {..} =
    progn
      ( map putProperty _propertiesFace
          <> map putProperty _propertiesGoto
          <> map putProperty _propertiesType
      )
