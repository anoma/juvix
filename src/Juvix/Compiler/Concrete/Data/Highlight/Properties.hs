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
  toProperties :: a -> NonEmpty GenericProperty

instance IsProperty GenericProperty where
  toProperties = pure

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
  | FaceJudoc
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
  FaceJudoc -> Str.judoc
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
  | EPropertyDoc PropertyDoc

type LocEmacsProperty = WithLoc EmacsProperty

data PropertyGoto = PropertyGoto
  { _gotoFile :: Path Abs File,
    _gotoPos :: FileLoc
  }

newtype PropertyFace = PropertyFace
  { _faceFace :: Face
  }

data PropertyDoc = PropertyDoc
  { _docText :: Text,
    _docSExp :: SExp
  }

data LocProperties = LocProperties
  { _propertiesGoto :: [WithLoc PropertyGoto],
    _propertiesFace :: [WithLoc PropertyFace],
    _propertiesDoc :: [WithLoc PropertyDoc]
  }

data RawProperties = RawProperties
  { _rawPropertiesFace :: [RawWithLoc RawFace],
    _rawPropertiesGoto :: [RawWithLoc RawGoto],
    _rawPropertiesDoc :: [RawWithLoc RawType]
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
      _rawPropertiesDoc = map (rawWithLoc rawType) _propertiesDoc
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

    rawType :: PropertyDoc -> RawType
    rawType PropertyDoc {..} = _docText

    rawFace :: PropertyFace -> RawFace
    rawFace PropertyFace {..} = _faceFace

    rawGoto :: PropertyGoto -> RawGoto
    rawGoto PropertyGoto {..} =
      ( _gotoFile,
        fromIntegral (_gotoPos ^. locLine),
        fromIntegral (_gotoPos ^. locCol)
      )

instance IsProperty EmacsProperty where
  toProperties = \case
    EPropertyFace p -> toProperties p
    EPropertyDoc p -> toProperties p
    EPropertyGoto p -> toProperties p

addGenericProperties :: WithRange (NonEmpty GenericProperty) -> SExp
addGenericProperties (WithRange i props) =
  App [Symbol "add-text-properties", start, end, propertyList]
  where
    start = Int (unPoint (i ^. pintervalStart))
    end = Int (unPoint (i ^. pintervalEnd))
    propertyList :: SExp
    propertyList = mkList (concat [[k, v] | (k, v) <- map mkItem (toList props)])
      where
        mkItem :: GenericProperty -> (SExp, SExp)
        mkItem GenericProperty {..} = (Symbol _gpropProperty, _gpropValue)

putProperty :: IsProperty a => WithRange a -> SExp
putProperty = addGenericProperties . fmap toProperties

putPropertyLoc :: IsProperty a => WithLoc a -> SExp
putPropertyLoc (WithLoc i a) = putProperty (WithRange i' a)
  where
    i' :: PointInterval
    i' =
      PointInterval
        { _pintervalStart = fileLocToPoint (i ^. intervalStart),
          _pintervalEnd = fileLocToPoint (i ^. intervalEnd)
        }

instance IsProperty PropertyFace where
  toProperties PropertyFace {..} =
    pure
      GenericProperty
        { _gpropProperty = "face",
          _gpropValue = toSExp _faceFace
        }

instance IsProperty PropertyGoto where
  toProperties PropertyGoto {..} =
    pure
      GenericProperty
        { _gpropProperty = "juvix-goto",
          _gpropValue = gotoPair
        }
    where
      gotoPair = Pair (String (pack (toFilePath _gotoFile))) (Int (_gotoPos ^. locOffset . to (succ . fromIntegral)))

instance IsProperty PropertyDoc where
  toProperties PropertyDoc {..} =
    GenericProperty
      { _gpropProperty = "help-echo",
        _gpropValue = String _docText
      }
      :| [ GenericProperty
             { _gpropProperty = "juvix-format",
               _gpropValue = _docSExp
             }
         ]

instance ToSExp LocProperties where
  toSExp LocProperties {..} =
    progn
      ( map putPropertyLoc _propertiesFace
          <> map putPropertyLoc _propertiesGoto
          <> map putPropertyLoc _propertiesDoc
      )
