module Juvix.Emacs.Properties where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH
import Juvix.Emacs.Point
import Juvix.Emacs.SExp
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

data PropertyId
  = PropertyIdFace
  | PropertyIdGoto
  | PropertyIdInfo
  | PropertyIdFormat

propertyIdText :: PropertyId -> Text
propertyIdText = \case
  PropertyIdFace -> "face"
  PropertyIdInfo -> "juvix-info"
  PropertyIdGoto -> "juvix-goto"
  PropertyIdFormat -> "juvix-format"

data GenericProperty = GenericProperty
  { _gpropProperty :: PropertyId,
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
  | FaceFixity
  | FaceAxiom
  | FaceDelimiter
  | FaceKeyword
  | FaceString
  | FaceNumber
  | FaceComment
  | FacePragma
  | FaceJudoc
  | FaceError

faceSymbolStr :: Face -> Text
faceSymbolStr = \case
  FaceAxiom -> Str.axiom
  FaceInductive -> Str.inductive
  FaceConstructor -> Str.constructor
  FacePragma -> Str.pragma
  FaceModule -> Str.module_
  FaceKeyword -> Str.keyword
  FaceDelimiter -> Str.delimiter
  FaceFunction -> Str.function
  FaceNumber -> Str.number
  FaceComment -> Str.comment
  FaceJudoc -> Str.judoc
  FaceString -> Str.string
  FaceError -> Str.error
  FaceFixity -> Str.fixity

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
  | EPropertyInfo PropertyInfo

type LocEmacsProperty = WithLoc EmacsProperty

data PropertyGoto = PropertyGoto
  { _gotoFile :: Path Abs File,
    _gotoPos :: FileLoc
  }

-- | Location where a top symbol is defined
newtype PropertyTopDef = PropertyTopDef
  { _topDef :: Text
  }
  deriving stock (Eq, Generic)

instance Hashable PropertyTopDef

newtype PropertyFace = PropertyFace
  { _faceFace :: Face
  }

data PropertyInfo = PropertyInfo
  { _infoInfo :: SExp,
    _infoInit :: SExp
  }

data LocProperties = LocProperties
  { _propertiesGoto :: [WithLoc PropertyGoto],
    _propertiesFace :: [WithLoc PropertyFace],
    _propertiesTopDef :: [WithLoc PropertyTopDef],
    _propertiesInfo :: [WithLoc PropertyInfo]
  }

data RawProperties = RawProperties
  { _rawPropertiesFace :: [RawWithLoc RawFace],
    _rawPropertiesGoto :: [RawWithLoc RawGoto],
    _rawPropertiesDoc :: [RawWithLoc RawType],
    _rawPropertiesTopDef :: [RawWithLoc RawTopDef]
  }

-- | (File, Start Row, Start Col, Length, End Row, End Col)
type RawInterval = (Path Abs File, Int, Int, Int, Int, Int)

type RawWithLoc a = (RawInterval, a)

type RawFace = Face

-- | (TargetFile, TargetLine, TargetColumn)
type RawGoto = (Path Abs File, Int, Int)

type RawTopDef = Text

-- | (Type)
type RawType = Text

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier = over _head toLower . dropPrefix "_rawProperties",
         constructorTagModifier = map toLower
       }
     ''RawProperties
 )

rawProperties :: LocProperties -> RawProperties
rawProperties LocProperties {..} =
  RawProperties
    { _rawPropertiesGoto = map (rawWithLoc rawGoto) _propertiesGoto,
      _rawPropertiesFace = map (rawWithLoc rawFace) _propertiesFace,
      _rawPropertiesTopDef = map (rawWithLoc rawTopDef) _propertiesTopDef,
      _rawPropertiesDoc = map (rawWithLoc rawType) _propertiesInfo
    }
  where
    rawInterval :: Interval -> RawInterval
    rawInterval i =
      ( i ^. intervalFile,
        fromIntegral (i ^. intervalStart . locLine),
        fromIntegral (i ^. intervalStart . locCol),
        intervalLength i,
        fromIntegral (i ^. intervalEnd . locLine),
        fromIntegral (i ^. intervalEnd . locCol)
      )

    rawWithLoc :: (a -> b) -> WithLoc a -> RawWithLoc b
    rawWithLoc f x = (rawInterval (getLoc x), f (x ^. withLocParam))

    rawType :: PropertyInfo -> RawType
    rawType PropertyInfo {..} = case _infoInfo of
      Symbol s -> s
      String s -> s
      _ -> error "unsupported"

    rawFace :: PropertyFace -> RawFace
    rawFace PropertyFace {..} = _faceFace

    rawTopDef :: PropertyTopDef -> RawTopDef
    rawTopDef PropertyTopDef {..} = _topDef

    rawGoto :: PropertyGoto -> RawGoto
    rawGoto PropertyGoto {..} =
      ( _gotoFile,
        fromIntegral (_gotoPos ^. locLine),
        fromIntegral (_gotoPos ^. locCol)
      )

instance IsProperty EmacsProperty where
  toProperties = \case
    EPropertyFace p -> toProperties p
    EPropertyInfo p -> toProperties p
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
        mkItem GenericProperty {..} = (Symbol (propertyIdText _gpropProperty), _gpropValue)

putProperty :: (IsProperty a) => WithRange a -> SExp
putProperty = addGenericProperties . fmap toProperties

putPropertyLoc :: (IsProperty a) => WithLoc a -> SExp
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
        { _gpropProperty = PropertyIdFace,
          _gpropValue = toSExp _faceFace
        }

instance IsProperty PropertyGoto where
  toProperties PropertyGoto {..} =
    pure
      GenericProperty
        { _gpropProperty = PropertyIdGoto,
          _gpropValue = gotoPair
        }
    where
      gotoPair = Pair (String (pack (toFilePath _gotoFile))) (Int (_gotoPos ^. locOffset . to (succ . fromIntegral)))

instance IsProperty PropertyInfo where
  toProperties PropertyInfo {..} =
    GenericProperty
      { _gpropProperty = PropertyIdInfo,
        _gpropValue = _infoInfo
      }
      :| [ GenericProperty
             { _gpropProperty = PropertyIdFormat,
               _gpropValue = _infoInit
             }
         ]

instance ToSExp LocProperties where
  toSExp LocProperties {..} =
    progn
      ( map putPropertyLoc _propertiesFace
          <> map putPropertyLoc _propertiesGoto
          <> map putPropertyLoc _propertiesInfo
      )
