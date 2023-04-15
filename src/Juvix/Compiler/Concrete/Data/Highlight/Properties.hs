module Juvix.Compiler.Concrete.Data.Highlight.Properties where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH
import Juvix.Compiler.Concrete.Data.Highlight.SExp
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Lens.Micro.Platform qualified as Lens

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

faceSymbol :: Text -> SExp
faceSymbol faceSymbolTxt = Symbol ("juvix-highlight-" <> faceSymbolTxt <> "-face")

instance ToJSON Face where
  toJSON = Aeson.String . faceSymbolStr

data PropertyGoto = PropertyGoto
  { _gotoInterval :: Interval,
    _gotoFile :: Path Abs File,
    _gotoPos :: FileLoc
  }

data PropertyFace = PropertyFace
  { _faceInterval :: Interval,
    _faceFace :: Face
  }

data PropertyType = PropertyType
  { _typeInterval :: Interval,
    _typeType :: Text
  }

data Properties = Properties
  { _propertiesGoto :: [PropertyGoto],
    _propertiesFace :: [PropertyFace],
    _propertiesType :: [PropertyType]
  }

data RawProperties = RawProperties
  { _rawPropertiesFace :: [RawFace],
    _rawPropertiesGoto :: [RawGoto],
    _rawPropertiesType :: [RawType]
  }

-- | (File, Row, Col, Length)
type RawInterval = (Path Abs File, Int, Int, Int)

type RawFace = (RawInterval, Face)

-- | (Interval, TargetFile, TargetLine, TargetColumn)
type RawGoto = (RawInterval, Path Abs File, Int, Int)

-- | (Interval, Type)
type RawType = (RawInterval, Text)

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier = over Lens._head toLower . dropPrefix "_rawProperties",
         constructorTagModifier = map toLower
       }
     ''RawProperties
 )

rawProperties :: Properties -> RawProperties
rawProperties Properties {..} =
  RawProperties
    { _rawPropertiesGoto = map rawGoto _propertiesGoto,
      _rawPropertiesFace = map rawFace _propertiesFace,
      _rawPropertiesType = map rawType _propertiesType
    }
  where
    rawInterval :: Interval -> RawInterval
    rawInterval i =
      ( i ^. intervalFile,
        fromIntegral (i ^. intervalStart . locLine),
        fromIntegral (i ^. intervalStart . locCol),
        intervalLength i
      )
    rawType :: PropertyType -> RawType
    rawType PropertyType {..} = (rawInterval _typeInterval, _typeType)

    rawFace :: PropertyFace -> RawFace
    rawFace PropertyFace {..} = (rawInterval _faceInterval, _faceFace)

    rawGoto :: PropertyGoto -> RawGoto
    rawGoto PropertyGoto {..} =
      ( rawInterval _gotoInterval,
        _gotoFile,
        fromIntegral (_gotoPos ^. locLine),
        fromIntegral (_gotoPos ^. locCol)
      )

-- | Example instruction:
-- (add-text-properties 20 28
--  '(face juvix-highlight-constructor-face))
instance ToSExp PropertyFace where
  toSExp PropertyFace {..} =
    App [Symbol "add-text-properties", start, end, face]
    where
      i = _faceInterval
      f = _faceFace
      pos l = Int (succ (l ^. locOffset . unPos))
      start = pos (i ^. intervalStart)
      end = pos (i ^. intervalEnd)
      face = Quote (App [Symbol "face", faceSymbol (faceSymbolStr f)])

instance ToSExp PropertyGoto where
  toSExp PropertyGoto {..} =
    App [Symbol "add-text-properties", start, end, goto]
    where
      i = _gotoInterval
      targetPos = _gotoPos
      targetFile = _gotoFile
      goto = Quote (App [Symbol "juvix-goto", gotoPair])
      pos l = Int (succ (l ^. locOffset . unPos))
      start :: SExp = pos (i ^. intervalStart)
      end = pos (i ^. intervalEnd)
      gotoPair = Pair (String (pack (toFilePath targetFile))) (Int (targetPos ^. locOffset . to (succ . fromIntegral)))

-- (put-text-property 2 3 'display '(raise 0.5))
instance ToSExp PropertyType where
  toSExp PropertyType {..} =
    App
      [Symbol "put-text-property", start, end, echoHelp, String _typeType]
    where
      i = _typeInterval
      pos l = Int (succ (l ^. locOffset . unPos))
      echoHelp = Quote (Symbol "help-echo")
      start = pos (i ^. intervalStart)
      end = pos (i ^. intervalEnd)

instance ToSExp Properties where
  toSExp Properties {..} =
    progn
      ( map toSExp _propertiesFace
          <> map toSExp _propertiesGoto
          <> map toSExp _propertiesType
      )
