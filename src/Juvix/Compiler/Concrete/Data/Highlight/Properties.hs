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

data Properties = Properties
  { _propertiesGoto :: [PropertyGoto],
    _propertiesFace :: [PropertyFace]
  }

data RawProperties = RawProperties
  { _rawPropertiesFace :: [RawFace],
    _rawPropertiesGoto :: [RawGoto]
  }

-- | (File, Row, Col, Length)
type RawInterval = (Path Abs File, Int, Int, Int)

type RawFace = (RawInterval, Face)

-- | (Interval, TargetFile, TargetLine, TargetColumn)
type RawGoto = (RawInterval, Path Abs File, Int, Int)

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
      _rawPropertiesFace = map rawFace _propertiesFace
    }
  where
    rawInterval :: Interval -> RawInterval
    rawInterval i =
      ( i ^. intervalFile,
        fromIntegral (i ^. intervalStart . locLine),
        fromIntegral (i ^. intervalStart . locCol),
        intervalLength i
      )
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
instance ToSexp PropertyFace where
  toSexp PropertyFace {..} =
    App [Symbol "add-text-properties", start, end, face]
    where
      i = _faceInterval
      f = _faceFace
      pos l = Int (succ (l ^. locOffset . unPos))
      start = pos (i ^. intervalStart)
      end = pos (i ^. intervalEnd)
      face = Quote (App [Symbol "face", faceSymbol (faceSymbolStr f)])

instance ToSexp PropertyGoto where
  toSexp PropertyGoto {..} =
    App [Symbol "add-text-properties", start, end, goto]
    where
      i = _gotoInterval
      targetPos = _gotoPos
      targetFile = _gotoFile
      goto = Quote (App [Symbol "juvix-goto", gotoPair])
      pos l = Int (succ (l ^. locOffset . unPos))
      start = pos (i ^. intervalStart)
      end = pos (i ^. intervalEnd)
      gotoPair = Pair (String (toFilePath targetFile)) (Int (targetPos ^. locOffset . to (succ . fromIntegral)))

instance ToSexp Properties where
  toSexp Properties {..} =
    progn
      ( map toSexp _propertiesFace
          <> map toSexp _propertiesGoto
      )
