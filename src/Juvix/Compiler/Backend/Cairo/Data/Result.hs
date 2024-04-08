module Juvix.Compiler.Backend.Cairo.Data.Result where

import Data.Aeson as Aeson hiding (Result)
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types hiding (Result)
import Data.Vector qualified as V
import Juvix.Prelude hiding ((.=))
import Text.Read qualified as T

data Result = Result
  { _resultData :: [Text],
    _resultStart :: Int,
    _resultEnd :: Int,
    _resultMain :: Int,
    _resultHints :: [(Int, Text)],
    _resultBuiltins :: [Text]
  }

makeLenses ''Result

instance ToJSON Result where
  toJSON Result {..} =
    object
      [ "data" .= toJSON _resultData,
        "attributes" .= Array mempty,
        "builtins" .= toJSON _resultBuiltins,
        "hints" .= object (map mkHint _resultHints),
        "identifiers"
          .= object
            [ "__main__.__start__"
                .= object
                  [ "pc" .= Number (fromIntegral _resultStart),
                    "type" .= String "label"
                  ],
              "__main__.__end__"
                .= object
                  [ "pc" .= Number (fromIntegral _resultEnd),
                    "type" .= String "label"
                  ],
              "__main__.main"
                .= object
                  [ "pc" .= Number (fromIntegral _resultMain),
                    "decorators" .= Array mempty,
                    "type" .= String "function"
                  ]
            ],
        "main_scope" .= String "__main__",
        "prime" .= String "0x800000000000011000000000000000000000000000000000000000000000001",
        "reference_manager"
          .= object
            [ "references" .= Array mempty
            ]
      ]
    where
      mkHint :: (Int, Text) -> Pair
      mkHint (pc, hintCode) = (fromString (show pc), Array $ V.fromList [hint])
        where
          hint =
            object
              [ "accessible_scopes" .= Array mempty,
                "code" .= hintCode,
                "flow_tracking_data"
                  .= object
                    [ "ap_tracking"
                        .= object
                          [ "group" .= Number 0,
                            "offset" .= Number 0
                          ],
                      "reference_ids" .= object []
                    ]
              ]

instance FromJSON Result where
  parseJSON val = case val of
    Object obj -> do
      _resultData <- maybe (return []) parseJSON (KeyMap.lookup "data" obj)
      _resultBuiltins <- maybe (return []) parseJSON (KeyMap.lookup "builtins" obj)
      (_resultStart, _resultEnd, _resultMain) <- maybe (return (0, 0, 0)) parseIdents (KeyMap.lookup "identifiers" obj)
      _resultHints <- maybe (return []) parseHints (KeyMap.lookup "hints" obj)
      return Result {..}
    _ ->
      typeMismatch "Object" val
    where
      parseIdents :: Value -> Parser (Int, Int, Int)
      parseIdents val' = case val' of
        Object obj -> do
          startPc <- maybe (return 0) parseIdentPc (KeyMap.lookup "__main__.__start__" obj)
          endPc <- maybe (return 0) parseIdentPc (KeyMap.lookup "__main__.__end__" obj)
          mainPc <- maybe (return 0) parseIdentPc (KeyMap.lookup "__main__.main" obj)
          return (startPc, endPc, mainPc)
        _ ->
          typeMismatch "Object" val'

      parseIdentPc :: Value -> Parser Int
      parseIdentPc val' = case val' of
        Object obj ->
          maybe (return 0) parseJSON (KeyMap.lookup "pc" obj)
        _ ->
          typeMismatch "Object" val'

      parseHints :: Value -> Parser [(Int, Text)]
      parseHints val' = case val' of
        Object obj -> do
          lst <-
            mapM
              ( \(k, v) -> do
                  v' <- parseHint v
                  return (Aeson.toText k, v')
              )
              (KeyMap.toList obj)
          mapM (firstM (maybe (typeMismatch "Integer" val') return . T.readMaybe . unpack)) lst
        _ ->
          typeMismatch "Object" val'

      parseHint :: Value -> Parser Text
      parseHint val' = case val' of
        Array arr -> case toList arr of
          [Object obj] ->
            maybe (return "") parseJSON (KeyMap.lookup "code" obj)
          _ ->
            typeMismatch "singleton Array" val'
        _ ->
          typeMismatch "singleton Array" val'
