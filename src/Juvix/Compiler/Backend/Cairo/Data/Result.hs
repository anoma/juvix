module Juvix.Compiler.Backend.Cairo.Data.Result where

import Data.Aeson as Aeson hiding (Result)
import Data.Aeson.Types hiding (Result)
import Data.Vector qualified as V
import Juvix.Prelude hiding ((.=))

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
