module Juvix.Compiler.Backend.Cairo.Data.Result where

import Data.Aeson as Aeson hiding (Result)
import Juvix.Prelude hiding ((.=))

data Result = Result
  { _resultData :: [Text]
  }

makeLenses ''Result

instance ToJSON Result where
  toJSON Result {..} =
    object
      [ "data" .= toJSON _resultData,
        "attributes" .= Array mempty,
        "builtins" .= Array mempty,
        "hints" .= object [],
        "identifiers"
          .= object
            [ "__main__.__start__"
                .= object
                  [ "pc" .= Number 0,
                    "type" .= String "label"
                  ],
              "__main__.__end__"
                .= object
                  [ "pc" .= Number (fromIntegral $ length _resultData),
                    "type" .= String "label"
                  ],
              "__main__.main"
                .= object
                  [ "pc" .= Number 0,
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
