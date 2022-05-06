module Anoma.Base where

import Prelude

readPre :: String -> Int
readPre "change1-key" = 100
readPre "change2-key" = 90
readPre _ = -1

readPost :: String -> Int
readPost "change1-key" = 90
readPost "change2-key" = 100
readPost _ = -1

isBalanceKey :: String -> String -> String
isBalanceKey _ _ = "owner-address"
