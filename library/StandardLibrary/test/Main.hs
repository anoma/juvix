module Main where

import Mari.Library
import qualified NameSymb
import qualified Pretty
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ NameSymb.top,
      Pretty.top
    ]

main :: IO ()
main = do
  T.defaultMain allCheckedTests
