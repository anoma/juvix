module Main where

import Mari.Library
import qualified Sexp
import qualified Sexp.Parser
import qualified Sexp.Serialize
import qualified Sexp.SimplifiedPasses
import qualified Test.Tasty as T

allCheckedTests :: T.TestTree
allCheckedTests =
  T.testGroup
    "All tests that are checked"
    [ Sexp.top,
      Sexp.Parser.top,
      Sexp.SimplifiedPasses.top,
      Sexp.Serialize.top
    ]

main :: IO ()
main =
  T.defaultMain allCheckedTests
