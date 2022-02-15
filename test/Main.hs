module Main (main) where

import Base

import qualified Scope as Scope

negatives :: TestTree
negatives = testGroup "Negative tests" $
  map aTest $ concat
  [
  Scope.allTests

 ]

allTests :: TestTree
allTests = negatives

main :: IO ()
main = defaultMain allTests
