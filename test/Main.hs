module Main (main) where

import Base

import qualified Scope

negatives :: TestTree
negatives = testGroup "MiniJuvix tests" $
  [
  Scope.allTests
  ]

allTests :: TestTree
allTests = negatives

main :: IO ()
main = defaultMain allTests
