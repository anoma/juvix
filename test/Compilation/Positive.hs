module Compilation.Positive where

import Base
import Compilation.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Compilation/positive/")

toTestDescr :: PosTest -> TestDescr
toTestDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ compileAssertion file' expected'
        }

filterOutTests :: [String] -> [PosTest] -> [PosTest]
filterOutTests out = filter (\PosTest {..} -> _name `notElem` out)

allTests :: TestTree
allTests =
  testGroup
    "Juvix compilation pipeline positive tests"
    ( map
        (mkTest . toTestDescr)
        ( filterOutTests
            [ "Let",
              "Local functions with free variables",
              "Nested binders with variable capture",
              "Functional queues",
              "Merge sort",
              "Recursive let",
              "Applications with lets and cases in function position",
              "Self-application"
            ]
            tests
        )
    )

tests :: [PosTest]
tests =
  [ PosTest
      "Arithmetic operators"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Arithmetic operators inside lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix")
      $(mkRelFile "out/test002.out"),
    PosTest
      "IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test004.juvix")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test005.juvix")
      $(mkRelFile "out/test005.out"),
    PosTest
      "If-then-else"
      $(mkRelDir ".")
      $(mkRelFile "test006.juvix")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Pattern matching and lambda-case"
      $(mkRelDir ".")
      $(mkRelFile "test007.juvix")
      $(mkRelFile "out/test007.out"),
    PosTest
      "Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test008.juvix")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test009.juvix")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Let"
      $(mkRelDir ".")
      $(mkRelFile "test010.juvix")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test011.juvix")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Trees"
      $(mkRelDir ".")
      $(mkRelFile "test012.juvix")
      $(mkRelFile "out/test012.out"),
    PosTest
      "Functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test013.juvix")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test014.juvix")
      $(mkRelFile "out/test014.out"),
    PosTest
      "Local functions with free variables"
      $(mkRelDir ".")
      $(mkRelFile "test015.juvix")
      $(mkRelFile "out/test015.out"),
    PosTest
      "Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test016.juvix")
      $(mkRelFile "out/test016.out"),
    PosTest
      "Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.juvix")
      $(mkRelFile "out/test017.out"),
    PosTest
      "Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test018.juvix")
      $(mkRelFile "out/test018.out"),
    PosTest
      "Self-application"
      $(mkRelDir ".")
      $(mkRelFile "test019.juvix")
      $(mkRelFile "out/test019.out"),
    PosTest
      "Recursive functions: McCarthy's 91 function, subtraction by increments"
      $(mkRelDir ".")
      $(mkRelFile "test020.juvix")
      $(mkRelFile "out/test020.out"),
    PosTest
      "Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test021.juvix")
      $(mkRelFile "out/test021.out"),
    PosTest
      "Lists"
      $(mkRelDir ".")
      $(mkRelFile "test022.juvix")
      $(mkRelFile "out/test022.out"),
    PosTest
      "Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test023.juvix")
      $(mkRelFile "out/test023.out"),
    PosTest
      "Nested binders with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test024.juvix")
      $(mkRelFile "out/test024.out"),
    PosTest
      "Euclid's algorithm"
      $(mkRelDir ".")
      $(mkRelFile "test025.juvix")
      $(mkRelFile "out/test025.out"),
    PosTest
      "Functional queues"
      $(mkRelDir ".")
      $(mkRelFile "test026.juvix")
      $(mkRelFile "out/test026.out"),
    PosTest
      "Church numerals"
      $(mkRelDir ".")
      $(mkRelFile "test027.juvix")
      $(mkRelFile "out/test027.out"),
    PosTest
      "Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test028.juvix")
      $(mkRelFile "out/test028.out"),
    PosTest
      "Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test029.juvix")
      $(mkRelFile "out/test029.out"),
    PosTest
      "Ackermann function (higher-order definition)"
      $(mkRelDir ".")
      $(mkRelFile "test030.juvix")
      $(mkRelFile "out/test030.out"),
    PosTest
      "Nested lists"
      $(mkRelDir ".")
      $(mkRelFile "test031.juvix")
      $(mkRelFile "out/test031.out"),
    PosTest
      "Merge sort"
      $(mkRelDir ".")
      $(mkRelFile "test032.juvix")
      $(mkRelFile "out/test032.out"),
    PosTest
      "Eta-expansion of builtins and constructors"
      $(mkRelDir ".")
      $(mkRelFile "test033.juvix")
      $(mkRelFile "out/test033.out"),
    PosTest
      "Recursive let"
      $(mkRelDir ".")
      $(mkRelFile "test034.juvix")
      $(mkRelFile "out/test034.out"),
    PosTest
      "Pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test035.juvix")
      $(mkRelFile "out/test035.out"),
    PosTest
      "Eta-expansion"
      $(mkRelDir ".")
      $(mkRelFile "test036.juvix")
      $(mkRelFile "out/test036.out"),
    PosTest
      "Applications with lets and cases in function position"
      $(mkRelDir ".")
      $(mkRelFile "test037.juvix")
      $(mkRelFile "out/test037.out")
  ]
