module Compilation.Positive where

import Base
import Compilation.Base

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Path Abs File
  }

makeLenses ''PosTest

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Compilation/positive/")

toTestDescr :: PosTest -> TestDescr
toTestDescr PosTest {..} =
  let tRoot = _dir
      file' = _file
      expected' = _expectedFile
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
              "Applications with lets and cases in function position"
            ]
            tests
        )
    )

posTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTest _name rdir rfile routfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = root <//> routfile
   in PosTest {..}

tests :: [PosTest]
tests =
  [ posTest
      "Arithmetic operators"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
      $(mkRelFile "out/test001.out"),
    posTest
      "Arithmetic operators inside lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix")
      $(mkRelFile "out/test002.out"),
    posTest
      "Integer arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test003.juvix")
      $(mkRelFile "out/test003.out"),
    posTest
      "IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test004.juvix")
      $(mkRelFile "out/test004.out"),
    posTest
      "Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test005.juvix")
      $(mkRelFile "out/test005.out"),
    posTest
      "If-then-else"
      $(mkRelDir ".")
      $(mkRelFile "test006.juvix")
      $(mkRelFile "out/test006.out"),
    posTest
      "Pattern matching and lambda-case"
      $(mkRelDir ".")
      $(mkRelFile "test007.juvix")
      $(mkRelFile "out/test007.out"),
    posTest
      "Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test008.juvix")
      $(mkRelFile "out/test008.out"),
    posTest
      "Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test009.juvix")
      $(mkRelFile "out/test009.out"),
    posTest
      "Let"
      $(mkRelDir ".")
      $(mkRelFile "test010.juvix")
      $(mkRelFile "out/test010.out"),
    posTest
      "Tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test011.juvix")
      $(mkRelFile "out/test011.out"),
    posTest
      "Trees"
      $(mkRelDir ".")
      $(mkRelFile "test012.juvix")
      $(mkRelFile "out/test012.out"),
    posTest
      "Functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test013.juvix")
      $(mkRelFile "out/test013.out"),
    posTest
      "Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test014.juvix")
      $(mkRelFile "out/test014.out"),
    posTest
      "Local functions with free variables"
      $(mkRelDir ".")
      $(mkRelFile "test015.juvix")
      $(mkRelFile "out/test015.out"),
    posTest
      "Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test016.juvix")
      $(mkRelFile "out/test016.out"),
    posTest
      "Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.juvix")
      $(mkRelFile "out/test017.out"),
    posTest
      "Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test018.juvix")
      $(mkRelFile "out/test018.out"),
    posTest
      "Self-application"
      $(mkRelDir ".")
      $(mkRelFile "test019.juvix")
      $(mkRelFile "out/test019.out"),
    posTest
      "Recursive functions: McCarthy's 91 function, subtraction by increments"
      $(mkRelDir ".")
      $(mkRelFile "test020.juvix")
      $(mkRelFile "out/test020.out"),
    posTest
      "Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test021.juvix")
      $(mkRelFile "out/test021.out"),
    posTest
      "Lists"
      $(mkRelDir ".")
      $(mkRelFile "test022.juvix")
      $(mkRelFile "out/test022.out"),
    posTest
      "Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test023.juvix")
      $(mkRelFile "out/test023.out"),
    posTest
      "Nested binders with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test024.juvix")
      $(mkRelFile "out/test024.out"),
    posTest
      "Euclid's algorithm"
      $(mkRelDir ".")
      $(mkRelFile "test025.juvix")
      $(mkRelFile "out/test025.out"),
    posTest
      "Functional queues"
      $(mkRelDir ".")
      $(mkRelFile "test026.juvix")
      $(mkRelFile "out/test026.out"),
    posTest
      "Church numerals"
      $(mkRelDir ".")
      $(mkRelFile "test027.juvix")
      $(mkRelFile "out/test027.out"),
    posTest
      "Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test028.juvix")
      $(mkRelFile "out/test028.out"),
    posTest
      "Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test029.juvix")
      $(mkRelFile "out/test029.out"),
    posTest
      "Ackermann function (higher-order definition)"
      $(mkRelDir ".")
      $(mkRelFile "test030.juvix")
      $(mkRelFile "out/test030.out"),
    posTest
      "Nested lists"
      $(mkRelDir ".")
      $(mkRelFile "test031.juvix")
      $(mkRelFile "out/test031.out"),
    posTest
      "Merge sort"
      $(mkRelDir ".")
      $(mkRelFile "test032.juvix")
      $(mkRelFile "out/test032.out"),
    posTest
      "Eta-expansion of builtins and constructors"
      $(mkRelDir ".")
      $(mkRelFile "test033.juvix")
      $(mkRelFile "out/test033.out"),
    posTest
      "Recursive let"
      $(mkRelDir ".")
      $(mkRelFile "test034.juvix")
      $(mkRelFile "out/test034.out"),
    posTest
      "Pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test035.juvix")
      $(mkRelFile "out/test035.out"),
    posTest
      "Eta-expansion"
      $(mkRelDir ".")
      $(mkRelFile "test036.juvix")
      $(mkRelFile "out/test036.out"),
    posTest
      "Applications with lets and cases in function position"
      $(mkRelDir ".")
      $(mkRelFile "test037.juvix")
      $(mkRelFile "out/test037.out")
  ]
