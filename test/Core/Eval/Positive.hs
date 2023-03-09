module Core.Eval.Positive where

import Base
import Core.Eval.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _expectedFile :: Path Rel File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Core/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreEvalAssertion file' expected' [] (const (return ()))
        }

filterOutTests :: [String] -> [PosTest] -> [PosTest]
filterOutTests out = filter (\PosTest {..} -> _name `notElem` out)

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Arithmetic operators"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Arithmetic operators inside lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Empty program with comments"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvc")
      $(mkRelFile "out/test003.out"),
    PosTest
      "IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvc")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvc")
      $(mkRelFile "out/test005.out"),
    PosTest
      "If-then-else"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvc")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Case"
      $(mkRelDir ".")
      $(mkRelFile "test007.jvc")
      $(mkRelFile "out/test007.out"),
    PosTest
      "Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvc")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvc")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Let"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvc")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test011.jvc")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Trees"
      $(mkRelDir ".")
      $(mkRelFile "test012.jvc")
      $(mkRelFile "out/test012.out"),
    PosTest
      "Functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test013.jvc")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test014.jvc")
      $(mkRelFile "out/test014.out"),
    PosTest
      "Local functions with free variables"
      $(mkRelDir ".")
      $(mkRelFile "test015.jvc")
      $(mkRelFile "out/test015.out"),
    PosTest
      "Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test016.jvc")
      $(mkRelFile "out/test016.out"),
    PosTest
      "Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.jvc")
      $(mkRelFile "out/test017.out"),
    PosTest
      "Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test018.jvc")
      $(mkRelFile "out/test018.out"),
    PosTest
      "Self-application"
      $(mkRelDir ".")
      $(mkRelFile "test019.jvc")
      $(mkRelFile "out/test019.out"),
    PosTest
      "Recursive functions: McCarthy's 91 function, subtraction by increments"
      $(mkRelDir ".")
      $(mkRelFile "test020.jvc")
      $(mkRelFile "out/test020.out"),
    PosTest
      "Higher-order recursive functions"
      $(mkRelDir ".")
      $(mkRelFile "test021.jvc")
      $(mkRelFile "out/test021.out"),
    PosTest
      "Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test022.jvc")
      $(mkRelFile "out/test022.out"),
    PosTest
      "Lists"
      $(mkRelDir ".")
      $(mkRelFile "test023.jvc")
      $(mkRelFile "out/test023.out"),
    PosTest
      "Structural equality"
      $(mkRelDir ".")
      $(mkRelFile "test024.jvc")
      $(mkRelFile "out/test024.out"),
    PosTest
      "Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test025.jvc")
      $(mkRelFile "out/test025.out"),
    PosTest
      "Nested 'case', 'let' and 'if' with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test026.jvc")
      $(mkRelFile "out/test026.out"),
    PosTest
      "Euclid's algorithm"
      $(mkRelDir ".")
      $(mkRelFile "test027.jvc")
      $(mkRelFile "out/test027.out"),
    PosTest
      "Functional queues"
      $(mkRelDir ".")
      $(mkRelFile "test028.jvc")
      $(mkRelFile "out/test028.out"),
    PosTest
      "Church numerals"
      $(mkRelDir ".")
      $(mkRelFile "test029.jvc")
      $(mkRelFile "out/test029.out"),
    PosTest
      "Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test030.jvc")
      $(mkRelFile "out/test030.out"),
    PosTest
      "Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test031.jvc")
      $(mkRelFile "out/test031.out"),
    PosTest
      "Ackermann function (higher-order definition)"
      $(mkRelDir ".")
      $(mkRelFile "test032.jvc")
      $(mkRelFile "out/test032.out"),
    PosTest
      "Nested lists"
      $(mkRelDir ".")
      $(mkRelFile "test033.jvc")
      $(mkRelFile "out/test033.out"),
    {-    PosTest
          "Evaluation order"
          $(mkRelDir ".")
          $(mkRelFile "test034.jvc")
          $(mkRelFile "out/test034.out"), -}
    PosTest
      "Merge sort"
      $(mkRelDir ".")
      $(mkRelFile "test035.jvc")
      $(mkRelFile "out/test035.out"),
    PosTest
      "Big numbers"
      $(mkRelDir ".")
      $(mkRelFile "test036.jvc")
      $(mkRelFile "out/test036.out"),
    PosTest
      "Global variables"
      $(mkRelDir ".")
      $(mkRelFile "test037.jvc")
      $(mkRelFile "out/test037.out"),
    PosTest
      "Global variables and forward declarations"
      $(mkRelDir ".")
      $(mkRelFile "test038.jvc")
      $(mkRelFile "out/test038.out"),
    PosTest
      "Eta-expansion of builtins and constructors"
      $(mkRelDir ".")
      $(mkRelFile "test039.jvc")
      $(mkRelFile "out/test039.out"),
    PosTest
      "LetRec"
      $(mkRelDir ".")
      $(mkRelFile "test040.jvc")
      $(mkRelFile "out/test040.out"),
    PosTest
      "Match with complex patterns"
      $(mkRelDir ".")
      $(mkRelFile "test041.jvc")
      $(mkRelFile "out/test041.out"),
    PosTest
      "Type annotations"
      $(mkRelDir ".")
      $(mkRelFile "test042.jvc")
      $(mkRelFile "out/test042.out"),
    PosTest
      "Dependent lambda-abstractions"
      $(mkRelDir ".")
      $(mkRelFile "test043.jvc")
      $(mkRelFile "out/test043.out"),
    PosTest
      "Eta-expansion"
      $(mkRelDir ".")
      $(mkRelFile "test044.jvc")
      $(mkRelFile "out/test044.out"),
    PosTest
      "Type application and abstraction"
      $(mkRelDir ".")
      $(mkRelFile "test045.jvc")
      $(mkRelFile "out/test045.out"),
    PosTest
      "Applications with lets and cases in function position"
      $(mkRelDir ".")
      $(mkRelFile "test046.jvc")
      $(mkRelFile "out/test046.out"),
    PosTest
      "Builtin natural numbers"
      $(mkRelDir ".")
      $(mkRelFile "test047.jvc")
      $(mkRelFile "out/test047.out"),
    PosTest
      "String builtins"
      $(mkRelDir ".")
      $(mkRelFile "test048.jvc")
      $(mkRelFile "out/test048.out"),
    PosTest
      "Lifting and polymorphism"
      $(mkRelDir ".")
      $(mkRelFile "test049.jvc")
      $(mkRelFile "out/test049.out"),
    PosTest
      "Church numerals with pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test050.jvc")
      $(mkRelFile "out/test050.out"),
    PosTest
      "Type annotations for patterns"
      $(mkRelDir ".")
      $(mkRelFile "test051.jvc")
      $(mkRelFile "out/test051.out"),
    PosTest
      "foldl with match"
      $(mkRelDir ".")
      $(mkRelFile "test052.jvc")
      $(mkRelFile "out/test052.out"),
    PosTest
      "Match with higher-order polymorphic functions"
      $(mkRelDir ".")
      $(mkRelFile "test053.jvc")
      $(mkRelFile "out/test053.out"),
    PosTest
      "Typed match"
      $(mkRelDir ".")
      $(mkRelFile "test054.jvc")
      $(mkRelFile "out/test054.out"),
    PosTest
      "Eta-expansion of polymorphic constructors"
      $(mkRelDir ".")
      $(mkRelFile "test055.jvc")
      $(mkRelFile "out/test055.out"),
    PosTest
      "LetRec with type annotations"
      $(mkRelDir ".")
      $(mkRelFile "test056.jvc")
      $(mkRelFile "out/test056.out"),
    PosTest
      "Type synonyms"
      $(mkRelDir ".")
      $(mkRelFile "test057.jvc")
      $(mkRelFile "out/test057.out"),
    PosTest
      "Lifting and partial application"
      $(mkRelDir ".")
      $(mkRelFile "test058.jvc")
      $(mkRelFile "out/test058.out")
  ]
