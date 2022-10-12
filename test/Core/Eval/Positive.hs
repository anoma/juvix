module Core.Eval.Positive where

import Base
import Core.Eval.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: FilePath,
    _file :: FilePath,
    _expectedFile :: FilePath
  }

root :: FilePath
root = "tests/Core/positive"

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root </> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreEvalAssertion _file _expectedFile [] (const (return ()))
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore positive tests"
    (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Arithmetic operators"
      "."
      "test001.jvc"
      "out/test001.out",
    PosTest
      "Arithmetic operators inside lambdas"
      "."
      "test002.jvc"
      "out/test002.out",
    PosTest
      "Empty program with comments"
      "."
      "test003.jvc"
      "out/test003.out",
    PosTest
      "IO builtins"
      "."
      "test004.jvc"
      "out/test004.out",
    PosTest
      "Higher-order functions"
      "."
      "test005.jvc"
      "out/test005.out",
    PosTest
      "If-then-else"
      "."
      "test006.jvc"
      "out/test006.out",
    PosTest
      "Case"
      "."
      "test007.jvc"
      "out/test007.out",
    PosTest
      "Recursion"
      "."
      "test008.jvc"
      "out/test008.out",
    PosTest
      "Tail recursion"
      "."
      "test009.jvc"
      "out/test009.out",
    PosTest
      "Let"
      "."
      "test010.jvc"
      "out/test010.out",
    PosTest
      "Tail recursion: Fibonacci numbers in linear time"
      "."
      "test011.jvc"
      "out/test011.out",
    PosTest
      "Trees"
      "."
      "test012.jvc"
      "out/test012.out",
    PosTest
      "Functions returning functions with variable capture"
      "."
      "test013.jvc"
      "out/test013.out",
    PosTest
      "Arithmetic"
      "."
      "test014.jvc"
      "out/test014.out",
    PosTest
      "Local functions with free variables"
      "."
      "test015.jvc"
      "out/test015.out",
    PosTest
      "Recursion through higher-order functions"
      "."
      "test016.jvc"
      "out/test016.out",
    PosTest
      "Tail recursion through higher-order functions"
      "."
      "test017.jvc"
      "out/test017.out",
    PosTest
      "Higher-order functions and recursion"
      "."
      "test018.jvc"
      "out/test018.out",
    PosTest
      "Self-application"
      "."
      "test019.jvc"
      "out/test019.out",
    PosTest
      "Recursive functions: McCarthy's 91 function, subtraction by increments"
      "."
      "test020.jvc"
      "out/test020.out",
    PosTest
      "Higher-order recursive functions"
      "."
      "test021.jvc"
      "out/test021.out",
    PosTest
      "Fast exponentiation"
      "."
      "test022.jvc"
      "out/test022.out",
    PosTest
      "Lists"
      "."
      "test023.jvc"
      "out/test023.out",
    PosTest
      "Structural equality"
      "."
      "test024.jvc"
      "out/test024.out",
    PosTest
      "Mutual recursion"
      "."
      "test025.jvc"
      "out/test025.out",
    PosTest
      "Nested 'case', 'let' and 'if' with variable capture"
      "."
      "test026.jvc"
      "out/test026.out",
    PosTest
      "Euclid's algorithm"
      "."
      "test027.jvc"
      "out/test027.out",
    PosTest
      "Functional queues"
      "."
      "test028.jvc"
      "out/test028.out",
    PosTest
      "Church numerals"
      "."
      "test029.jvc"
      "out/test029.out",
    PosTest
      "Streams without memoization"
      "."
      "test030.jvc"
      "out/test030.out",
    PosTest
      "Ackermann function"
      "."
      "test031.jvc"
      "out/test031.out",
    PosTest
      "Ackermann function (higher-order definition)"
      "."
      "test032.jvc"
      "out/test032.out",
    PosTest
      "Nested lists"
      "."
      "test033.jvc"
      "out/test033.out",
    {-    PosTest
          "Evaluation order"
          "."
          "test034.jvc"
          "out/test034.out", -}
    PosTest
      "Merge sort"
      "."
      "test035.jvc"
      "out/test035.out",
    PosTest
      "Big numbers"
      "."
      "test036.jvc"
      "out/test036.out",
    PosTest
      "Global variables"
      "."
      "test037.jvc"
      "out/test037.out",
    PosTest
      "Global variables and forward declarations"
      "."
      "test038.jvc"
      "out/test038.out",
    PosTest
      "Eta-expansion of builtins and constructors"
      "."
      "test039.jvc"
      "out/test039.out",
    PosTest
      "LetRec"
      "."
      "test040.jvc"
      "out/test040.out",
    PosTest
      "Match with complex patterns"
      "."
      "test041.jvc"
      "out/test041.out"
  ]
