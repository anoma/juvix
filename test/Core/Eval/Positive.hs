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
      "Test001: Arithmetic operators"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "out/test001.out"),
    PosTest
      "Test002: Arithmetic operators inside lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "out/test002.out"),
    PosTest
      "Test003: Empty program with comments"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvc")
      $(mkRelFile "out/test003.out"),
    PosTest
      "Test004: IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvc")
      $(mkRelFile "out/test004.out"),
    PosTest
      "Test005: Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvc")
      $(mkRelFile "out/test005.out"),
    PosTest
      "Test006: If-then-else"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvc")
      $(mkRelFile "out/test006.out"),
    PosTest
      "Test007: Case"
      $(mkRelDir ".")
      $(mkRelFile "test007.jvc")
      $(mkRelFile "out/test007.out"),
    PosTest
      "Test008: Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvc")
      $(mkRelFile "out/test008.out"),
    PosTest
      "Test009: Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvc")
      $(mkRelFile "out/test009.out"),
    PosTest
      "Test010: Let"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvc")
      $(mkRelFile "out/test010.out"),
    PosTest
      "Test011: Tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test011.jvc")
      $(mkRelFile "out/test011.out"),
    PosTest
      "Test012: Trees"
      $(mkRelDir ".")
      $(mkRelFile "test012.jvc")
      $(mkRelFile "out/test012.out"),
    PosTest
      "Test013: Functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test013.jvc")
      $(mkRelFile "out/test013.out"),
    PosTest
      "Test014: Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test014.jvc")
      $(mkRelFile "out/test014.out"),
    PosTest
      "Test015: Local functions with free variables"
      $(mkRelDir ".")
      $(mkRelFile "test015.jvc")
      $(mkRelFile "out/test015.out"),
    PosTest
      "Test016: Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test016.jvc")
      $(mkRelFile "out/test016.out"),
    PosTest
      "Test017: Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.jvc")
      $(mkRelFile "out/test017.out"),
    PosTest
      "Test018: Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test018.jvc")
      $(mkRelFile "out/test018.out"),
    PosTest
      "Test019: Self-application"
      $(mkRelDir ".")
      $(mkRelFile "test019.jvc")
      $(mkRelFile "out/test019.out"),
    PosTest
      "Test020: McCarthy's 91 function, subtraction by increments"
      $(mkRelDir ".")
      $(mkRelFile "test020.jvc")
      $(mkRelFile "out/test020.out"),
    PosTest
      "Test021: Higher-order recursive functions"
      $(mkRelDir ".")
      $(mkRelFile "test021.jvc")
      $(mkRelFile "out/test021.out"),
    PosTest
      "Test022: Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test022.jvc")
      $(mkRelFile "out/test022.out"),
    PosTest
      "Test023: Lists"
      $(mkRelDir ".")
      $(mkRelFile "test023.jvc")
      $(mkRelFile "out/test023.out"),
    PosTest
      "Test024: Structural equality"
      $(mkRelDir ".")
      $(mkRelFile "test024.jvc")
      $(mkRelFile "out/test024.out"),
    PosTest
      "Test025: Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test025.jvc")
      $(mkRelFile "out/test025.out"),
    PosTest
      "Test026: Nested 'case', 'let' and 'if' with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test026.jvc")
      $(mkRelFile "out/test026.out"),
    PosTest
      "Test027: Euclid's algorithm"
      $(mkRelDir ".")
      $(mkRelFile "test027.jvc")
      $(mkRelFile "out/test027.out"),
    PosTest
      "Test028: Functional queues"
      $(mkRelDir ".")
      $(mkRelFile "test028.jvc")
      $(mkRelFile "out/test028.out"),
    PosTest
      "Test029: Church numerals"
      $(mkRelDir ".")
      $(mkRelFile "test029.jvc")
      $(mkRelFile "out/test029.out"),
    PosTest
      "Test030: Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test030.jvc")
      $(mkRelFile "out/test030.out"),
    PosTest
      "Test031: Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test031.jvc")
      $(mkRelFile "out/test031.out"),
    PosTest
      "Test032: Ackermann function (higher-order definition)"
      $(mkRelDir ".")
      $(mkRelFile "test032.jvc")
      $(mkRelFile "out/test032.out"),
    PosTest
      "Test033: Nested lists"
      $(mkRelDir ".")
      $(mkRelFile "test033.jvc")
      $(mkRelFile "out/test033.out"),
    PosTest
      "Test034: Evaluation order"
      $(mkRelDir ".")
      $(mkRelFile "test034.jvc")
      $(mkRelFile "out/test034.out"),
    PosTest
      "Test035: Merge sort"
      $(mkRelDir ".")
      $(mkRelFile "test035.jvc")
      $(mkRelFile "out/test035.out"),
    PosTest
      "Test036: Big numbers"
      $(mkRelDir ".")
      $(mkRelFile "test036.jvc")
      $(mkRelFile "out/test036.out"),
    PosTest
      "Test037: Global variables"
      $(mkRelDir ".")
      $(mkRelFile "test037.jvc")
      $(mkRelFile "out/test037.out"),
    PosTest
      "Test038: Global variables and forward declarations"
      $(mkRelDir ".")
      $(mkRelFile "test038.jvc")
      $(mkRelFile "out/test038.out"),
    PosTest
      "Test039: Eta-expansion of builtins and constructors"
      $(mkRelDir ".")
      $(mkRelFile "test039.jvc")
      $(mkRelFile "out/test039.out"),
    PosTest
      "Test040: LetRec - fib, fact"
      $(mkRelDir ".")
      $(mkRelFile "test040.jvc")
      $(mkRelFile "out/test040.out"),
    PosTest
      "Test041: Match with complex patterns"
      $(mkRelDir ".")
      $(mkRelFile "test041.jvc")
      $(mkRelFile "out/test041.out"),
    PosTest
      "Test042: Type annotations"
      $(mkRelDir ".")
      $(mkRelFile "test042.jvc")
      $(mkRelFile "out/test042.out"),
    PosTest
      "Test043: Dependent lambda-abstractions"
      $(mkRelDir ".")
      $(mkRelFile "test043.jvc")
      $(mkRelFile "out/test043.out"),
    PosTest
      "Test044: Eta-expansion"
      $(mkRelDir ".")
      $(mkRelFile "test044.jvc")
      $(mkRelFile "out/test044.out"),
    PosTest
      "Test045: Type application and abstraction"
      $(mkRelDir ".")
      $(mkRelFile "test045.jvc")
      $(mkRelFile "out/test045.out"),
    PosTest
      "Test046: Applications with lets and cases in function position"
      $(mkRelDir ".")
      $(mkRelFile "test046.jvc")
      $(mkRelFile "out/test046.out"),
    PosTest
      "Test047: Builtin natural numbers"
      $(mkRelDir ".")
      $(mkRelFile "test047.jvc")
      $(mkRelFile "out/test047.out"),
    PosTest
      "Test048: String builtins"
      $(mkRelDir ".")
      $(mkRelFile "test048.jvc")
      $(mkRelFile "out/test048.out"),
    PosTest
      "Test049: Lifting and polymorphism"
      $(mkRelDir ".")
      $(mkRelFile "test049.jvc")
      $(mkRelFile "out/test049.out"),
    PosTest
      "Test050: Church numerals with pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test050.jvc")
      $(mkRelFile "out/test050.out"),
    PosTest
      "Test051: Type annotations for patterns"
      $(mkRelDir ".")
      $(mkRelFile "test051.jvc")
      $(mkRelFile "out/test051.out"),
    PosTest
      "Test052: foldl with match"
      $(mkRelDir ".")
      $(mkRelFile "test052.jvc")
      $(mkRelFile "out/test052.out"),
    PosTest
      "Test053: Match with higher-order polymorphic functions"
      $(mkRelDir ".")
      $(mkRelFile "test053.jvc")
      $(mkRelFile "out/test053.out"),
    PosTest
      "Test054: Typed match"
      $(mkRelDir ".")
      $(mkRelFile "test054.jvc")
      $(mkRelFile "out/test054.out"),
    PosTest
      "Test055: Eta-expansion of polymorphic constructors"
      $(mkRelDir ".")
      $(mkRelFile "test055.jvc")
      $(mkRelFile "out/test055.out"),
    PosTest
      "Test056: LetRec with type annotations"
      $(mkRelDir ".")
      $(mkRelFile "test056.jvc")
      $(mkRelFile "out/test056.out"),
    PosTest
      "Test057: Type synonyms"
      $(mkRelDir ".")
      $(mkRelFile "test057.jvc")
      $(mkRelFile "out/test057.out"),
    PosTest
      "Test058: Lifting and partial application"
      $(mkRelDir ".")
      $(mkRelFile "test058.jvc")
      $(mkRelFile "out/test058.out"),
    PosTest
      "Test059: Polymorphic type arguments"
      $(mkRelDir ".")
      $(mkRelFile "test059.jvc")
      $(mkRelFile "out/test059.out")
  ]
