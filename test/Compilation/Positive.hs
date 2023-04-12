module Compilation.Positive where

import Base
import Compilation.Base

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _assertionMode :: CompileAssertionMode,
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
          _testAssertion = Steps $ compileAssertion _assertionMode file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix compilation pipeline positive tests"
    (map (mkTest . toTestDescr) tests)

posTest' :: CompileAssertionMode -> String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTest' _assertionMode _name rdir rfile routfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = root <//> routfile
   in PosTest {..}

posTestStdin :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> Text -> PosTest
posTestStdin _name rdir rfile routfile _stdinText =
  let t = posTest _name rdir rfile routfile
   in t
        { _assertionMode = CompileOnly _stdinText
        }

posTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTest = posTest' EvalAndCompile

-- tests which use large integers are only evaluated but not compiled
posTestEval :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTestEval = posTest' EvalOnly

tests :: [PosTest]
tests =
  [ posTest
      "Test001: Arithmetic operators"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
      $(mkRelFile "out/test001.out"),
    posTest
      "Test002: Arithmetic operators inside lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix")
      $(mkRelFile "out/test002.out"),
    posTest
      "Test003: Integer arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test003.juvix")
      $(mkRelFile "out/test003.out"),
    posTest
      "Test004: IO builtins"
      $(mkRelDir ".")
      $(mkRelFile "test004.juvix")
      $(mkRelFile "out/test004.out"),
    posTest
      "Test005: Higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test005.juvix")
      $(mkRelFile "out/test005.out"),
    posTest
      "Test006: If-then-else and lazy boolean operators"
      $(mkRelDir ".")
      $(mkRelFile "test006.juvix")
      $(mkRelFile "out/test006.out"),
    posTest
      "Test007: Pattern matching and lambda-case"
      $(mkRelDir ".")
      $(mkRelFile "test007.juvix")
      $(mkRelFile "out/test007.out"),
    posTest
      "Test008: Recursion"
      $(mkRelDir ".")
      $(mkRelFile "test008.juvix")
      $(mkRelFile "out/test008.out"),
    posTest
      "Test009: Tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test009.juvix")
      $(mkRelFile "out/test009.out"),
    posTest
      "Test010: Let"
      $(mkRelDir ".")
      $(mkRelFile "test010.juvix")
      $(mkRelFile "out/test010.out"),
    posTestEval
      "Test011: Tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test011.juvix")
      $(mkRelFile "out/test011.out"),
    posTest
      "Test012: Trees"
      $(mkRelDir ".")
      $(mkRelFile "test012.juvix")
      $(mkRelFile "out/test012.out"),
    posTest
      "Test013: Functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test013.juvix")
      $(mkRelFile "out/test013.out"),
    posTest
      "Test014: Arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test014.juvix")
      $(mkRelFile "out/test014.out"),
    posTest
      "Test015: Local functions with free variables"
      $(mkRelDir ".")
      $(mkRelFile "test015.juvix")
      $(mkRelFile "out/test015.out"),
    posTest
      "Test016: Recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test016.juvix")
      $(mkRelFile "out/test016.out"),
    posTest
      "Test017: Tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test017.juvix")
      $(mkRelFile "out/test017.out"),
    posTest
      "Test018: Higher-order functions and recursion"
      $(mkRelDir ".")
      $(mkRelFile "test018.juvix")
      $(mkRelFile "out/test018.out"),
    posTest
      "Test019: Self-application"
      $(mkRelDir ".")
      $(mkRelFile "test019.juvix")
      $(mkRelFile "out/test019.out"),
    posTest
      "Test020: Recursive functions: McCarthy's 91 function, subtraction by increments"
      $(mkRelDir ".")
      $(mkRelFile "test020.juvix")
      $(mkRelFile "out/test020.out"),
    posTest
      "Test021: Fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test021.juvix")
      $(mkRelFile "out/test021.out"),
    posTest
      "Test022: Lists"
      $(mkRelDir ".")
      $(mkRelFile "test022.juvix")
      $(mkRelFile "out/test022.out"),
    posTest
      "Test023: Mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test023.juvix")
      $(mkRelFile "out/test023.out"),
    posTest
      "Test024: Nested binders with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test024.juvix")
      $(mkRelFile "out/test024.out"),
    posTest
      "Test025: Euclid's algorithm"
      $(mkRelDir ".")
      $(mkRelFile "test025.juvix")
      $(mkRelFile "out/test025.out"),
    posTest
      "Test026: Functional queues"
      $(mkRelDir ".")
      $(mkRelFile "test026.juvix")
      $(mkRelFile "out/test026.out"),
    posTest
      "Test027: Church numerals"
      $(mkRelDir ".")
      $(mkRelFile "test027.juvix")
      $(mkRelFile "out/test027.out"),
    posTest
      "Test028: Streams without memoization"
      $(mkRelDir ".")
      $(mkRelFile "test028.juvix")
      $(mkRelFile "out/test028.out"),
    posTest
      "Test029: Ackermann function"
      $(mkRelDir ".")
      $(mkRelFile "test029.juvix")
      $(mkRelFile "out/test029.out"),
    posTest
      "Test030: Ackermann function (higher-order definition)"
      $(mkRelDir ".")
      $(mkRelFile "test030.juvix")
      $(mkRelFile "out/test030.out"),
    posTest
      "Test031: Nested lists"
      $(mkRelDir ".")
      $(mkRelFile "test031.juvix")
      $(mkRelFile "out/test031.out"),
    posTest
      "Test032: Merge sort"
      $(mkRelDir ".")
      $(mkRelFile "test032.juvix")
      $(mkRelFile "out/test032.out"),
    posTest
      "Test033: Eta-expansion of builtins and constructors"
      $(mkRelDir ".")
      $(mkRelFile "test033.juvix")
      $(mkRelFile "out/test033.out"),
    posTest
      "Test034: Recursive let"
      $(mkRelDir ".")
      $(mkRelFile "test034.juvix")
      $(mkRelFile "out/test034.out"),
    posTest
      "Test035: Pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test035.juvix")
      $(mkRelFile "out/test035.out"),
    posTest
      "Test036: Eta-expansion"
      $(mkRelDir ".")
      $(mkRelFile "test036.juvix")
      $(mkRelFile "out/test036.out"),
    posTest
      "Test037: Applications with lets and cases in function position"
      $(mkRelDir ".")
      $(mkRelFile "test037.juvix")
      $(mkRelFile "out/test037.out"),
    posTest
      "Test038: Simple case expression"
      $(mkRelDir ".")
      $(mkRelFile "test038.juvix")
      $(mkRelFile "out/test038.out"),
    posTest
      "Test039: Mutually recursive let expression"
      $(mkRelDir ".")
      $(mkRelFile "test039.juvix")
      $(mkRelFile "out/test039.out"),
    posTest
      "Test040: Pattern matching nullary constructor"
      $(mkRelDir ".")
      $(mkRelFile "test040.juvix")
      $(mkRelFile "out/test040.out"),
    posTest
      "Test041: Use a builtin inductive in an inductive constructor"
      $(mkRelDir ".")
      $(mkRelFile "test041.juvix")
      $(mkRelFile "out/test041.out"),
    posTest
      "Test042: Builtin string-to-nat"
      $(mkRelDir ".")
      $(mkRelFile "test042.juvix")
      $(mkRelFile "out/test042.out"),
    posTest
      "Test043: Builtin trace"
      $(mkRelDir ".")
      $(mkRelFile "test043.juvix")
      $(mkRelFile "out/test043.out"),
    posTestStdin
      "Test044: Builtin readline"
      $(mkRelDir ".")
      $(mkRelFile "test044.juvix")
      $(mkRelFile "out/test044.out")
      "a\n",
    posTest
      "Test045: Implicit builtin bool"
      $(mkRelDir ".")
      $(mkRelFile "test045.juvix")
      $(mkRelFile "out/test045.out"),
    posTest
      "Test046: Polymorphic type arguments"
      $(mkRelDir ".")
      $(mkRelFile "test046.juvix")
      $(mkRelFile "out/test046.out"),
    posTest
      "Test047: Local Modules"
      $(mkRelDir ".")
      $(mkRelFile "test047.juvix")
      $(mkRelFile "out/test047.out"),
    posTest
      "Test048: String quoting"
      $(mkRelDir ".")
      $(mkRelFile "test048.juvix")
      $(mkRelFile "out/test048.out"),
    posTest
      "Test049: Builtin Int"
      $(mkRelDir ".")
      $(mkRelFile "test049.juvix")
      $(mkRelFile "out/test049.out"),
    posTest
      "Test050: Pattern matching with integers"
      $(mkRelDir ".")
      $(mkRelFile "test050.juvix")
      $(mkRelFile "out/test050.out")
  ]
