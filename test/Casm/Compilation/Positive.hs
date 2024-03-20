module Casm.Compilation.Positive where

import Base
import Casm.Compilation.Base
import Data.HashSet qualified as HashSet

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Path Abs File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Casm/Compilation/positive/")

toTestDescr :: Int -> PosTest -> TestDescr
toTestDescr optLevel PosTest {..} =
  let tRoot = _dir
      file' = _file
      expected' = _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ compileAssertion _dir optLevel file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix to CASM positive tests"
    (map (mkTest . toTestDescr 3) tests)

allTestsNoOptimize :: TestTree
allTestsNoOptimize =
  testGroup
    "Juvix to CASM positive tests (no optimization)"
    (map (mkTest . toTestDescr 0) tests)

posTest :: String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTest _name rdir rfile routfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = root <//> routfile
   in PosTest {..}

isIgnored :: PosTest -> Bool
isIgnored t = HashSet.member (t ^. name) ignored

ignored :: HashSet String
ignored =
  HashSet.fromList
    [ -- strings not supported (Partial trait)
      "Test026: Functional queues"
    ]

tests :: [PosTest]
tests =
  filter
    (not . isIgnored)
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
        "Test050: Pattern matching with integers"
        $(mkRelDir ".")
        $(mkRelFile "test050.juvix")
        $(mkRelFile "out/test050.out"),
      posTest
        "Test053: Inlining"
        $(mkRelDir ".")
        $(mkRelFile "test053.juvix")
        $(mkRelFile "out/test053.out"),
      posTest
        "Test054: Iterators"
        $(mkRelDir ".")
        $(mkRelFile "test054.juvix")
        $(mkRelFile "out/test054.out"),
      posTest
        "Test056: Argument specialization"
        $(mkRelDir ".")
        $(mkRelFile "test056.juvix")
        $(mkRelFile "out/test056.out"),
      posTest
        "Test057: Case folding"
        $(mkRelDir ".")
        $(mkRelFile "test057.juvix")
        $(mkRelFile "out/test057.out"),
      posTest
        "Test058: Ranges"
        $(mkRelDir ".")
        $(mkRelFile "test058.juvix")
        $(mkRelFile "out/test058.out"),
      posTest
        "Test059: Builtin list"
        $(mkRelDir ".")
        $(mkRelFile "test059.juvix")
        $(mkRelFile "out/test059.out"),
      posTest
        "Test060: Record update"
        $(mkRelDir ".")
        $(mkRelFile "test060.juvix")
        $(mkRelFile "out/test060.out"),
      posTest
        "Test062: Overapplication"
        $(mkRelDir ".")
        $(mkRelFile "test062.juvix")
        $(mkRelFile "out/test062.out"),
      posTest
        "Test064: Constant folding"
        $(mkRelDir ".")
        $(mkRelFile "test064.juvix")
        $(mkRelFile "out/test064.out"),
      posTest
        "Test065: Arithmetic simplification"
        $(mkRelDir ".")
        $(mkRelFile "test065.juvix")
        $(mkRelFile "out/test065.out"),
      posTest
        "Test066: Import function with a function call in default argument"
        $(mkRelDir "test066")
        $(mkRelFile "M.juvix")
        $(mkRelFile "out/test066.out"),
      posTest
        "Test067: Dependent default values inserted during translation FromConcrete"
        $(mkRelDir ".")
        $(mkRelFile "test067.juvix")
        $(mkRelFile "out/test067.out"),
      posTest
        "Test068: Dependent default values inserted in the arity checker"
        $(mkRelDir ".")
        $(mkRelFile "test068.juvix")
        $(mkRelFile "out/test068.out"),
      posTest
        "Test069: Dependent default values for Ord trait"
        $(mkRelDir ".")
        $(mkRelFile "test069.juvix")
        $(mkRelFile "out/test069.out"),
      posTest
        "Test070: Nested default values and named arguments"
        $(mkRelDir ".")
        $(mkRelFile "test070.juvix")
        $(mkRelFile "out/test070.out"),
      posTest
        "Test071: Named application (Ord instance with default cmp)"
        $(mkRelDir ".")
        $(mkRelFile "test071.juvix")
        $(mkRelFile "out/test071.out"),
      posTest
        "Test072: Monad transformers (ReaderT + StateT + Identity)"
        $(mkRelDir "test072")
        $(mkRelFile "ReaderT.juvix")
        $(mkRelFile "out/test072.out"),
      posTest
        "Test073: Import and use a syntax alias"
        $(mkRelDir "test073")
        $(mkRelFile "test073.juvix")
        $(mkRelFile "out/test073.out"),
      posTest
        "Test074: Fields"
        $(mkRelDir ".")
        $(mkRelFile "test074.juvix")
        $(mkRelFile "out/test074.out")
    ]
