module Compilation.Positive where

import Base
import Compilation.Base
import Data.HashSet qualified as HashSet

data PosTest = PosTest
  { _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _assertionMode :: CompileAssertionMode,
    _expectedFile :: Path Abs File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Compilation/positive/")

toTestDescr :: Int -> PosTest -> TestDescr
toTestDescr optLevel PosTest {..} =
  let tRoot = _dir
      file' = _file
      expected' = _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ compileAssertion _dir optLevel _assertionMode file' expected'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix compilation pipeline positive tests"
    (map (mkTest . toTestDescr 3) tests)

allTestsNoOptimize :: TestTree
allTestsNoOptimize =
  testGroup
    "Juvix compilation pipeline positive tests (no optimization)"
    (map (mkTest . toTestDescr 0) tests)

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

isIgnored :: PosTest -> Bool
isIgnored t = HashSet.member (t ^. name) ignored

ignored :: HashSet String
ignored =
  HashSet.fromList
    [ -- TODO allow lambda branches of different number of patterns
      "Test027: Church numerals"
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
        $(mkRelFile "out/test050.out"),
      posTest
        "Test051: Local recursive function using IO >>>"
        $(mkRelDir ".")
        $(mkRelFile "test051.juvix")
        $(mkRelFile "out/test051.out"),
      posTest
        "Test052: Simple lambda calculus"
        $(mkRelDir ".")
        $(mkRelFile "test052.juvix")
        $(mkRelFile "out/test052.out"),
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
        "Test055: Constructor printing"
        $(mkRelDir ".")
        $(mkRelFile "test055.juvix")
        $(mkRelFile "out/test055.out"),
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
        "Test061: Traits"
        $(mkRelDir ".")
        $(mkRelFile "test061.juvix")
        $(mkRelFile "out/test061.out"),
      posTest
        "Test062: Overapplication"
        $(mkRelDir ".")
        $(mkRelFile "test062.juvix")
        $(mkRelFile "out/test062.out"),
      posTest
        "Test063: Coercions"
        $(mkRelDir ".")
        $(mkRelFile "test063.juvix")
        $(mkRelFile "out/test063.out"),
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
      posTestEval
        "Test074: Fields"
        $(mkRelDir ".")
        $(mkRelFile "test074.juvix")
        $(mkRelFile "out/test074.out"),
      posTestEval
        "Test075: Multiway If"
        $(mkRelDir ".")
        $(mkRelFile "test075.juvix")
        $(mkRelFile "out/test075.out"),
      posTest
        "Test076: Builtin Maybe"
        $(mkRelDir ".")
        $(mkRelFile "test076.juvix")
        $(mkRelFile "out/test076.out"),
      posTest
        "Test077: Instance fields"
        $(mkRelDir ".")
        $(mkRelFile "test077.juvix")
        $(mkRelFile "out/test077.out"),
      posTestEval
        "Test078: Builtin Byte"
        $(mkRelDir ".")
        $(mkRelFile "test078.juvix")
        $(mkRelFile "out/test078.out"),
      posTest
        "Test079: Let / LetRec type inference (during lambda lifting) in Core"
        $(mkRelDir ".")
        $(mkRelFile "test079.juvix")
        $(mkRelFile "out/test079.out"),
      posTest
        "Test080: Do notation"
        $(mkRelDir ".")
        $(mkRelFile "test080.juvix")
        $(mkRelFile "out/test080.out"),
      posTest
        "Test081: Non-duplication in let-folding"
        $(mkRelDir ".")
        $(mkRelFile "test081.juvix")
        $(mkRelFile "out/test081.out"),
      posTest
        "Test082: Pattern matching with side conditions"
        $(mkRelDir ".")
        $(mkRelFile "test082.juvix")
        $(mkRelFile "out/test082.out"),
      posTest
        "Test083: Record update"
        $(mkRelDir ".")
        $(mkRelFile "test083.juvix")
        $(mkRelFile "out/test083.out"),
      posTest
        "Test084: issue3030"
        $(mkRelDir ".")
        $(mkRelFile "test084.juvix")
        $(mkRelFile "out/test084.out"),
      posTest
        "Test085: Deriving Eq"
        $(mkRelDir ".")
        $(mkRelFile "test085.juvix")
        $(mkRelFile "out/test085.out"),
      posTest
        "Test086: Patterns in definitions"
        $(mkRelDir ".")
        $(mkRelFile "test086.juvix")
        $(mkRelFile "out/test086.out"),
      posTest
        "Test087: Deriving Ord"
        $(mkRelDir ".")
        $(mkRelFile "test087.juvix")
        $(mkRelFile "out/test087.out"),
      posTest
        "Test088: Record update pun"
        $(mkRelDir ".")
        $(mkRelFile "test088.juvix")
        $(mkRelFile "out/test088.out"),
      posTest
        "Test089: Loop invariant code motion"
        $(mkRelDir ".")
        $(mkRelFile "test089.juvix")
        $(mkRelFile "out/test089.out"),
      posTest
        "Test090: Module self-reference"
        $(mkRelDir ".")
        $(mkRelFile "test090.juvix")
        $(mkRelFile "out/test090.out"),
      posTest
        "Test091: Instance termination"
        $(mkRelDir ".")
        $(mkRelFile "test091.juvix")
        $(mkRelFile "out/test091.out"),
      posTest
        "Test092: Type level naturals"
        $(mkRelDir ".")
        $(mkRelFile "test092.juvix")
        $(mkRelFile "out/test092.out"),
      posTest
        "Test093: Pattern matching with indexed types"
        $(mkRelDir ".")
        $(mkRelFile "test093.juvix")
        $(mkRelFile "out/test093.out")
    ]
