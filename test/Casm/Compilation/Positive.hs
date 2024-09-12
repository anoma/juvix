module Casm.Compilation.Positive where

import Base
import Casm.Compilation.Base
import Data.HashSet qualified as HashSet

data PosTest = PosTest
  { _interp :: Bool,
    _runVM :: Bool,
    _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _expectedFile :: Path Abs File,
    _inputFile :: Maybe (Path Abs File)
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Casm/Compilation/positive/")

toTestDescr :: Int -> PosTest -> TestDescr
toTestDescr optLevel PosTest {..} =
  let tRoot = _dir
      file' = _file
      expected' = _expectedFile
      input' = _inputFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ compileAssertion _dir _interp _runVM optLevel file' input' expected'
        }

mkAllTests :: String -> Int -> IO TestTree
mkAllTests title optimLevel = do
  let (vmTests, nonVmTests) = partition (^. runVM) tests
      vmGroup = testGroup "With VM" (mkTest . toTestDescr optimLevel <$> vmTests)
  vmTestTree <- withPrecondition cairoVmPrecondition (return vmGroup)
  let nonVmTestTree = testGroup "Without VM" (mkTest . toTestDescr optimLevel <$> nonVmTests)
  return $
    testGroup
      title
      [vmTestTree, nonVmTestTree]

allTests :: IO TestTree
allTests = mkAllTests "CASM run positive tests" 3

allTestsNoOptimize :: IO TestTree
allTestsNoOptimize = mkAllTests "Juvix to CASM positive tests (no optimization)" 0

posTest :: String -> Bool -> Bool -> Path Rel Dir -> Path Rel File -> Maybe (Path Rel File) -> Path Rel File -> PosTest
posTest _name _interp _runVM rdir rfile rinfile routfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _expectedFile = root <//> routfile
      _inputFile = fmap (root <//>) rinfile
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
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test001.juvix")
        Nothing
        $(mkRelFile "out/test001.out"),
      posTest
        "Test002: Arithmetic operators inside lambdas"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test002.juvix")
        Nothing
        $(mkRelFile "out/test002.out"),
      posTest
        "Test003: Integer arithmetic"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test003.juvix")
        Nothing
        $(mkRelFile "out/test003.out"),
      posTest
        "Test005: Higher-order functions"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test005.juvix")
        Nothing
        $(mkRelFile "out/test005.out"),
      posTest
        "Test006: If-then-else and lazy boolean operators"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test006.juvix")
        Nothing
        $(mkRelFile "out/test006.out"),
      posTest
        "Test007: Pattern matching and lambda-case"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test007.juvix")
        Nothing
        $(mkRelFile "out/test007.out"),
      posTest
        "Test008: Recursion"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test008.juvix")
        Nothing
        $(mkRelFile "out/test008.out"),
      posTest
        "Test009: Tail recursion"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test009.juvix")
        Nothing
        $(mkRelFile "out/test009.out"),
      posTest
        "Test010: Let"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test010.juvix")
        Nothing
        $(mkRelFile "out/test010.out"),
      posTest
        "Test013: Functions returning functions with variable capture"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test013.juvix")
        Nothing
        $(mkRelFile "out/test013.out"),
      posTest
        "Test014: Arithmetic"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test014.juvix")
        Nothing
        $(mkRelFile "out/test014.out"),
      posTest
        "Test015: Local functions with free variables"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test015.juvix")
        Nothing
        $(mkRelFile "out/test015.out"),
      posTest
        "Test016: Recursion through higher-order functions"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test016.juvix")
        Nothing
        $(mkRelFile "out/test016.out"),
      posTest
        "Test017: Tail recursion through higher-order functions"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test017.juvix")
        Nothing
        $(mkRelFile "out/test017.out"),
      posTest
        "Test018: Higher-order functions and recursion"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test018.juvix")
        Nothing
        $(mkRelFile "out/test018.out"),
      posTest
        "Test019: Self-application"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test019.juvix")
        Nothing
        $(mkRelFile "out/test019.out"),
      posTest
        "Test020: Recursive functions: McCarthy's 91 function, subtraction by increments"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test020.juvix")
        Nothing
        $(mkRelFile "out/test020.out"),
      posTest
        "Test021: Fast exponentiation"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test021.juvix")
        Nothing
        $(mkRelFile "out/test021.out"),
      posTest
        "Test022: Lists"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test022.juvix")
        Nothing
        $(mkRelFile "out/test022.out"),
      posTest
        "Test023: Mutual recursion"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test023.juvix")
        Nothing
        $(mkRelFile "out/test023.out"),
      posTest
        "Test024: Nested binders with variable capture"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test024.juvix")
        Nothing
        $(mkRelFile "out/test024.out"),
      posTest
        "Test025: Euclid's algorithm"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test025.juvix")
        Nothing
        $(mkRelFile "out/test025.out"),
      posTest
        "Test026: Functional queues"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test026.juvix")
        Nothing
        $(mkRelFile "out/test026.out"),
      posTest
        "Test028: Streams without memoization"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test028.juvix")
        Nothing
        $(mkRelFile "out/test028.out"),
      posTest
        "Test029: Ackermann function"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test029.juvix")
        Nothing
        $(mkRelFile "out/test029.out"),
      posTest
        "Test030: Ackermann function (higher-order definition)"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test030.juvix")
        Nothing
        $(mkRelFile "out/test030.out"),
      posTest
        "Test032: Merge sort"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test032.juvix")
        Nothing
        $(mkRelFile "out/test032.out"),
      posTest
        "Test033: Eta-expansion of builtins and constructors"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test033.juvix")
        Nothing
        $(mkRelFile "out/test033.out"),
      posTest
        "Test034: Recursive let"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test034.juvix")
        Nothing
        $(mkRelFile "out/test034.out"),
      posTest
        "Test035: Pattern matching"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test035.juvix")
        Nothing
        $(mkRelFile "out/test035.out"),
      posTest
        "Test036: Eta-expansion"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test036.juvix")
        Nothing
        $(mkRelFile "out/test036.out"),
      posTest
        "Test037: Applications with lets and cases in function position"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test037.juvix")
        Nothing
        $(mkRelFile "out/test037.out"),
      posTest
        "Test038: Simple case expression"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test038.juvix")
        Nothing
        $(mkRelFile "out/test038.out"),
      posTest
        "Test039: Mutually recursive let expression"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test039.juvix")
        Nothing
        $(mkRelFile "out/test039.out"),
      posTest
        "Test040: Pattern matching nullary constructor"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test040.juvix")
        Nothing
        $(mkRelFile "out/test040.out"),
      posTest
        "Test045: Implicit builtin bool"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test045.juvix")
        Nothing
        $(mkRelFile "out/test045.out"),
      posTest
        "Test046: Polymorphic type arguments"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test046.juvix")
        Nothing
        $(mkRelFile "out/test046.out"),
      posTest
        "Test047: Local Modules"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test047.juvix")
        Nothing
        $(mkRelFile "out/test047.out"),
      posTest
        "Test050: Pattern matching with integers"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test050.juvix")
        Nothing
        $(mkRelFile "out/test050.out"),
      posTest
        "Test053: Inlining"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test053.juvix")
        Nothing
        $(mkRelFile "out/test053.out"),
      posTest
        "Test054: Iterators"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test054.juvix")
        Nothing
        $(mkRelFile "out/test054.out"),
      posTest
        "Test056: Argument specialization"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test056.juvix")
        Nothing
        $(mkRelFile "out/test056.out"),
      posTest
        "Test057: Case folding"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test057.juvix")
        Nothing
        $(mkRelFile "out/test057.out"),
      posTest
        "Test058: Ranges"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test058.juvix")
        Nothing
        $(mkRelFile "out/test058.out"),
      posTest
        "Test059: Builtin list"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test059.juvix")
        Nothing
        $(mkRelFile "out/test059.out"),
      posTest
        "Test060: Record update"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test060.juvix")
        Nothing
        $(mkRelFile "out/test060.out"),
      posTest
        "Test062: Overapplication"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test062.juvix")
        Nothing
        $(mkRelFile "out/test062.out"),
      posTest
        "Test064: Constant folding"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test064.juvix")
        Nothing
        $(mkRelFile "out/test064.out"),
      posTest
        "Test065: Arithmetic simplification"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test065.juvix")
        Nothing
        $(mkRelFile "out/test065.out"),
      posTest
        "Test066: Import function with a function call in default argument"
        True
        True
        $(mkRelDir "test066")
        $(mkRelFile "M.juvix")
        Nothing
        $(mkRelFile "out/test066.out"),
      posTest
        "Test067: Dependent default values inserted during translation FromConcrete"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test067.juvix")
        Nothing
        $(mkRelFile "out/test067.out"),
      posTest
        "Test068: Dependent default values inserted in the arity checker"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test068.juvix")
        Nothing
        $(mkRelFile "out/test068.out"),
      posTest
        "Test069: Dependent default values for Ord trait"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test069.juvix")
        Nothing
        $(mkRelFile "out/test069.out"),
      posTest
        "Test070: Nested default values and named arguments"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test070.juvix")
        Nothing
        $(mkRelFile "out/test070.out"),
      posTest
        "Test071: Named application (Ord instance with default cmp)"
        True
        False
        $(mkRelDir ".")
        $(mkRelFile "test071.juvix")
        Nothing
        $(mkRelFile "out/test071.out"),
      posTest
        "Test072: Monad transformers (ReaderT + StateT + Identity)"
        True
        True
        $(mkRelDir "test072")
        $(mkRelFile "ReaderT.juvix")
        Nothing
        $(mkRelFile "out/test072.out"),
      posTest
        "Test073: Import and use a syntax alias"
        True
        True
        $(mkRelDir "test073")
        $(mkRelFile "test073.juvix")
        Nothing
        $(mkRelFile "out/test073.out"),
      posTest
        "Test074: Fields"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test074.juvix")
        Nothing
        $(mkRelFile "out/test074.out"),
      posTest
        "Test075: Poseidon hash"
        False
        True
        $(mkRelDir ".")
        $(mkRelFile "test075.juvix")
        Nothing
        $(mkRelFile "out/test075.out"),
      posTest
        "Test076: Elliptic Curve builtin"
        False
        True
        $(mkRelDir ".")
        $(mkRelFile "test076.juvix")
        Nothing
        $(mkRelFile "out/test076.out"),
      posTest
        "Test077: Input and output"
        False
        True
        $(mkRelDir ".")
        $(mkRelFile "test077.juvix")
        (Just $(mkRelFile "in/test077.json"))
        $(mkRelFile "out/test077.out"),
      posTest
        "Test078: Assertions"
        True
        True
        $(mkRelDir ".")
        $(mkRelFile "test078.juvix")
        Nothing
        $(mkRelFile "out/test078.out")
    ]
