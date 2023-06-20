module VampIR.Compilation.Positive where

import Base
import VampIR.Compilation.Base

data PosTest = PosTest
  { _paramsNum :: Int,
    _name :: String,
    _dir :: Path Abs Dir,
    _file :: Path Abs File,
    _dataFile :: Path Abs File
  }

makeLenses ''PosTest

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VampIR/positive/Compilation")

toTestDescr :: PosTest -> TestDescr
toTestDescr PosTest {..} =
  let tRoot = _dir
      file' = _file
      data' = _dataFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ vampirCompileAssertion _paramsNum file' data'
        }

allTests :: TestTree
allTests =
  testGroup
    "Juvix to VampIR compilation positive tests"
    (map (mkTest . toTestDescr) tests)

posTest :: Int -> String -> Path Rel Dir -> Path Rel File -> Path Rel File -> PosTest
posTest _paramsNum _name rdir rfile routfile =
  let _dir = root <//> rdir
      _file = _dir <//> rfile
      _dataFile = root <//> routfile
   in PosTest {..}

tests :: [PosTest]
tests =
  [ posTest
      5
      "Test001: not function"
      $(mkRelDir ".")
      $(mkRelFile "test001.juvix")
      $(mkRelFile "data/test001.json"),
    posTest
      5
      "Test002: pattern matching"
      $(mkRelDir ".")
      $(mkRelFile "test002.juvix")
      $(mkRelFile "data/test002.json"),
    posTest
      6
      "Test003: inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test003.juvix")
      $(mkRelFile "data/test003.json"),
    posTest
      11
      "Test004: arithmetic"
      $(mkRelDir ".")
      $(mkRelFile "test004.juvix")
      $(mkRelFile "data/test004.json"),
    posTest
      5
      "Test005: single-constructor inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test005.juvix")
      $(mkRelFile "data/test005.json"),
    posTest
      5
      "Test006: higher-order inductive types"
      $(mkRelDir ".")
      $(mkRelFile "test006.juvix")
      $(mkRelFile "data/test006.json"),
    posTest
      5
      "Test007: let"
      $(mkRelDir ".")
      $(mkRelFile "test007.juvix")
      $(mkRelFile "data/test007.json"),
    posTest
      8
      "Test008: functions returning functions with variable capture"
      $(mkRelDir ".")
      $(mkRelFile "test008.juvix")
      $(mkRelFile "data/test008.json"),
    posTest
      6
      "Test009: applications with lets and cases in function position"
      $(mkRelDir ".")
      $(mkRelFile "test009.juvix")
      $(mkRelFile "data/test009.json"),
    posTest
      14
      "Test010: mid-square hashing (unrolled)"
      $(mkRelDir ".")
      $(mkRelFile "test010.juvix")
      $(mkRelFile "data/test010.json"),
    posTest
      8
      "Test011: recursion"
      $(mkRelDir ".")
      $(mkRelFile "test011.juvix")
      $(mkRelFile "data/test011.json"),
    posTest
      8
      "Test012: tail recursion"
      $(mkRelDir ".")
      $(mkRelFile "test012.juvix")
      $(mkRelFile "data/test012.json"),
    posTest
      8
      "Test013: tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir ".")
      $(mkRelFile "test013.juvix")
      $(mkRelFile "data/test013.json"),
    posTest
      8
      "Test014: recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test014.juvix")
      $(mkRelFile "data/test014.json"),
    posTest
      8
      "Test015: tail recursion through higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test015.juvix")
      $(mkRelFile "data/test015.json"),
    posTest
      5
      "Test016: higher-order functions"
      $(mkRelDir ".")
      $(mkRelFile "test016.juvix")
      $(mkRelFile "data/test016.json"),
    posTest
      9
      "Test017: mutual recursion"
      $(mkRelDir ".")
      $(mkRelFile "test017.juvix")
      $(mkRelFile "data/test017.json"),
    posTest
      14
      "Test018: mid-square hashing"
      $(mkRelDir ".")
      $(mkRelFile "test018.juvix")
      $(mkRelFile "data/test018.json"),
    posTest
      3
      "Test019: polymorphism"
      $(mkRelDir ".")
      $(mkRelFile "test019.juvix")
      $(mkRelFile "data/test019.json"),
    posTest
      8
      "Test020: boolean target"
      $(mkRelDir ".")
      $(mkRelFile "test020.juvix")
      $(mkRelFile "data/test020.json"),
    posTest
      15
      "Test021: fast exponentiation (exponential unrolling)"
      $(mkRelDir ".")
      $(mkRelFile "test021.juvix")
      $(mkRelFile "data/test021.json"),
    posTest
      14
      "Test022: fast exponentiation"
      $(mkRelDir ".")
      $(mkRelFile "test022.juvix")
      $(mkRelFile "data/test022.json"),
    posTest
      5
      "Test023: permutative conversions"
      $(mkRelDir ".")
      $(mkRelFile "test023.juvix")
      $(mkRelFile "data/test023.json")
  ]
