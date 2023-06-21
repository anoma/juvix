module Core.Normalize.Positive where

import Base
import Core.Normalize.Base

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File,
    _dataFile :: Path Rel File
  }

makeLenses ''PosTest

filterOutTests :: [String] -> [PosTest] -> [PosTest]
filterOutTests out = filter (\PosTest {..} -> _name `notElem` out)

fromTest :: PosTest -> TestTree
fromTest = mkTest . toTestDescr

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/VampIR/positive")

toTestDescr' ::
  ( Path Abs File ->
    Path Abs File ->
    (String -> IO ()) ->
    Assertion
  ) ->
  PosTest ->
  TestDescr
toTestDescr' assertion PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
      expected' = tRoot <//> _dataFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ assertion file' expected'
        }

toTestDescr :: PosTest -> TestDescr
toTestDescr = toTestDescr' coreNormalizeAssertion

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore normalize positive tests"
    (map (mkTest . toTestDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest
      "Test001: not function"
      $(mkRelDir "Core")
      $(mkRelFile "test001.jvc")
      $(mkRelFile "data/test001.json"),
    PosTest
      "Test002: pattern matching"
      $(mkRelDir "Core")
      $(mkRelFile "test002.jvc")
      $(mkRelFile "data/test002.json"),
    PosTest
      "Test003: inductive types"
      $(mkRelDir "Core")
      $(mkRelFile "test003.jvc")
      $(mkRelFile "data/test003.json"),
    PosTest
      "Test004: definitions"
      $(mkRelDir "Core")
      $(mkRelFile "test004.jvc")
      $(mkRelFile "data/test004.json"),
    PosTest
      "Test005: basic arithmetic"
      $(mkRelDir "Core")
      $(mkRelFile "test005.jvc")
      $(mkRelFile "data/test005.json"),
    PosTest
      "Test006: arithmetic"
      $(mkRelDir "Core")
      $(mkRelFile "test006.jvc")
      $(mkRelFile "data/test006.json"),
    PosTest
      "Test007: single-constructor inductive types"
      $(mkRelDir "Core")
      $(mkRelFile "test007.jvc")
      $(mkRelFile "data/test007.json"),
    PosTest
      "Test008: higher-order inductive types"
      $(mkRelDir "Core")
      $(mkRelFile "test008.jvc")
      $(mkRelFile "data/test008.json"),
    PosTest
      "Test009: comparisons"
      $(mkRelDir "Core")
      $(mkRelFile "test009.jvc")
      $(mkRelFile "data/test009.json"),
    PosTest
      "Test010: let"
      $(mkRelDir "Core")
      $(mkRelFile "test010.jvc")
      $(mkRelFile "data/test010.json"),
    PosTest
      "Test011: functions returning functions with variable capture"
      $(mkRelDir "Core")
      $(mkRelFile "test011.jvc")
      $(mkRelFile "data/test011.json"),
    PosTest
      "Test012: partial application"
      $(mkRelDir "Core")
      $(mkRelFile "test012.jvc")
      $(mkRelFile "data/test012.json"),
    PosTest
      "Test013: mid-square hashing (unrolled)"
      $(mkRelDir "Core")
      $(mkRelFile "test013.jvc")
      $(mkRelFile "data/test013.json"),
    PosTest
      "Test014: recursion"
      $(mkRelDir "Core")
      $(mkRelFile "test014.jvc")
      $(mkRelFile "data/test014.json"),
    PosTest
      "Test015: tail recursion"
      $(mkRelDir "Core")
      $(mkRelFile "test015.jvc")
      $(mkRelFile "data/test015.json"),
    PosTest
      "Test016: tail recursion: Fibonacci numbers in linear time"
      $(mkRelDir "Core")
      $(mkRelFile "test016.jvc")
      $(mkRelFile "data/test016.json"),
    PosTest
      "Test017: recursion through higher-order functions"
      $(mkRelDir "Core")
      $(mkRelFile "test017.jvc")
      $(mkRelFile "data/test017.json"),
    PosTest
      "Test018: tail recursion through higher-order functions"
      $(mkRelDir "Core")
      $(mkRelFile "test018.jvc")
      $(mkRelFile "data/test018.json"),
    PosTest
      "Test019: higher-order functions"
      $(mkRelDir "Core")
      $(mkRelFile "test019.jvc")
      $(mkRelFile "data/test019.json"),
    PosTest
      "Test020: functional queues"
      $(mkRelDir "Core")
      $(mkRelFile "test020.jvc")
      $(mkRelFile "data/test020.json"),
    PosTest
      "Test021: polymorphism"
      $(mkRelDir "Core")
      $(mkRelFile "test021.jvc")
      $(mkRelFile "data/test021.json"),
    PosTest
      "Test022: mutual recursion"
      $(mkRelDir "Core")
      $(mkRelFile "test022.jvc")
      $(mkRelFile "data/test022.json"),
    PosTest
      "Test023: eta-expansion"
      $(mkRelDir "Core")
      $(mkRelFile "test023.jvc")
      $(mkRelFile "data/test023.json"),
    PosTest
      "Test024: eta-expansion of polymorphic constructors"
      $(mkRelDir "Core")
      $(mkRelFile "test024.jvc")
      $(mkRelFile "data/test024.json"),
    PosTest
      "Test025: mid-square hashing"
      $(mkRelDir "Core")
      $(mkRelFile "test025.jvc")
      $(mkRelFile "data/test025.json"),
    PosTest
      "Test026: letrec"
      $(mkRelDir "Core")
      $(mkRelFile "test026.jvc")
      $(mkRelFile "data/test026.json"),
    PosTest
      "Test027: type synonyms"
      $(mkRelDir "Core")
      $(mkRelFile "test027.jvc")
      $(mkRelFile "data/test027.json"),
    PosTest
      "Test028: let hoisting"
      $(mkRelDir "Core")
      $(mkRelFile "test028.jvc")
      $(mkRelFile "data/test028.json"),
    PosTest
      "Test029: let hoisting"
      $(mkRelDir "Core")
      $(mkRelFile "test029.jvc")
      $(mkRelFile "data/test029.json")
  ]
