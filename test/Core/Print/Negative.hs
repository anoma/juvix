module Core.Print.Negative where

import Base
import Core.Print.Base

data NegTest = NegTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/Core/negative/parsing")

testDescr :: NegTest -> TestDescr
testDescr NegTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ coreParseErrorAssertion file'
        }

allTests :: TestTree
allTests =
  testGroup
    "JuvixCore parsing negative tests"
    (map (mkTest . testDescr) tests)

tests :: [NegTest]
tests =
  [ NegTest
      "Undeclared identifier"
      $(mkRelDir ".")
      $(mkRelFile "test001.jvc"),
    NegTest
      "Let not recursive"
      $(mkRelDir ".")
      $(mkRelFile "test002.jvc"),
    NegTest
      "Duplicate definition"
      $(mkRelDir ".")
      $(mkRelFile "test003.jvc"),
    NegTest
      "Duplicate identifier"
      $(mkRelDir ".")
      $(mkRelFile "test004.jvc"),
    NegTest
      "Duplicate constructor"
      $(mkRelDir ".")
      $(mkRelFile "test005.jvc"),
    NegTest
      "Type signature mismatch"
      $(mkRelDir ".")
      $(mkRelFile "test006.jvc"),
    NegTest
      "Signature after definition"
      $(mkRelDir ".")
      $(mkRelFile "test007.jvc"),
    NegTest
      "Too many lambdas"
      $(mkRelDir ".")
      $(mkRelFile "test008.jvc"),
    NegTest
      "Duplicate identifier (def vs type)"
      $(mkRelDir ".")
      $(mkRelFile "test009.jvc"),
    NegTest
      "Duplicate identifier (def vs constructor)"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvc"),
    NegTest
      "Duplicate identifier (def vs constructor)"
      $(mkRelDir ".")
      $(mkRelFile "test010.jvc"),
    NegTest
      "Unrecognized builtin"
      $(mkRelDir ".")
      $(mkRelFile "test011.jvc"),
    NegTest
      "Empty letrec"
      $(mkRelDir ".")
      $(mkRelFile "test012.jvc"),
    NegTest
      "Wrong definition order in letrec"
      $(mkRelDir ".")
      $(mkRelFile "test013.jvc"),
    NegTest
      "Wrong name in letrec signature"
      $(mkRelDir ".")
      $(mkRelFile "test014.jvc"),
    NegTest
      "Case: multiple default branches"
      $(mkRelDir ".")
      $(mkRelFile "test015.jvc"),
    NegTest
      "Case: default branch only"
      $(mkRelDir ".")
      $(mkRelFile "test016.jvc"),
    NegTest
      "Case: no branches"
      $(mkRelDir ".")
      $(mkRelFile "test017.jvc"),
    NegTest
      "Case: multiple default branches only"
      $(mkRelDir ".")
      $(mkRelFile "test018.jvc"),
    NegTest
      "Case: not a constructor"
      $(mkRelDir ".")
      $(mkRelFile "test019.jvc"),
    NegTest
      "Case: undeclared identifier"
      $(mkRelDir ".")
      $(mkRelFile "test020.jvc"),
    NegTest
      "Case: wrong number of constructor arguments"
      $(mkRelDir ".")
      $(mkRelFile "test021.jvc"),
    NegTest
      "Match: wrong number of constructor arguments"
      $(mkRelDir ".")
      $(mkRelFile "test022.jvc"),
    NegTest
      "Match: wrong number of patterns"
      $(mkRelDir ".")
      $(mkRelFile "test023.jvc"),
    NegTest
      "Match: wrong number of patterns with side-conditions"
      $(mkRelDir ".")
      $(mkRelFile "test024.jvc")
  ]
