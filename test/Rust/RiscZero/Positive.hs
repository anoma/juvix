module Rust.RiscZero.Positive where

import Base
import Rust.Compilation.Positive qualified as C
import Rust.RiscZero.Base

toTestDescr :: IO (Path Abs Dir) -> Int -> C.PosTest -> TestDescr
toTestDescr tmpDir optLevel C.PosTest {..} =
  let tRoot = _dir
      file' = _file
      expected' = _expectedFile
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ compileAssertion tmpDir _dir optLevel file' expected'
        }

allTests :: TestTree
allTests =
  withResource
    createTempDir'
    removeDirRecur
    ( \tmpDir ->
        sequentialTestGroup
          "Juvix to RISC0 Rust positive tests"
          AllSucceed
          (map (mkTest . toTestDescr tmpDir 2) C.tests)
    )

allTestsNoOptimize :: TestTree
allTestsNoOptimize =
  withResource
    createTempDir'
    removeDirRecur
    ( \tmpDir ->
        sequentialTestGroup
          "Juvix to RISC0 Rust positive tests (no optimization)"
          AllSucceed
          (map (mkTest . toTestDescr tmpDir 0) C.tests)
    )
