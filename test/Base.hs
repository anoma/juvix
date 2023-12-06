module Base
  ( module Test.Tasty,
    module Test.Tasty.HUnit,
    module Juvix.Prelude,
    module Base,
    module Juvix.Extra.Paths,
    module Juvix.Prelude.Env,
    module Juvix.Compiler.Pipeline.Run,
    module Juvix.Compiler.Pipeline.EntryPoint.IO,
  )
where

import Control.Monad.Extra as Monad
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Juvix.Compiler.Concrete (HighlightInput)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination
import Juvix.Compiler.Pipeline.EntryPoint.IO
import Juvix.Compiler.Pipeline.Run
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Paths hiding (rootBuildDir)
import Juvix.Prelude hiding (assert)
import Juvix.Prelude.Env
import Test.Tasty
import Test.Tasty.HUnit

data AssertionDescr
  = Single Assertion
  | Steps ((String -> IO ()) -> Assertion)

data TestDescr = TestDescr
  { _testName :: String,
    _testRoot :: Path Abs Dir,
    -- | relative to root
    _testAssertion :: AssertionDescr
  }

newtype WASMInfo = WASMInfo
  { _wasmInfoActual :: Path Abs File -> IO Text
  }

makeLenses ''TestDescr

data StdlibMode
  = StdlibInclude
  | StdlibExclude
  deriving stock (Show, Eq)

data CompileMode
  = WASI StdlibMode
  | WASM WASMInfo

mkTest :: TestDescr -> TestTree
mkTest TestDescr {..} = case _testAssertion of
  Single assertion -> testCase _testName (withCurrentDir _testRoot assertion)
  Steps steps -> testCaseSteps _testName (withCurrentDir _testRoot . steps)

assertEqDiffText :: String -> Text -> Text -> Assertion
assertEqDiffText = assertEqDiff unpack

assertEqDiff :: (Eq a) => (a -> String) -> String -> a -> a -> Assertion
assertEqDiff show_ msg a b
  | a == b = return ()
  | otherwise = do
      putStrLn (pack $ ppDiff (getGroupedDiff pa pb))
      putStrLn "End diff"
      Monad.fail msg
  where
    pa = lines $ show_ a
    pb = lines $ show_ b

assertEqDiffShow :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqDiffShow = assertEqDiff show

assertCmdExists :: Path Rel File -> Assertion
assertCmdExists cmd =
  assertBool ("Command: " <> toFilePath cmd <> " is not present on $PATH")
    . isJust
    =<< findExecutable cmd

testTaggedLockedToIO :: Sem PipelineAppEffects a -> IO a
testTaggedLockedToIO =
  runFinal
    . resourceToIOFinal
    . embedToFinal @IO
    . runTaggedLock LockModeExclusive

testRunIO ::
  forall a.
  EntryPoint ->
  Sem (PipelineEff PipelineAppEffects) a ->
  IO (ResolverState, a)
testRunIO e = testTaggedLockedToIO . runIO defaultGenericOptions e

testDefaultEntryPointIO :: Path Abs Dir -> Path Abs File -> IO EntryPoint
testDefaultEntryPointIO cwd mainFile = testTaggedLockedToIO (defaultEntryPointIO cwd mainFile)

testDefaultEntryPointNoFileIO :: Path Abs Dir -> IO EntryPoint
testDefaultEntryPointNoFileIO cwd = testTaggedLockedToIO (defaultEntryPointNoFileIO cwd)

testRunIOEither ::
  EntryPoint ->
  Sem (PipelineEff PipelineAppEffects) a ->
  IO (HighlightInput, (Either JuvixError (ResolverState, a)))
testRunIOEither entry = testTaggedLockedToIO . runIOEither entry

testRunIOEitherTermination ::
  EntryPoint ->
  Sem (Termination ': PipelineEff PipelineAppEffects) a ->
  IO (Either JuvixError (ResolverState, a))
testRunIOEitherTermination entry =
  fmap snd
    . testRunIOEither entry
    . evalTermination iniTerminationState
