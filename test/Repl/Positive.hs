module Repl.Positive where

import Base
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Extra.Value qualified as Core
import Juvix.Compiler.Core.Language.Value qualified as Core
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Pipeline.Repl
import Juvix.Compiler.Pipeline.Root
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Paths qualified as P
import Juvix.Extra.Stdlib
import Repl.Assertions
import Repl.Value

runTaggedLockIO' :: Sem '[Files, TaggedLock, Embed IO] a -> IO a
runTaggedLockIO' =
  runM
    . runTaggedLockPermissive
    . runFilesIO

loadPrelude :: Path Abs Dir -> IO (Artifacts, EntryPoint)
loadPrelude rootDir = runTaggedLockIO' $ do
  runReader rootDir writeStdlib
  pkg <- readPackageRootIO root
  let ep = defaultEntryPoint pkg root (rootDir <//> preludePath)
  artif <- embed (runReplPipelineIO ep)
  return (artif, ep)
  where
    root :: Root
    root =
      Root
        { _rootRootDir = rootDir,
          _rootPackageType = LocalPackage,
          _rootInvokeDir = rootDir,
          _rootBuildDir = DefaultBuildDir
        }

data TestCtx = TestCtx
  { _testCtxRootDir :: Path Abs Dir,
    _testCtxEntryPoint :: EntryPoint,
    _testCtxArtifacts :: Artifacts
  }

data PosTest = PosTest
  { _posTestName :: Text,
    _posTestInput :: Text,
    _posTestExpected :: Core.Value
  }

makeLenses ''TestCtx
makeLenses ''PosTest

mkPreludeTest :: IO TestCtx -> PosTest -> TestTree
mkPreludeTest getCtx p = testCase (unpack (p ^. posTestName)) (replTest (p ^. posTestInput) (p ^. posTestExpected) getCtx)

replSetup :: IO TestCtx
replSetup = do
  _testCtxRootDir <- do
    sysTemp <- getTempDir
    createTempDir sysTemp "repl"
  (_testCtxArtifacts, _testCtxEntryPoint) <- loadPrelude _testCtxRootDir
  return TestCtx {..}

replTeardown :: TestCtx -> IO ()
replTeardown = removeDirRecur . (^. testCtxRootDir)

replTest :: Text -> Core.Value -> IO TestCtx -> IO ()
replTest input' expectedNode getTestCtx = do
  ctx <- getTestCtx
  (artif, res) <- compileReplInputIO' ctx input'
  res' <- assertNoJuvixError res
  case res' of
    Nothing -> assertFailure "Compilation did not return a node"
    Just n -> do
      let ep = ctx ^. testCtxEntryPoint
      n' <- evalRepl artif ep n
      assertValueEqual expectedNode n'

allTests :: TestTree
allTests =
  testGroup
    "REPL positive tests"
    [ withResource
        replSetup
        replTeardown
        ( \getCtx ->
            testGroup
              "Loading Stdlib.Prelude"
              ( map
                  (mkPreludeTest getCtx)
                  [ PosTest "Arithmetic" "3 * (1 + 1)" (mkInteger 6),
                    PosTest "Logic And" "true && false" (mkBool False),
                    PosTest "Let" "let x : Nat := 2 + 1 in x" (mkInteger 3),
                    PosTest "Literal comparison" "1 == 1" (mkBool True),
                    PosTest "List literal in call" "head 0 [1;2;3]" (mkInteger 1)
                  ]
              )
        )
    ]

compileReplInputIO' :: TestCtx -> Text -> IO (Artifacts, (Either JuvixError (Maybe Core.Node)))
compileReplInputIO' ctx txt =
  runM
    . runState (ctx ^. testCtxArtifacts)
    . runReader (ctx ^. testCtxEntryPoint)
    $ do
      r <- compileReplInputIO P.replPath txt
      return (extractNode <$> r)
  where
    extractNode :: ReplPipelineResult -> Maybe Core.Node
    extractNode = \case
      ReplPipelineResultNode n -> Just n
      ReplPipelineResultImport {} -> Nothing
      ReplPipelineResultOpen {} -> Nothing

evalRepl :: Artifacts -> EntryPoint -> Core.Node -> IO Core.Value
evalRepl artif ep n = do
  (artif', n') <-
    assertNoJuvixError
      . run
      . runReader ep
      . runError @JuvixError
      . runState artif
      . runTransformations True toStoredTransformations
      $ n
  doEvalIO' artif' n' >>= assertNoJuvixError
  where
    doEvalIO' :: Artifacts -> Core.Node -> IO (Either JuvixError Core.Value)
    doEvalIO' artif' n' =
      mapRight (Core.toValue tab)
        . mapLeft (JuvixError @Core.CoreError)
        <$> (Core.doEvalIO Nothing False replDefaultLoc tab n')
      where
        tab :: Core.InfoTable
        tab = Core.computeCombinedInfoTable $ artif' ^. artifactCoreModule

    replDefaultLoc :: Interval
    replDefaultLoc = singletonInterval (mkInitialLoc P.replPath)
