module Repl.Positive where

import Base
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Stdlib
import Juvix.Compiler.Pipeline.Root
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Pipeline.Repl
import Juvix.Extra.Paths qualified as P

runTaggedLockIO' :: Sem '[TaggedLock, Files, Embed IO, Resource, Final IO] a -> IO a
runTaggedLockIO' = runFinal
       . resourceToIOFinal
       . embedToFinal @IO
       . runFilesIO
       . runTaggedLock LockModePermissive

loadPrelude :: Path Abs Dir -> IO (Artifacts, EntryPoint)
loadPrelude rootDir = runTaggedLockIO' $ do
  runReader rootDir writeStdlib
  pkg <- readPackageRootIO root
  let ep = defaultEntryPoint pkg root (rootDir <//> preludePath)
  artif <- embed (runReplPipelineIO ep)
  return (artif, ep)

  where
    root :: Root
    root = Root {_rootRootDir=rootDir,
                 _rootPackageType=LocalPackage,
                 _rootInvokeDir=rootDir,
                 _rootBuildDir=DefaultBuildDir}

data TestCtx = TestCtx {
  _testCtxRootDir :: Path Abs Dir,
  _testCtxEntryPoint :: EntryPoint,
  _testCtxArtifacts :: Artifacts
                       }

makeLenses ''TestCtx

assertNodeEqual :: Core.Node -> Core.Node -> Assertion
assertNodeEqual n1 n2 = undefined

replSetup :: IO TestCtx
replSetup = do
  _testCtxRootDir <- do
    sysTemp <- getTempDir
    createTempDir sysTemp "repl"
  (_testCtxArtifacts, _testCtxEntryPoint) <- loadPrelude _testCtxRootDir
  return TestCtx {..}

replTeardown :: TestCtx -> IO ()
replTeardown = removeDirRecur . (^. testCtxRootDir)

replTest :: IO TestCtx -> IO ()
replTest getTestCtx = do
  ctx <- getTestCtx
  (_, res) <- compileReplInputIO' ctx "1 + 1"
  case res of
    Left err -> assertFailure "err"
    Right Nothing -> assertFailure "nothing"
    Right n -> assertBool "expected equal" (n  == (Just (Core.mkConstant' (Core.ConstInteger 2))))


allTests :: TestTree
allTests = withResource
  replSetup
  replTeardown
  (\getTestCtx -> testGroup "REPL positive tests"
     (map (\f -> f getTestCtx ) [testCase "repl test" . replTest]))

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
