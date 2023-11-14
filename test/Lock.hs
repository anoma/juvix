module Lock where

import Base
-- import Juvix.Data.Effect.Lock

import Control.Concurrent
import Extra
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver (withLockfile)
import Juvix.Data.Effect.FileLock
import System.FileLock hiding (FileLock)

allTests :: Lock -> TestTree
allTests l = testGroup "Lock tests" (replicate 100 (lockTest'' l))

lockTest :: Lock -> TestTree
lockTest l = testCase "acquireAndReleaseLock" $ do
  let output = withLock l . (\msg -> putStrLn msg >> hFlush stdout)
  output "helloworld"

lockTest' :: Lock -> TestTree
lockTest' _ = testCase "acquireAndReleaseLock" $ do
  let output = withFileLock "/tmp/.lock" Exclusive . (\msg _ -> putStrLn msg >> hFlush stdout)
  output "helloworld"

runLock :: Sem '[FileLock, Files, TempFile, Embed IO, Resource, Final IO] a -> IO a
runLock =
  runFinal
    . resourceToIOFinal
    . embedToFinal @IO
    . runTempFileIO
    . runFilesIO
    . runFileLockIO

lockTest'' :: Lock -> TestTree
lockTest'' _ = testCase "acquireAndReleaseLock" $ runLock $ do
  let output = withFileLockDir $(mkAbsDir "/tmp/lalalala") . (\msg -> embed (putStrLn msg >> hFlush stdout))
  output "helloworld"
