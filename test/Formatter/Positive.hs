module Formatter.Positive where

import Base
import Juvix.Formatter
import Scope.Positive qualified
import Scope.Positive qualified as Scope

runScopeEffIO :: (Member (Embed IO) r) => Sem (ScopeEff ': r) a -> Sem r a
runScopeEffIO = interpret $ \case
  ScopeFile p -> do
    entry <- embed (defaultEntryPointCwdIO p)
    embed (snd <$> runIO' entry upToScoping)
  ScopeStdin -> do
    entry <- embed defaultEntryPointNoFileCwdIO
    embed (snd <$> runIO' entry upToScoping)

makeFormatTest' :: Scope.PosTest -> TestDescr
makeFormatTest' Scope.PosTest {..} =
  let tRoot = Scope.Positive.root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            d <-
              runM
                . runError
                . runOutputList @FormattedFileInfo
                . runScopeEffIO
                . runFilesIO
                $ format file'
            case d of
              Right (_, FormatResultOK) -> return ()
              Right (_, FormatResultNotFormatted) -> assertFailure ("File: " <> show file' <> " is not formatted")
              Right (_, FormatResultFail) -> assertFailure ("File: " <> show file' <> " is failed to format")
              Left {} -> assertFailure ("Error: ")
        }

filterOutTests :: [String] -> [Scope.PosTest] -> [Scope.PosTest]
filterOutTests out = filter (\Scope.PosTest {..} -> _name `notElem` out)

-- Ignore tests that use the stdlib
ignoredTests :: [String]
ignoredTests = ["Import embedded standard library", "Basic dependencies"]

allTests :: TestTree
allTests =
  testGroup
    "Formatter positive tests"
    (map (mkTest . makeFormatTest') (filterOutTests ignoredTests Scope.Positive.tests))
