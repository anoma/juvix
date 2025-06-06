module Formatter.Positive where

import Base
import Juvix.Formatter
import Scope.Positive qualified
import Scope.Positive qualified as Scope

runScopeEffIO :: (Member EmbedIO r) => Path Abs Dir -> Sem (ScopeEff ': r) a -> Sem r a
runScopeEffIO root = interpret $ \case
  ScopeFile p -> do
    entry <- testDefaultEntryPointIO root p
    ((^. pipelineResult) . snd <$> testRunIO entry upToScopingEntry)
  ScopeStdin entry -> do
    ((^. pipelineResult) . snd <$> testRunIO entry upToScopingEntry)

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
                . runScopeEffIO tRoot
                . runFilesIO
                $ format file'
            case d of
              Right (_, FormatResultOK) -> return ()
              Right (_, FormatResultNotFormatted) -> assertFailure ("File: " <> show file' <> " is not formatted")
              Left {} -> assertFailure ("Error: ")
        }

filterOutTests :: [String] -> [Scope.PosTest] -> [Scope.PosTest]
filterOutTests out = filter (\Scope.PosTest {..} -> _name `notElem` out)

-- Ignore tests that use the stdlib or contain CRs or TABs
ignoredTests :: [String]
ignoredTests = ["Import embedded standard library", "Basic dependencies", "Whitespace", "Tabs"]

allTests :: TestTree
allTests =
  testGroup
    "Formatter positive tests"
    (map (mkTest . makeFormatTest') (filterOutTests ignoredTests Scope.Positive.tests))
