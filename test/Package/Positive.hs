module Package.Positive where

import Base
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO

type FailMsg = String

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _checkPackage :: Package -> BuildDir -> Maybe FailMsg
  }

v1Root :: Path Abs Dir
v1Root = relToProject $(mkRelDir "tests/positive/PackageLoader")

v2Root :: Path Abs Dir
v2Root = relToProject $(mkRelDir "tests/positive/PackageLoaderV2")

testDescr :: Path Abs Dir -> PosTest -> TestDescr
testDescr root PosTest {..} =
  let tRoot = root <//> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            entry <- testDefaultEntryPointNoFileIO tRoot
            withTempDir' $ \d -> do
              let buildDir = CustomBuildDir (Abs d)
              res <-
                runM
                  . runReader entry
                  . runError @JuvixError
                  . runFilesIO
                  . mapError (JuvixError @PackageLoaderError)
                  . runTaggedLock LockModeExclusive
                  . runEvalFileEffIO
                  . readPackage tRoot
                  $ buildDir
              case res of
                Right p -> whenJust (_checkPackage p buildDir) assertFailure
                Left {} -> assertFailure "An error occurred when reading the package."
        }

allTests :: TestTree
allTests =
  testGroup
    "Package loading positive tests"
    ( map (mkTest . testDescr v1Root) yamlTests
        <> map (mkTest . testDescr v1Root) packageLoadingTests
        <> map (mkTest . testDescr v2Root) packageLoadingTests
    )

yamlTests :: [PosTest]
yamlTests =
  [ PosTest
      "empty YAML is valid"
      $(mkRelDir "YamlEmpty")
      $ \p _ ->
        if
            | p ^. packageName == defaultPackageName -> Nothing
            | otherwise -> Just "Package did not have default name",
    PosTest
      "no dependencies uses default stdlib"
      $(mkRelDir "YamlNoDependencies")
      $ \p b -> case p ^? packageDependencies . _head of
        Just d ->
          if
              | d == defaultStdlibDep b -> Nothing
              | otherwise -> Just "Package dependency is not the default standard library"
        _ -> Just "The package has no dependencies",
    PosTest
      "empty dependencies does not use default stdlib"
      $(mkRelDir "YamlEmptyDependencies")
      $ \p _ ->
        if
            | null (p ^. packageDependencies) -> Nothing
            | otherwise -> Just "Expected dependencies to be empty"
  ]

packageLoadingTests :: [PosTest]
packageLoadingTests =
  [ PosTest
      "Package.juvix is read"
      $(mkRelDir "PackageJuvix")
      $ \p _ ->
        if
            | p ^. packageName == "package-juvix" -> Nothing
            | otherwise -> Just "Expected package name to be package-juvix",
    PosTest
      "Package.juvix is read in priority over yaml"
      $(mkRelDir "PackageJuvixAndYaml")
      $ \p _ ->
        if
            | p ^. packageName == "package-juvix" -> Nothing
            | otherwise -> Just "Expected package name to be package-juvix",
    PosTest
      "Package.juvix uses lock file"
      $(mkRelDir "PackageJuvixUsesLockfile")
      $ \p _ ->
        if
            | isJust (p ^. packageLockfile) -> Nothing
            | otherwise -> Just "No lock file was read",
    PosTest
      "Package.juvix unspecified dependencies uses default stdlib"
      $(mkRelDir "PackageJuvixNoDependencies")
      $ \p b -> case p ^? packageDependencies . _head of
        Just d ->
          if
              | d == defaultStdlibDep b -> Nothing
              | otherwise -> Just ("Package dependency is not the default standard library: " <> show (d, defaultStdlibDep b))
        _ -> Just "The package has no dependencies",
    PosTest
      "empty dependencies does not use default stdlib"
      $(mkRelDir "PackageJuvixEmptyDependencies")
      $ \p _ ->
        if
            | null (p ^. packageDependencies) -> Nothing
            | otherwise -> Just "Expected dependencies to be empty",
    PosTest
      "Package.juvix can be defined with PackageDescription.Basic"
      $(mkRelDir "PackageJuvixBasic")
      $ \p _ ->
        if
            | p ^. packageName == defaultPackageName -> Nothing
            | otherwise -> Just "Package did not have default name"
  ]
