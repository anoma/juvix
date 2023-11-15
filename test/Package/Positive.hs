module Package.Positive where

import Base
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Data.Effect.TaggedLock

type FailMsg = String

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _checkPackage :: Package -> BuildDir -> Maybe FailMsg
  }

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/positive/PackageLoader")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Single $ do
            withTempDir' $ \d -> do
              let buildDir = CustomBuildDir (Abs d)
              res <-
                runM
                  . runError @JuvixError
                  . runFilesIO
                  . mapError (JuvixError @PackageLoaderError)
                  . runTaggedLockPermissive
                  . runEvalFileEffIO
                  . readPackage tRoot
                  $ buildDir
              case res of
                Right p -> whenJust (_checkPackage p buildDir) assertFailure
                Left {} -> assertFailure "An error ocurred when reading the package."
        }

allTests :: TestTree
allTests =
  testGroup
    "Package loading positive tests"
    ( map (mkTest . testDescr) packageLoadingTests
    )

packageLoadingTests :: [PosTest]
packageLoadingTests =
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
            | otherwise -> Just "Expected dependencies to be empty",
    PosTest
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
