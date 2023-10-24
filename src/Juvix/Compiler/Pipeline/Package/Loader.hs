module Juvix.Compiler.Pipeline.Package.Loader where

import Data.FileEmbed qualified as FE
import Data.Versions
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Value
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Language.Haskell.TH.Syntax

-- | The name of the Package type name in the PackageDescription module
packageTypeName :: Text
packageTypeName = "Package"

-- | Load a package file in the context of the PackageDescription module and the global package stdlib.
loadPackage :: (Members '[Files, EvalFileEff, Error PackageLoaderError] r) => BuildDir -> Path Abs File -> Sem r Package
loadPackage buildDir packagePath = do
  globalPackageDir <- globalPackageDescriptionRoot
  let packageDescriptionPath' = globalPackageDir <//> filename packageDescriptionPath

  scoped @(Path Abs File) @EvalEff packagePath $ do
    v <- getPackageNode packageDescriptionPath' >>= eval'
    toPackage buildDir packagePath v
  where
    -- Obtain the Node corresponding to the `package` identifier in the loaded
    -- Package
    --
    -- This function also checks that the type of the identifier is the expected
    -- type from the specific PackageDescription module that is provided by the
    -- PathResolver.
    getPackageNode :: forall r. (Members '[EvalEff] r) => Path Abs File -> Sem r Node
    getPackageNode packageDescriptionPath' = do
      n <- lookupIdentifier Str.package
      assertNodeType
        n
        TypeSpec
          { _typeSpecFile = packageDescriptionPath',
            _typeSpecName = packageTypeName
          }
      return n

toPackage ::
  forall r.
  (Member (Error PackageLoaderError) r) =>
  BuildDir ->
  Path Abs File ->
  Value ->
  Sem r Package
toPackage buildDir packagePath = \case
  ValueConstrApp ctor -> do
    case ctor ^. constrAppArgs of
      [vName, vVersion, vDeps, vMain, vBuildDir] -> do
        _packageName <- toText vName
        _packageMain <- toMaybeMain vMain
        _packageBuildDir <- toMaybeBuildDir vBuildDir
        _packageDependencies <- toList' toDependency vDeps
        _packageVersion <- toVersion vVersion
        return Package {_packageLockfile = Nothing, _packageFile = packagePath, ..}
      _ -> err
  _ -> err
  where
    err :: Sem r a
    err =
      throw
        PackageLoaderError
          { _packageLoaderErrorPath = packagePath,
            _packageLoaderErrorCause = ErrPackageTypeError
          }

    toMaybe :: (Value -> Sem r a) -> Value -> Sem r (Maybe a)
    toMaybe f = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [] -> return Nothing
        [v] -> Just <$> f v
        _ -> err
      _ -> err

    toList' :: (Value -> Sem r a) -> Value -> Sem r [a]
    toList' f = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [] -> return []
        [x, xs] -> do
          v <- f x
          vs <- toList' f xs
          return (v : vs)
        _ -> err
      _ -> err

    toText :: Value -> Sem r Text
    toText = \case
      ValueConstant (ConstString s) -> return s
      _ -> err

    toInteger' :: Value -> Sem r Integer
    toInteger' = \case
      ValueConstant (ConstInteger i) -> return i
      _ -> err

    toWord :: Value -> Sem r Word
    toWord = fmap fromInteger . toInteger'

    toMaybeMain :: Value -> Sem r (Maybe (Prepath File))
    toMaybeMain = toMaybe (fmap (mkPrepath . unpack) . toText)

    toMaybeBuildDir :: Value -> Sem r (Maybe (SomeBase Dir))
    toMaybeBuildDir = toMaybe go
      where
        go :: Value -> Sem r (SomeBase Dir)
        go v = do
          s <- unpack <$> toText v
          let p :: Maybe (SomeBase Dir)
              p = (Abs <$> parseAbsDir s) <|> (Rel <$> parseRelDir s)
          maybe err return p

    toVersion :: Value -> Sem r SemVer
    toVersion = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [vMaj, vMin, vPatch, _, vMeta] -> do
          maj <- toWord vMaj
          min' <- toWord vMin
          patch' <- toWord vPatch
          meta' <- toMaybe toText vMeta
          return (SemVer maj min' patch' Nothing meta')
        _ -> err
      _ -> err

    toDependency :: Value -> Sem r Dependency
    toDependency = \case
      ValueConstrApp c -> case c ^. constrAppArgs of
        [] -> return defaultStdlib
        [v] -> do
          p <- mkPrepath . unpack <$> toText v
          return (DependencyPath (PathDependency {_pathDependencyPath = p}))
        [vName, vUrl, vRef] -> do
          _gitDependencyUrl <- toText vUrl
          _gitDependencyName <- toText vName
          _gitDependencyRef <- toText vRef
          return (DependencyGit (GitDependency {..}))
        _ -> err
      _ -> err

    defaultStdlib :: Dependency
    defaultStdlib = mkPathDependency (fromSomeDir p)
      where
        p :: SomeBase Dir
        p = resolveBuildDir buildDir <///> relStdlibDir

packageDescriptionPath :: Path Abs File
packageDescriptionPath =
  $( FE.makeRelativeToProject (toFilePath packageDescriptionFile)
       >>= runIO . parseAbsFile
       >>= lift
   )
