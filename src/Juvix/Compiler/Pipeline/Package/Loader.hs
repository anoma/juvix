module Juvix.Compiler.Pipeline.Package.Loader
  ( module Juvix.Compiler.Pipeline.Package.Loader,
    module Juvix.Compiler.Pipeline.Package.Loader.Versions,
  )
where

import Data.FileEmbed qualified as FE
import Data.Versions
import Juvix.Compiler.Concrete.Gen
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromSource hiding (symbol)
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Language.Value
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Package.Loader.Versions
import Juvix.Extra.Paths
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Language.Haskell.TH.Syntax hiding (Module)
import System.FilePath qualified as FP

acceptableTypes :: forall r. (Member Files r) => Sem r [TypeSpec]
acceptableTypes = mapM go packageDescriptionTypes
  where
    go :: PackageDescriptionType -> Sem r TypeSpec
    go t = do
      globalPackageDir <- globalPackageDescriptionRoot
      return
        TypeSpec
          { _typeSpecName = t ^. packageDescriptionTypeName,
            _typeSpecFile = globalPackageDir <//> (t ^. packageDescriptionTypePath)
          }

-- | Load a package file in the context of the PackageDescription module and the global package stdlib.
loadPackage :: (Members '[Files, EvalFileEff, Error PackageLoaderError] r) => BuildDir -> Path Abs File -> Sem r Package
loadPackage buildDir packagePath = do
  scoped @(Path Abs File) @EvalEff packagePath $ do
    v <- getPackageNode >>= eval'
    toPackage buildDir packagePath v
  where
    -- Obtain the Node corresponding to the `package` identifier in the loaded
    -- Package
    --
    -- This function also checks that the type of the identifier is among the
    -- expected types from the specific PackageDescription modules that are
    -- provided by the PathResolver.
    getPackageNode :: forall r. (Members '[Files, EvalEff] r) => Sem r Core.Node
    getPackageNode = do
      n <- lookupIdentifier Str.package
      acceptableTypes >>= assertNodeType n
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
      ValueConstant (Core.ConstString s) -> return s
      _ -> err

    toInteger' :: Value -> Sem r Integer
    toInteger' = \case
      ValueConstant (Core.ConstInteger i) -> return i
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

toConcrete :: PackageDescriptionType -> Package -> Module 'Parsed 'ModuleTop
toConcrete t p = run . runReader l $ do
  packageSymbol <- symbol "Package"
  importModuleSymbols <- mapM symbol (pack . FP.dropExtension <$> FP.splitDirectories (toFilePath (t ^. packageDescriptionTypePath)))
  let _importModule :: TopModulePath = mkTopModulePath (fromJust (nonEmpty importModuleSymbols))
  _moduleBody :: [Statement 'Parsed] <- do
    stdlib <- maybeToList <$> stdlibImport
    body <- sequence [mkImport _importModule, funDef]
    return (stdlib <> body)
  _moduleKw <- kw kwModule
  let _modulePath = mkTopModulePath (packageSymbol :| [])
  return
    Module
      { _moduleKwEnd = (),
        _moduleInductive = (),
        _moduleDoc = Nothing,
        _modulePragmas = Nothing,
        ..
      }
  where
    funDef :: (Member (Reader Interval) r) => Sem r (Statement 'Parsed)
    funDef = do
      packageTypeIdentifier <- identifier (t ^. packageDescriptionTypeName)
      _signRetType <- Just <$> expressionAtoms' (packageTypeIdentifier :| [])
      _signName <- symbol Str.package
      _signColonKw <- Irrelevant . Just <$> kw kwColon
      let _signBody = (t ^. packageDescriptionTypeTransform) p
      return
        ( StatementFunctionDef
            FunctionDef
              { _signTerminating = Nothing,
                _signPragmas = Nothing,
                _signInstance = Nothing,
                _signDoc = Nothing,
                _signCoercion = Nothing,
                _signBuiltin = Nothing,
                _signArgs = [],
                ..
              }
        )

    stdlibImport :: (Member (Reader Interval) r) => Sem r (Maybe (Statement 'Parsed))
    stdlibImport
      | (t ^. packageDescriptionTypeNeedsStdlibImport) p = Just <$> mkStdlibImport
      | otherwise = return Nothing

    mkImport :: (Member (Reader Interval) r) => TopModulePath -> Sem r (Statement 'Parsed)
    mkImport _importModule = do
      _openModuleKw <- kw kwOpen
      _importKw <- kw kwImport
      return
        ( StatementImport
            Import
              { _importOpen =
                  Just
                    OpenModuleParams
                      { _openUsingHiding = Nothing,
                        _openPublicKw = Irrelevant Nothing,
                        _openPublic = NoPublic,
                        ..
                      },
                _importAsName = Nothing,
                ..
              }
        )

    mkStdlibImport :: (Member (Reader Interval) r) => Sem r (Statement 'Parsed)
    mkStdlibImport = do
      stdlibSymbol <- symbol "Stdlib"
      preludeSymbol <- symbol "Prelude"
      mkImport (mkTopModulePath (stdlibSymbol :| [preludeSymbol]))

    l :: Interval
    l = singletonInterval (mkInitialLoc (p ^. packageFile))

packageDescriptionDir' :: Path Abs Dir
packageDescriptionDir' =
  $( FE.makeRelativeToProject (toFilePath packageDescriptionDir)
       >>= runIO
         . parseAbsDir
       >>= lift
   )
