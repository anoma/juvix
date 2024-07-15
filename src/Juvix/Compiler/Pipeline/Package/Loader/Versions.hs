module Juvix.Compiler.Pipeline.Package.Loader.Versions where

import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Versions
import Juvix.Compiler.Concrete.Gen
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Language.Value
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Extra.Paths
import Juvix.Prelude

currentPackageVersion :: PackageVersion
currentPackageVersion = PackageVersion2

data PackageVersion
  = PackageVersion1
  | PackageVersion2
  | PackageBasic
  deriving stock (Bounded, Enum)

data PackageDescriptionType = PackageDescriptionType
  { _packageDescriptionTypePath :: Path Rel File,
    _packageDescriptionTypeName :: Text,
    _packageDescriptionTypeTransform :: Package -> FunctionDefBody 'Parsed,
    _packageDescriptionTypeNeedsStdlibImport :: Package -> Bool,
    _packageDescriptionTypeVersion :: PackageVersion,
    _packageDescriptionTypeToPackage :: forall r. (Member (Error PackageLoaderError) r) => BuildDir -> Path Abs File -> Value -> Sem r Package
  }

makeLenses ''PackageDescriptionType

getPackageType :: PackageVersion -> PackageDescriptionType
getPackageType = \case
  PackageVersion1 -> v1PackageDescriptionType
  PackageVersion2 -> v2PackageDescriptionType
  PackageBasic -> basicPackageDescriptionType

-- | The names of the Package type name in every version of the PackageDescription module
packageDescriptionTypes :: [PackageDescriptionType]
packageDescriptionTypes = map getPackageType allElements

basicPackageDescriptionType :: PackageDescriptionType
basicPackageDescriptionType =
  PackageDescriptionType
    { _packageDescriptionTypePath = basicPackageDescriptionFile,
      _packageDescriptionTypeName = "Package",
      _packageDescriptionTypeTransform = fromPackage,
      _packageDescriptionTypeToPackage = toPackage,
      _packageDescriptionTypeNeedsStdlibImport = const False,
      _packageDescriptionTypeVersion = PackageBasic
    }
  where
    fromPackage :: Package -> FunctionDefBody 'Parsed
    fromPackage p = run . runReader l $ do
      bodyExpression <- NEL.singleton <$> identifier "basicPackage"
      functionDefExpression bodyExpression
      where
        l :: Interval
        l = singletonInterval (mkInitialLoc (p ^. packageFile))

    toPackage ::
      BuildDir ->
      Path Abs File ->
      Value ->
      Sem r Package
    toPackage buildDir f _ = return (emptyPackage buildDir f)

v2PackageDescriptionType :: PackageDescriptionType
v2PackageDescriptionType =
  PackageDescriptionType
    { _packageDescriptionTypePath = v2PackageDescriptionFile,
      _packageDescriptionTypeName = "Package",
      _packageDescriptionTypeTransform = v1v2FromPackage,
      _packageDescriptionTypeToPackage = v1v2ToPackage,
      _packageDescriptionTypeNeedsStdlibImport = const False,
      _packageDescriptionTypeVersion = PackageVersion2
    }

v1PackageDescriptionType :: PackageDescriptionType
v1PackageDescriptionType =
  PackageDescriptionType
    { _packageDescriptionTypePath = v1PackageDescriptionFile,
      _packageDescriptionTypeName = "Package",
      _packageDescriptionTypeTransform = v1v2FromPackage,
      _packageDescriptionTypeToPackage = v1v2ToPackage,
      _packageDescriptionTypeNeedsStdlibImport = needsStdlib,
      _packageDescriptionTypeVersion = PackageVersion1
    }
  where
    needsStdlib :: Package -> Bool
    needsStdlib p =
      let SemVer {..} = p ^. packageVersion
       in isJust _svMeta || isJust _svPreRel || isJust (p ^. packageMain) || isJust (p ^. packageBuildDir)

v1v2FromPackage :: Package -> FunctionDefBody 'Parsed
v1v2FromPackage p = run . runReader l $ do
  bodyExpression <-
    maybeM
      defaultPackageNoArgs
      defaultPackageWithArgs
      (nonEmpty <$> mkNamedArgs)
  functionDefExpression bodyExpression
  where
    defaultPackageStr :: Text
    defaultPackageStr = "defaultPackage"

    defaultPackageNoArgs :: (Member (Reader Interval) r) => Sem r (NonEmpty (ExpressionAtom 'Parsed))
    defaultPackageNoArgs = NEL.singleton <$> identifier defaultPackageStr

    defaultPackageWithArgs :: (Member (Reader Interval) r) => NonEmpty (NamedArgumentAssign 'Parsed) -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
    defaultPackageWithArgs as = do
      defaultPackageName' <- NameUnqualified <$> symbol defaultPackageStr
      argBlock <- argumentBlock Implicit as
      let defaultPackageArg = namedApplication defaultPackageName' (argBlock :| [])
      return (defaultPackageArg :| [])

    l :: Interval
    l = singletonInterval (mkInitialLoc (p ^. packageFile))

    mkNamedArgs :: forall r. (Member (Reader Interval) r) => Sem r [NamedArgumentAssign 'Parsed]
    mkNamedArgs = do
      catMaybes <$> sequence [mkNameArg, mkVersionArg, mkDependenciesArg, mkMainArg, mkBuildDirArg]
      where
        mkNameArg :: Sem r (Maybe (NamedArgumentAssign 'Parsed))
        mkNameArg
          | defaultPackageName == p ^. packageName = return Nothing
          | otherwise = do
              n <- literalString (p ^. packageName)
              Just <$> namedArgument "name" (n :| [])

        mkDependenciesArg :: Sem r (Maybe (NamedArgumentAssign 'Parsed))
        mkDependenciesArg = do
          let deps = p ^. packageDependencies
              dependenciesArg = Just <$> mkDependenciesArg' (p ^. packageDependencies)
          case deps of
            [d] ->
              if
                | d == defaultStdlibDep DefaultBuildDir -> return Nothing
                | otherwise -> dependenciesArg
            _ -> dependenciesArg
          where
            mkDependenciesArg' :: [Dependency] -> Sem r (NamedArgumentAssign 'Parsed)
            mkDependenciesArg' ds = do
              deps <- mkList =<< mapM mkDependencyArg ds
              namedArgument "dependencies" (deps :| [])

            mkDependencyArg :: Dependency -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
            mkDependencyArg = \case
              DependencyPath x ->
                sequence
                  ( identifier "path"
                      :| [literalString (pack (unsafePrepathToFilePath (x ^. pathDependencyPath)))]
                  )
              DependencyGit x ->
                sequence
                  ( identifier "git"
                      :| ( literalString
                             <$> [ x ^. gitDependencyName,
                                   x ^. gitDependencyUrl,
                                   x ^. gitDependencyRef
                                 ]
                         )
                  )

        mkMainArg :: Sem r (Maybe (NamedArgumentAssign 'Parsed))
        mkMainArg = do
          arg <- mapM mainArg (p ^. packageMain)
          mapM (namedArgument "main") arg
          where
            mainArg :: Prepath File -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
            mainArg p' = mkJust =<< literalString (pack (unsafePrepathToFilePath p'))

        mkBuildDirArg :: Sem r (Maybe (NamedArgumentAssign 'Parsed))
        mkBuildDirArg = do
          arg <- mapM buildDirArg (p ^. packageBuildDir)
          mapM (namedArgument "buildDir") arg
          where
            buildDirArg :: SomeBase Dir -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
            buildDirArg d = mkJust =<< literalString (pack (fromSomeDir d))

        mkVersionArg :: Sem r (Maybe (NamedArgumentAssign 'Parsed))
        mkVersionArg
          | p ^. packageVersion == defaultVersion = return Nothing
          | otherwise = Just <$> mkVersionArg'
          where
            mkVersionArg' :: Sem r (NamedArgumentAssign 'Parsed)
            mkVersionArg' = do
              mkVersionArgs <- liftM2 (++) explicitArgs implicitArgs
              mkVersionName <- identifier "mkVersion"
              namedArgument "version" (mkVersionName :| mkVersionArgs)

            explicitArgs :: Sem r [ExpressionAtom 'Parsed]
            explicitArgs =
              let SemVer {..} = p ^. packageVersion
               in mapM literalInteger [_svMajor, _svMinor, _svPatch]

            implicitArgs :: Sem r [ExpressionAtom 'Parsed]
            implicitArgs = do
              releaseArg' <- releaseArg
              metaArg' <- metaArg
              mapM
                (>>= braced)
                ( case (releaseArg', metaArg') of
                    (Nothing, Nothing) -> []
                    (Nothing, Just ma) -> [mkNothing, mkJust ma]
                    (Just ra, Nothing) -> [mkJust ra]
                    (Just ra, Just ma) -> [mkJust ra, mkJust ma]
                )

            releaseArg :: Sem r (Maybe (ExpressionAtom 'Parsed))
            releaseArg = let SemVer {..} = p ^. packageVersion in mapM mkReleaseArg _svPreRel
              where
                mkReleaseArg :: Release -> Sem r (ExpressionAtom 'Parsed)
                mkReleaseArg = literalString . prettyRelease

                prettyRelease :: Release -> Text
                prettyRelease (Release cs) = T.intercalate "." . map prettyChunk $ NEL.toList cs

                prettyChunk :: Chunk -> Text
                prettyChunk (Numeric n) = show n
                prettyChunk (Alphanum s) = s

            metaArg :: Sem r (Maybe (ExpressionAtom 'Parsed))
            metaArg = let SemVer {..} = p ^. packageVersion in mapM literalString _svMeta

        mkJust :: ExpressionAtom 'Parsed -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
        mkJust a = do
          justIdent <- identifier "just"
          return (justIdent :| [a])

        mkNothing :: Sem r (NonEmpty (ExpressionAtom 'Parsed))
        mkNothing = do
          nothingIdent <- identifier "nothing"
          return (nothingIdent :| [])

v1v2ToPackage ::
  forall r.
  (Member (Error PackageLoaderError) r) =>
  BuildDir ->
  Path Abs File ->
  Value ->
  Sem r Package
v1v2ToPackage buildDir packagePath = \case
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
        [] -> return (defaultStdlib buildDir)
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

defaultStdlib :: BuildDir -> Dependency
defaultStdlib buildDir = mkPathDependency (fromSomeDir p)
  where
    p :: SomeBase Dir
    p = resolveBuildDir buildDir <///> relStdlibDir
