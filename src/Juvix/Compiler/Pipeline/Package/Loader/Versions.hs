module Juvix.Compiler.Pipeline.Package.Loader.Versions where

import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Versions
import Juvix.Compiler.Concrete.Gen
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Extra.Paths
import Juvix.Prelude

data PackageDescriptionType = PackageDescriptionType
  { _packageDescriptionTypePath :: Path Rel File,
    _packageDescriptionTypeName :: Text,
    _packageDescriptionTypeTransform :: Package -> FunctionDefBody 'Parsed,
    _packageDescriptionTypeNeedsStdlibImport :: Package -> Bool
  }

makeLenses ''PackageDescriptionType

data PackageVersion = PackageVersion1

-- | The names of the Package type name in every version of the PackageDescription module
packageDescriptionTypes :: [PackageDescriptionType]
packageDescriptionTypes = [v1PackageDescriptionType]

v1PackageDescriptionType :: PackageDescriptionType
v1PackageDescriptionType = PackageDescriptionType v1PackageDescriptionFile "Package" fromPackage needsStdlib
  where
    needsStdlib :: Package -> Bool
    needsStdlib p =
      let SemVer {..} = p ^. packageVersion
       in isJust _svMeta || isJust _svPreRel || isJust (p ^. packageMain) || isJust (p ^. packageBuildDir)

    fromPackage :: Package -> FunctionDefBody 'Parsed
    fromPackage p = run . runReader l $ do
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

        defaultPackageWithArgs :: (Member (Reader Interval) r) => NonEmpty (NamedArgument 'Parsed) -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
        defaultPackageWithArgs as = do
          defaultPackageName' <- NameUnqualified <$> symbol defaultPackageStr
          argBlock <- argumentBlock Implicit as
          let defaultPackageArg = namedApplication defaultPackageName' (argBlock :| [])
          return (defaultPackageArg :| [])

        l :: Interval
        l = singletonInterval (mkInitialLoc (p ^. packageFile))

        mkNamedArgs :: forall r. (Member (Reader Interval) r) => Sem r [NamedArgument 'Parsed]
        mkNamedArgs = do
          catMaybes <$> sequence [mkNameArg, mkVersionArg, mkDependenciesArg, mkMainArg, mkBuildDirArg]
          where
            mkNameArg :: Sem r (Maybe (NamedArgument 'Parsed))
            mkNameArg
              | defaultPackageName == p ^. packageName = return Nothing
              | otherwise = do
                  n <- literalString (p ^. packageName)
                  Just <$> namedArgument "name" (n :| [])

            mkDependenciesArg :: Sem r (Maybe (NamedArgument 'Parsed))
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
                mkDependenciesArg' :: [Dependency] -> Sem r (NamedArgument 'Parsed)
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

            mkMainArg :: Sem r (Maybe (NamedArgument 'Parsed))
            mkMainArg = do
              arg <- mapM mainArg (p ^. packageMain)
              mapM (namedArgument "main") arg
              where
                mainArg :: Prepath File -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
                mainArg p' = mkJust =<< literalString (pack (unsafePrepathToFilePath p'))

            mkBuildDirArg :: Sem r (Maybe (NamedArgument 'Parsed))
            mkBuildDirArg = do
              arg <- mapM buildDirArg (p ^. packageBuildDir)
              mapM (namedArgument "buildDir") arg
              where
                buildDirArg :: SomeBase Dir -> Sem r (NonEmpty (ExpressionAtom 'Parsed))
                buildDirArg d = mkJust =<< literalString (pack (fromSomeDir d))

            mkVersionArg :: Sem r (Maybe (NamedArgument 'Parsed))
            mkVersionArg
              | p ^. packageVersion == defaultVersion = return Nothing
              | otherwise = Just <$> mkVersionArg'
              where
                mkVersionArg' :: Sem r (NamedArgument 'Parsed)
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
