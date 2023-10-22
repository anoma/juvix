module Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Builtins
import Juvix.Compiler.Concrete hiding (Symbol)
import Juvix.Compiler.Core (CoreResult, coreResultTable)
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra.Value
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff
import Juvix.Compiler.Pipeline.Package.Loader.PathResolver
import Juvix.Data.Effect.Git
import Juvix.Data.Effect.Process
import Juvix.Extra.Paths qualified as Paths

data LoaderResource = LoaderResource
  { _loaderResourceResult :: CoreResult,
    _loaderResourcePackagePath :: Path Abs File
  }

makeLenses ''LoaderResource

runEvalFileEffIO :: forall r a. (Members '[Embed IO, Error PackageLoaderError] r) => Sem (EvalFileEff ': r) a -> Sem r a
runEvalFileEffIO = interpretScopedAs allocator handler
  where
    allocator :: Path Abs File -> Sem r LoaderResource
    allocator p = do
      res <- loadPackage' p
      return
        LoaderResource
          { _loaderResourceResult = res,
            _loaderResourcePackagePath = p
          }

    handler :: LoaderResource -> EvalEff m x -> Sem r x
    handler res = \case
      Eval' n -> toValue tab <$> evalNode n
      LookupIdentifier n ->
        ( maybe
            ( throw
                PackageLoaderError
                  { _packageLoaderErrorPath = packagePath,
                    _packageLoaderErrorCause = ErrPackageSymbolNotFound
                  }
            )
            return
            ( identifierSymbol n
                >>= (`HashMap.lookup` (tab ^. Core.identContext))
            )
        )
      AssertNodeType n ty -> assertNodeType' n ty
      where
        tab :: Core.InfoTable
        tab = res ^. loaderResourceResult . coreResultTable

        packagePath :: Path Abs File
        packagePath = res ^. loaderResourcePackagePath

        identifiers :: HashMap Symbol Core.IdentifierInfo
        identifiers = tab ^. Core.infoIdentifiers

        identifierSymbol :: Text -> Maybe Symbol
        identifierSymbol n = fst <$> find (\(_, ii) -> ii ^. Core.identifierName == n) (HashMap.toList identifiers)

        evalNode :: Node -> Sem r Node
        evalNode n = do
          n' <- doEval False packageLoc tab n
          case n' of
            Left e -> do
              throw
                PackageLoaderError
                  { _packageLoaderErrorPath = packagePath,
                    _packageLoaderErrorCause =
                      ErrPackageEvaluationError
                        PackageEvaluationError
                          { _packageEvaluationErrorError = JuvixError e
                          }
                  }
            Right resN -> return resN
          where
            packageLoc :: Interval
            packageLoc = singletonInterval (mkInitialLoc packagePath)

        assertNodeType' :: Node -> TypeSpec -> Sem r ()
        assertNodeType' n s = do
          evalN <- evalNode n
          case evalN of
            NCtr Constr {..} -> do
              let ci = Core.lookupConstructorInfo tab _constrTag
                  ii = Core.lookupInductiveInfo tab (ci ^. Core.constructorInductive)
              checkInductiveType ii
            _ -> err
          where
            err :: Sem r b
            err = throw PackageLoaderError {_packageLoaderErrorPath = packagePath, _packageLoaderErrorCause = ErrPackageTypeError}

            -- Check that the type of the package identifier is the expected type from the specific
            -- PackageDescription module that is provided by the PathResolver
            checkInductiveType :: Core.InductiveInfo -> Sem r ()
            checkInductiveType ii = checkPackageName >> checkPackageLocation
              where
                checkPackageName :: Sem r ()
                checkPackageName = unless (ii ^. Core.inductiveName == s ^. typeSpecName) err

                checkPackageLocation :: Sem r ()
                checkPackageLocation = case ii ^. Core.inductiveLocation of
                  Just l -> unless (l ^. intervalFile == s ^. typeSpecFile) err
                  Nothing -> err

loadPackage' :: (Members '[Embed IO, Error PackageLoaderError] r) => Path Abs File -> Sem r CoreResult
loadPackage' packagePath = do
  ( mapError
      ( \e ->
          PackageLoaderError
            { _packageLoaderErrorPath = packagePath,
              _packageLoaderErrorCause = ErrPackageJuvixError (PackageJuvixError e)
            }
      )
      . evalInternetOffline
      . ignoreHighlightBuilder
      . runProcessIO
      . runFilesIO
      . evalTopBuiltins
      . evalTopNameIdGen
      . evalTopBuiltins
      . evalTopNameIdGen
      . runReader packageEntryPoint
      . ignoreLog
      . mapError (JuvixError @GitProcessError)
      . runGitProcess
      . runPackagePathResolver rootPath
      $ upToEval
    )
  where
    rootPath :: Path Abs Dir
    rootPath = parent packagePath

    packageEntryPoint :: EntryPoint
    packageEntryPoint = defaultEntryPoint roots packagePath
      where
        roots :: Roots
        roots =
          Roots
            { _rootsRootDir = rootPath,
              _rootsPackageGlobal = False,
              _rootsPackage = rootsPkg,
              _rootsInvokeDir = rootPath,
              _rootsBuildDir = Paths.rootBuildDir rootPath
            }

        rootsPkg :: Package
        rootsPkg =
          Package
            { _packageVersion = defaultVersion,
              _packageName = "Package",
              _packageMain = Nothing,
              _packageLockfile = Nothing,
              _packageFile = packagePath,
              _packageDependencies = [],
              _packageBuildDir = Nothing
            }
