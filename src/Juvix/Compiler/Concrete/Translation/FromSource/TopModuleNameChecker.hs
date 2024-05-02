module Juvix.Compiler.Concrete.Translation.FromSource.TopModuleNameChecker where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Paths
import Juvix.Parser.Error
import Juvix.Prelude

data TopModuleNameChecker :: Effect where
  CheckModulePath :: TopModulePath -> TopModuleNameChecker m ()

makeSem ''TopModuleNameChecker

ignoreTopModuleNameChecker ::
  Sem (TopModuleNameChecker ': r) a ->
  Sem r a
ignoreTopModuleNameChecker = interpret $ \case
  CheckModulePath {} -> return ()

runTopModuleNameChecker ::
  (Members '[PathResolver, Files, Error ParserError] r) =>
  Sem (TopModuleNameChecker ': r) a ->
  Sem r a
runTopModuleNameChecker = interpret $ \case
  CheckModulePath m -> checkModulePath' m

checkModulePath' ::
  (Members '[PathResolver, Files, Error ParserError] s) =>
  TopModulePath ->
  Sem s ()
checkModulePath' topJuvixPath = do
  pathInfo :: PathInfoTopModule <- expectedPathInfoTopModule topJuvixPath
  let expectedRootInfo = pathInfo ^. pathInfoRootInfo
      actualPath = getLoc topJuvixPath ^. intervalFile
  case expectedRootInfo ^. rootInfoKind of
    RootKindSingleFile -> do
      let expectedName = pack . toFilePath . removeExtensions . filename $ actualPath
          actualName = topModulePathToDottedPath topJuvixPath

      unless (expectedName == actualName)
        . throw
        $ ErrWrongTopModuleNameOrphan
          WrongTopModuleNameOrphan
            { _wrongTopModuleNameOrpahnExpectedName = expectedName,
              _wrongTopModuleNameOrpahnActualName = topJuvixPath
            }
    RootKindPackage -> do
      let relPath = topModulePathToRelativePath' topJuvixPath
          expectedAbsPath = (expectedRootInfo ^. rootInfoPath) <//> relPath
      unlessM (equalPaths actualPath expectedAbsPath)
        . throw
        $ ErrWrongTopModuleName
          WrongTopModuleName
            { _wrongTopModuleNameActualName = topJuvixPath,
              _wrongTopModuleNameExpectedPath = expectedAbsPath,
              _wrongTopModuleNameActualPath = actualPath
            }
