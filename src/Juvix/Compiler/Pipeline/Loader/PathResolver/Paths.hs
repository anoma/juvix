module Juvix.Compiler.Pipeline.Loader.PathResolver.Paths where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Prelude

topModulePathToRelativePath' :: TopModulePath -> Path Rel File
topModulePathToRelativePath' m =
  let absPath :: Path Abs File = getLoc m ^. intervalFile
      ext = fileExtension' absPath
   in topModulePathToRelativePath ext "" (</>) m

topModulePathToRelativePathNoExt :: TopModulePath -> Path Rel File
topModulePathToRelativePathNoExt TopModulePath {..} = relFile (joinFilePaths (map (unpack . (^. withLocParam)) (_modulePathDir ++ [_modulePathName])))

topModulePathToImportScan :: TopModulePath -> ImportScan
topModulePathToImportScan t@TopModulePath {..} =
  ImportScan
    { _importNames = unpack . (^. withLocParam) <$> (NonEmpty.prependList _modulePathDir (pure _modulePathName)),
      _importLoc = getLoc t
    }

topModulePathToRelativePath :: String -> String -> (FilePath -> FilePath -> FilePath) -> TopModulePath -> Path Rel File
topModulePathToRelativePath ext suffix joinpath mp = relFile relFilePath
  where
    relDirPath :: FilePath
    relDirPath = foldr (joinpath . toPath) mempty (mp ^. modulePathDir)

    relFilePath :: FilePath
    relFilePath = addExt (relDirPath `joinpath'` toPath (mp ^. modulePathName) <> suffix)

    joinpath' :: FilePath -> FilePath -> FilePath
    joinpath' l r
      | null l = r
      | otherwise = joinpath l r

    addExt :: FilePath -> FilePath
    addExt = (<.> ext)

    toPath :: Symbol -> FilePath
    toPath s = unpack (s ^. symbolText)

topModulePathToRelativePathDot :: String -> String -> TopModulePath -> Path Rel File
topModulePathToRelativePathDot ext suff m = topModulePathToRelativePath ext suff joinDot m
  where
    joinDot :: FilePath -> FilePath -> FilePath
    joinDot l r = l <.> r
