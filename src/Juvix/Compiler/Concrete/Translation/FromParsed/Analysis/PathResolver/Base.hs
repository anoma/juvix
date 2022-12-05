module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Prelude
import Juvix.Prelude.Path as Path

topModulePathToRelativeFilePath' :: TopModulePath -> Path Rel File
topModulePathToRelativeFilePath' = topModulePathToRelativeFilePath ".juvix" "" (</>)

topModulePathToRelativeFilePath :: String -> String -> (FilePath -> FilePath -> FilePath) -> TopModulePath -> Path Rel File
topModulePathToRelativeFilePath ext suffix joinpath mp = relFile relFilePath
  where
    relDirPath :: FilePath
    relDirPath = foldr (joinpath . toPath) mempty (mp ^. modulePathDir)
    relFilePath :: FilePath
    relFilePath = addExt (relDirPath `joinpath'` toPath (mp ^. modulePathName) <> suffix)
    joinpath' :: FilePath -> FilePath -> FilePath
    joinpath' l r
      | null l = r
      | otherwise = joinpath l r
    addExt = (<.> ext)
    toPath :: Symbol -> FilePath
    toPath s = unpack (s ^. symbolText)

topModulePathToRelativeFilePathDot :: String -> String -> TopModulePath -> Path Rel File
topModulePathToRelativeFilePathDot ext suff m = topModulePathToRelativeFilePath ext suff joinDot m
  where
    joinDot :: FilePath -> FilePath -> FilePath
    joinDot l r = l <.> r
