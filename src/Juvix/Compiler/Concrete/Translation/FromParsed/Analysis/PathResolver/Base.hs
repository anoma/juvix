module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Prelude

topModulePathToRelativeFilePath' :: TopModulePath -> FilePath
topModulePathToRelativeFilePath' = topModulePathToRelativeFilePath ".juvix" "" (</>)

topModulePathToRelativeFilePath :: String -> String -> (FilePath -> FilePath -> FilePath) -> TopModulePath -> FilePath
topModulePathToRelativeFilePath ext suffix joinpath mp = relFilePath
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
