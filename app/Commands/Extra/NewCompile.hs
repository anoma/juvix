module Commands.Extra.NewCompile
  ( module Commands.Extra.NewCompile,
    module Commands.Extra.Clang,
  )
where

import Commands.Base
import Commands.Extra.Clang

getOutputFile :: (Members '[App] r) => FileExt -> AppPath File -> Maybe (AppPath File) -> Sem r (Path Abs File)
getOutputFile ext inp mout = do
  case mout of
    Just out -> fromAppPathFile out
    Nothing -> do
      i <- fromAppPathFile inp
      return (replaceExtension' (unpack (fileExtToText ext)) i)
