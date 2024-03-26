module Commands.Extra.NewCompile where

import Commands.Base

getOutputFile :: (Members '[App] r) => FileExt -> AppPath File -> Maybe (AppPath File) -> Sem r (Path Abs File)
getOutputFile ext inp mout = do
  case mout of
    Just out -> fromAppPathFile out
    Nothing -> do
      i <- fromAppPathFile inp
      return (replaceExtension' (unpack (fileExtToText ext)) i)
