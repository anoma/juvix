module Juvix.Compiler.Casm.Extra.InputInfo where

import Juvix.Compiler.Casm.Data.InputInfo
import Juvix.Prelude
import Juvix.Prelude.Aeson

readInputInfo :: Maybe (Path Abs File) -> IO InputInfo
readInputInfo inputFile = case inputFile of
  Just file ->
    fromJust <$> readJSONFile (toFilePath file)
  Nothing ->
    return $ InputInfo mempty
