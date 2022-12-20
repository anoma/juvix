module Commands.Dev.DisplayRoot where

import Commands.Base
import Commands.Dev.DisplayRoot.Options
import Data.Yaml

runCommand :: forall r. Members '[Embed IO, App] r => RootOptions -> Sem r ()
runCommand RootOptions {..} = do
  askPkgDir >>= say . pack . toFilePath
  when _rootPrintPackage printPackage
  where
    printPackage :: Sem r ()
    printPackage = do
      say "+----------------------------+"
      askPackage >>= say . decodeUtf8 . encode . rawPackage
