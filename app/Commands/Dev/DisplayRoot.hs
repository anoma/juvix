module Commands.Dev.DisplayRoot where

import Commands.Base
import Commands.Dev.DisplayRoot.Options
import Commands.Extra.Package

runCommand :: forall r. (Members '[EmbedIO, App] r) => RootOptions -> Sem r ()
runCommand RootOptions {..} = do
  askPkgDir >>= renderStdOutLn . pack . toFilePath
  when _rootPrintPackage printPackage
  where
    printPackage :: Sem r ()
    printPackage = do
      renderStdOutLn @Text "+----------------------------+"
      askPackage >>= renderStdOutLn . renderPackage
