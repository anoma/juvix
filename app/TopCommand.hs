module TopCommand where

import Commands.Base hiding (Format)
import Commands.Clean qualified as Clean
import Commands.Compile qualified as Compile
import Commands.Dependencies qualified as Dependencies
import Commands.Dev qualified as Dev
import Commands.Doctor qualified as Doctor
import Commands.Eval qualified as Eval
import Commands.Format qualified as Format
import Commands.Html qualified as Html
import Commands.Init qualified as Init
import Commands.Repl qualified as Repl
import Commands.Typecheck qualified as Typecheck
import Juvix.Extra.Version
import System.Environment (getProgName)
import TopCommand.Options

showHelpText :: IO ()
showHelpText = do
  let p = prefs showHelpOnEmpty
  progn <- getProgName
  let helpText = parserFailure p descr (ShowHelpText Nothing) []
      (msg, _) = renderFailure helpText progn
  putStrLn (pack msg)

runTopCommand :: forall r. (Members '[Embed IO, App, Resource] r) => TopCommand -> Sem r ()
runTopCommand = \case
  DisplayVersion -> embed runDisplayVersion
  DisplayNumericVersion -> embed runDisplayNumericVersion
  DisplayHelp -> embed showHelpText
  Doctor opts -> runLogIO (Doctor.runCommand opts)
  Init opts -> runLogIO (Init.init opts)
  Dev opts -> Dev.runCommand opts
  Typecheck opts -> Typecheck.runCommand opts
  Compile opts -> Compile.runCommand opts
  Clean opts -> runFilesIO (Clean.runCommand opts)
  Eval opts -> Eval.runCommand opts
  Html opts -> Html.runCommand opts
  JuvixRepl opts -> Repl.runCommand opts
  JuvixFormat opts -> runFilesIO (Format.runCommand opts)
  Dependencies opts -> Dependencies.runCommand opts
