module TopCommand where

import Commands.Base
import Commands.Compile qualified as Compile
import Commands.Dev qualified as Dev
import Commands.Doctor qualified as Doctor
import Commands.Html qualified as Html
import Commands.Init qualified as Init
import Commands.Org qualified as Org
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

runTopCommand :: forall r. (Members '[Embed IO, App] r) => TopCommand -> Sem r ()
runTopCommand = \case
  DisplayVersion -> embed runDisplayVersion
  DisplayHelp -> embed showHelpText
  Doctor opts -> runLogIO (Doctor.runCommand opts)
  Init -> runLogIO Init.init
  Dev opts -> Dev.runCommand opts
  Typecheck opts -> Typecheck.runCommand opts
  Compile opts -> Compile.runCommand opts
  Html opts -> Html.runCommand opts
  JuvixRepl opts -> Repl.runCommand opts
  Org opts -> Org.runCommand opts
