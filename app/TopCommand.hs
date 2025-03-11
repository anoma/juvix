module TopCommand where

import Commands.Base
import Commands.Clean qualified as Clean
import Commands.Compile qualified as Compile
import Commands.Dependencies qualified as Dependencies
import Commands.Dev qualified as Dev
import Commands.Doctor qualified as Doctor
import Commands.Eval qualified as Eval
import Commands.Format qualified as Format
import Commands.Html qualified as Html
import Commands.Init qualified as Init
import Commands.Isabelle qualified as Isabelle
import Commands.Markdown qualified as Markdown
import Commands.Repl qualified as Repl
import Commands.Typecheck qualified as Typecheck
import Juvix.Extra.Version
import System.Environment qualified as E
import TopCommand.Options

showHelpText :: (MonadIO m) => m ()
showHelpText = do
  let p = prefs showHelpOnEmpty
  progn <- liftIO E.getProgName
  let helpText = parserFailure p descr (ShowHelpText Nothing) []
      (msg, _) = renderFailure helpText progn
  putStrLn (pack msg)

runTopCommand ::
  forall r.
  (Members AppEffects r) =>
  TopCommand ->
  Sem r ()
runTopCommand = \case
  DisplayVersion -> runDisplayVersion
  DisplayNumericVersion -> runDisplayNumericVersion
  DisplayHelp -> showHelpText
  Doctor opts -> Doctor.runCommand opts
  Isabelle opts -> Isabelle.runCommand opts
  Init opts -> Init.init opts
  Dev opts -> Dev.runCommand opts
  Typecheck opts -> Typecheck.runCommand opts
  Compile opts -> Compile.runCommand opts
  Clean opts -> Clean.runCommand opts
  Eval opts -> Eval.runCommand opts
  Html opts -> Html.runCommand opts
  Markdown opts -> Markdown.runCommand opts
  JuvixRepl opts -> Repl.runCommand opts
  JuvixFormat opts -> Format.runCommand opts
  Dependencies opts -> Dependencies.runCommand opts
