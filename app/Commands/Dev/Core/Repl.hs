module Commands.Dev.Core.Repl where

import Commands.Dev.Core.Eval (doEval)
import Commands.Base
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra.Base qualified as Core
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Text.Megaparsec.Pos qualified as M
import Commands.Dev.Core.Repl.Options
import Juvix.Compiler.Core.Translation.FromSource qualified as Core

runCommand :: forall r. Members '[Embed IO, App] r => CoreReplOptions -> Sem r ()
runCommand opts = do
  showReplWelcome
  runRepl opts Core.emptyInfoTable

runRepl :: forall r. Members '[Embed IO, App] r => CoreReplOptions -> Core.InfoTable -> Sem r ()
runRepl opts tab = do
  embed (putStr "> ")
  embed (hFlush stdout)
  done <- embed isEOF
  unless done $ do
    s <- embed getLine
    case fromText (strip s) of
      ":q" -> return ()
      ":h" -> do
        embed showReplHelp
        runRepl opts tab
      ':' : 'p' : ' ' : s' ->
        case Core.parseText tab (fromString s') of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', Just node) -> do
            renderStdOut (Core.ppOut opts node)
            embed (putStrLn "")
            runRepl opts tab'
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 'e' : ' ' : s' ->
        case Core.parseText tab (fromString s') of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', Just node) ->
            replEval True tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 'l' : ' ' : f -> do
        s' <- embed (readFile f)
        case Core.runParser "" f Core.emptyInfoTable s' of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', mnode) -> case mnode of
            Nothing -> runRepl opts tab'
            Just node -> replEval False tab' node
      ":r" ->
        runRepl opts Core.emptyInfoTable
      _ ->
        case Core.parseText tab s of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', Just node) ->
            replEval False tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
  where
    replEval :: Bool -> Core.InfoTable -> Core.Node -> Sem r ()
    replEval noIO tab' node = do
      r <- doEval noIO defaultLoc tab' node
      case r of
        Left err -> do
          printJuvixError (JuvixError err)
          runRepl opts tab'
        Right node'
          | Info.member Info.kNoDisplayInfo (Core.getInfo node') -> runRepl opts tab'
          | otherwise -> do
              renderStdOut (Core.ppOut opts node')
              embed (putStrLn "")
              runRepl opts tab'
      where
        defaultLoc = singletonInterval (mkLoc "stdin" 0 (M.initialPos "stdin"))

showReplWelcome :: Members '[Embed IO, App] r => Sem r ()
showReplWelcome = embed $ do
  putStrLn "JuvixCore REPL"
  putStrLn ""
  putStrLn "Type \":h\" for help."
  putStrLn ""

showReplHelp :: IO ()
showReplHelp = do
  putStrLn ""
  putStrLn "JuvixCore REPL"
  putStrLn ""
  putStrLn "Type in a JuvixCore program to evaluate."
  putStrLn ""
  putStrLn "Available commands:"
  putStrLn ":p expr               Pretty print \"expr\"."
  putStrLn ":e expr               Evaluate \"expr\" without interpreting IO actions."
  putStrLn ":l file               Load and evaluate \"file\". Resets REPL state."
  putStrLn ":r                    Reset REPL state."
  putStrLn ":q                    Quit."
  putStrLn ":h                    Display this help message."
  putStrLn ""
