module Commands.Dev.Core.Repl where

import Commands.Base
import Commands.Dev.Core.Repl.Options
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra.Base qualified as Core
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Normalizer
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core
import Juvix.Extra.Paths

runCommand :: forall r. (Members '[Embed IO, App] r) => CoreReplOptions -> Sem r ()
runCommand opts = do
  showReplWelcome
  runRepl opts Core.emptyInfoTable

parseText :: Core.InfoTable -> Text -> Either Core.MegaparsecError (Core.InfoTable, Maybe Core.Node)
parseText = Core.runParser replPath

runRepl :: forall r. (Members '[Embed IO, App] r) => CoreReplOptions -> Core.InfoTable -> Sem r ()
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
        case parseText tab (fromString s') of
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
        case parseText tab (fromString s') of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', Just node) ->
            replEval True tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 'n' : ' ' : s' ->
        case parseText tab (fromString s') of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', Just node) ->
            replNormalize tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 't' : ' ' : s' ->
        case parseText tab (fromString s') of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', Just node) ->
            replType tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 'l' : ' ' : f -> do
        s' <- embed (readFile f)
        sf <- someBaseToAbs' (someFile f)
        case Core.runParser sf Core.emptyInfoTable s' of
          Left err -> do
            printJuvixError (JuvixError err)
            runRepl opts tab
          Right (tab', mnode) -> case mnode of
            Nothing -> runRepl opts tab'
            Just node -> replEval False tab' node
      ":r" ->
        runRepl opts Core.emptyInfoTable
      _ ->
        case parseText tab s of
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
              renderStdOut (Core.ppOut opts (Core.disambiguateNodeNames tab' node'))
              embed (putStrLn "")
              runRepl opts tab'
      where
        defaultLoc = singletonInterval (mkInitialLoc replPath)

    replNormalize :: Core.InfoTable -> Core.Node -> Sem r ()
    replNormalize tab' node =
      let node' = normalize tab' node
       in if
              | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
                  runRepl opts tab'
              | otherwise -> do
                  renderStdOut (Core.ppOut opts (Core.disambiguateNodeNames tab' node'))
                  embed (putStrLn "")
                  runRepl opts tab'

    replType :: Core.InfoTable -> Core.Node -> Sem r ()
    replType tab' node = do
      let ty = Core.disambiguateNodeNames tab' (Core.computeNodeType tab' node)
      renderStdOut (Core.ppOut opts ty)
      embed (putStrLn "")
      runRepl opts tab'

showReplWelcome :: (Members '[Embed IO, App] r) => Sem r ()
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
  putStrLn ":n expr               Normalize \"expr\"."
  putStrLn ":t expr               Infer and print the type of \"expr\"."
  putStrLn ":l file               Load and evaluate \"file\". Resets REPL state."
  putStrLn ":r                    Reset REPL state."
  putStrLn ":q                    Quit."
  putStrLn ":h                    Display this help message."
  putStrLn ""
