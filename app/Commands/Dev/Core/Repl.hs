module Commands.Dev.Core.Repl where

import Commands.Base
import Commands.Dev.Core.Repl.Options
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Core.Evaluator qualified as Core
import Juvix.Compiler.Core.Extra.Base qualified as Core
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Normalizer
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation.ComputeTypeInfo qualified as Core
import Juvix.Compiler.Core.Transformation.DisambiguateNames qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core
import Juvix.Data.Field
import Juvix.Extra.Paths

runCommand :: forall r. (Members '[EmbedIO, App] r) => CoreReplOptions -> Sem r ()
runCommand opts = do
  showReplWelcome
  runRepl opts mempty

parseText :: Core.InfoTable -> Text -> Either JuvixError (Core.InfoTable, Maybe Core.Node)
parseText = Core.runParser replPath defaultModuleId

runRepl :: forall r. (Members '[EmbedIO, App] r) => CoreReplOptions -> Core.InfoTable -> Sem r ()
runRepl opts tab = do
  putStr "> "
  hFlush stdout
  done <- liftIO isEOF
  unless done $ do
    s <- getLine
    case fromText (strip s) of
      ":q" -> return ()
      ":h" -> do
        showReplHelp
        runRepl opts tab
      ':' : 'p' : ' ' : s' ->
        case parseText tab (fromString s') of
          Left err -> do
            printJuvixError err
            runRepl opts tab
          Right (tab', Just node) -> do
            renderStdOut (Core.ppOut opts node)
            putStrLn ""
            runRepl opts tab'
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 'e' : ' ' : s' ->
        case parseText tab (fromString s') of
          Left err -> do
            printJuvixError err
            runRepl opts tab
          Right (tab', Just node) ->
            replEval True tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 'n' : ' ' : s' ->
        case parseText tab (fromString s') of
          Left err -> do
            printJuvixError err
            runRepl opts tab
          Right (tab', Just node) ->
            replNormalize tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 't' : ' ' : s' ->
        case parseText tab (fromString s') of
          Left err -> do
            printJuvixError err
            runRepl opts tab
          Right (tab', Just node) ->
            replType tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
      ':' : 'l' : ' ' : f -> do
        s' <- readFile (absFile f)
        sf <- someBaseToAbs' (someFile f)
        case Core.runParser sf defaultModuleId mempty s' of
          Left err -> do
            printJuvixError err
            runRepl opts tab
          Right (tab', mnode) -> case mnode of
            Nothing -> runRepl opts tab'
            Just node -> replEval False tab' node
      ":r" ->
        runRepl opts mempty
      _ ->
        case parseText tab s of
          Left err -> do
            printJuvixError err
            runRepl opts tab
          Right (tab', Just node) ->
            replEval False tab' node
          Right (tab', Nothing) ->
            runRepl opts tab'
  where
    replEval :: Bool -> Core.InfoTable -> Core.Node -> Sem r ()
    replEval noIO tab' node = do
      r <- Core.doEval Nothing noIO defaultLoc tab' node
      case r of
        Left err -> do
          printJuvixError (JuvixError err)
          runRepl opts tab'
        Right node'
          | Info.member Info.kNoDisplayInfo (Core.getInfo node') -> runRepl opts tab'
          | otherwise -> do
              renderStdOut (Core.ppOut opts (Core.disambiguateNodeNames (Core.moduleFromInfoTable tab') node'))
              putStrLn ""
              runRepl opts tab'
      where
        defaultLoc = singletonInterval (mkInitialLoc replPath)

    replNormalize :: Core.InfoTable -> Core.Node -> Sem r ()
    replNormalize tab' node =
      let md' = Core.moduleFromInfoTable tab'
          node' = normalize (maximum allowedFieldSizes) md' node
       in if
              | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
                  runRepl opts tab'
              | otherwise -> do
                  renderStdOut (Core.ppOut opts (Core.disambiguateNodeNames md' node'))
                  putStrLn ""
                  runRepl opts tab'

    replType :: Core.InfoTable -> Core.Node -> Sem r ()
    replType tab' node = do
      let md' = Core.moduleFromInfoTable tab'
          ty = Core.disambiguateNodeNames md' (Core.computeNodeType md' node)
      renderStdOut (Core.ppOut opts ty)
      putStrLn ""
      runRepl opts tab'

showReplWelcome :: (MonadIO m) => m ()
showReplWelcome = do
  putStrLn "JuvixCore REPL"
  putStrLn ""
  putStrLn "Type \":h\" for help."
  putStrLn ""

showReplHelp :: (MonadIO m) => m ()
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
