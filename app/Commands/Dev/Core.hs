module Commands.Dev.Core where

import Commands.Base
import Commands.Dev.Core.Options
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Error qualified as Core
import Juvix.Compiler.Core.Evaluator qualified as Core
import Juvix.Compiler.Core.Extra.Base qualified as Core
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.NoDisplayInfo qualified as Info
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Transformation qualified as Core
import Juvix.Compiler.Core.Translation.FromSource qualified as Core
import Text.Megaparsec.Pos qualified as M

runCommand :: forall r. Members '[Embed IO, App] r => CoreCommand -> Sem r ()
runCommand cm = do
  globalOpts <- askGlobalOptions
  go globalOpts
  where
    go :: GlobalOptions -> Sem r ()
    go globalOpts =
      case cm of
        Repl opts -> do
          embed showReplWelcome
          runRepl opts Core.emptyInfoTable
        Eval opts -> getFile >>= evalFile opts
        Read opts -> getFile >>= runRead opts
      where
        getFile :: Sem r FilePath
        getFile = case globalOpts ^? globalInputFiles . _head of
          Nothing -> printFailureExit "Provide a JuvixCore file to run this command\nUse --help to see all the options"
          Just f -> return f

        runRead :: CoreReadOptions -> FilePath -> Sem r ()
        runRead opts f = do
          s' <- embed (readFile f)
          tab <- getRight (fst <$> mapLeft JuvixError (Core.runParser "" f Core.emptyInfoTable s'))
          let tab' = Core.applyTransformations (opts ^. coreReadTransformations) tab
          renderStdOut (Core.ppOut opts tab')

        runRepl :: CoreReplOptions -> Core.InfoTable -> Sem r ()
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

        showReplWelcome :: IO ()
        showReplWelcome = do
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

        evalFile :: CoreEvalOptions -> FilePath -> Sem r ()
        evalFile opts f = do
          s <- embed (readFile f)
          case Core.runParser "" f Core.emptyInfoTable s of
            Left err -> exitJuvixError (JuvixError err)
            Right (tab, Just node) -> do
              r <- doEval (opts ^. coreEvalNoIO) defaultLoc tab node
              case r of
                Left err -> exitJuvixError (JuvixError err)
                Right node'
                  | Info.member Info.kNoDisplayInfo (Core.getInfo node') ->
                      return ()
                Right node' -> do
                  renderStdOut (Core.ppOut docOpts node')
                  embed (putStrLn "")
            Right (_, Nothing) -> return ()
          where
            docOpts :: Core.Options
            docOpts = Core.defaultOptions
            defaultLoc = singletonInterval (mkLoc f 0 (M.initialPos f))

        doEval ::
          Bool ->
          Interval ->
          Core.InfoTable ->
          Core.Node ->
          Sem r (Either Core.CoreError Core.Node)
        doEval noIO loc tab node
          | noIO = embed $ Core.catchEvalError loc (Core.eval (tab ^. Core.identContext) [] node)
          | otherwise = embed $ Core.catchEvalErrorIO loc (Core.evalIO (tab ^. Core.identContext) [] node)
