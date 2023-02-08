{-# LANGUAGE QuasiQuotes #-}

module Commands.Dev.Geb.Repl where

import Commands.Base hiding (command)
import Commands.Dev.Geb.Eval as Geb
import Commands.Dev.Geb.Repl.Colors
import Commands.Dev.Geb.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Juvix.Compiler.Backend.Geb.Evaluator qualified as Geb
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference qualified as Geb
import Juvix.Data.Error.GenericError qualified as Error
import Juvix.Extra.Version
import Juvix.Prelude.Pretty qualified as P
import System.Console.ANSI qualified as Ansi
import System.Console.Haskeline
-- import System.Console.Repline
import System.Console.Repline qualified as Repline

type ReplS = State.StateT ReplState IO

type Repl a = Repline.HaskelineT ReplS a

newtype ReplContext = ReplContext
  { _replContextEntryPoint :: EntryPoint
  }

data ReplState = ReplState
  { _replStateContext :: Maybe ReplContext,
    _replStateInvokeDir :: Path Abs Dir,
    _replStateGlobalOptions :: GlobalOptions
  }

makeLenses ''ReplState
makeLenses ''ReplContext

noFileLoadedMsg :: String
noFileLoadedMsg = formatError <> "No file loaded. Load a file using the `:load FILE` command."

-- | imaginary file path for error messages in the repl.
replPath :: Path Abs File
replPath = $(mkAbsFile "/<repl>")

runCommand :: (Members '[Embed IO, App] r) => GebReplOptions -> Sem r ()
runCommand _opts = do
  root <- askPkgDir
  buildDir <- askBuildDir
  package <- askPackage
  let getReplEntryPoint :: SomeBase File -> Repl EntryPoint
      getReplEntryPoint inputFile = do
        gopts <- State.gets (^. replStateGlobalOptions)
        absInputFile :: Path Abs File <- replMakeAbsolute inputFile
        return $
          EntryPoint
            { _entryPointRoot = root,
              _entryPointBuildDir = buildDir,
              _entryPointResolverRoot = root,
              _entryPointNoTermination = gopts ^. globalNoTermination,
              _entryPointNoPositivity = gopts ^. globalNoPositivity,
              _entryPointNoStdlib = gopts ^. globalNoStdlib,
              _entryPointPackage = package,
              _entryPointModulePaths = pure absInputFile,
              _entryPointGenericOptions = project gopts,
              _entryPointStdin = Nothing
            }

      loadEntryPoint :: EntryPoint -> Repl ()
      loadEntryPoint ep = do
        State.modify
          ( set
              replStateContext
              ( Just
                  ( ReplContext
                      { _replContextEntryPoint = ep
                      }
                  )
              )
          )
        let epPath :: Path Abs File = ep ^. entryPointModulePaths . _head1
        liftIO (putStrLn . pack $ "OK loaded " <> toFilePath epPath)
        content <- liftIO (readFile (toFilePath epPath))
        let evalRes =
              Geb.runEval $
                Geb.RunEvalArgs
                  { _runEvalArgsContent = content,
                    _runEvalArgsInputFile = epPath,
                    _runEvalArgsEvaluatorOptions = Geb.defaultEvaluatorOptions
                  }
        case evalRes of
          Left err -> printError err
          Right n -> renderOut (Geb.ppOut Geb.defaultEvaluatorOptions n)

      reloadFile :: String -> Repl ()
      reloadFile _ = do
        mentryPoint <- State.gets (fmap (^. replContextEntryPoint) . (^. replStateContext))
        case mentryPoint of
          Just entryPoint -> do
            loadEntryPoint entryPoint
          Nothing -> liftIO . putStrLn $ pack noFileLoadedMsg

      pSomeFile :: String -> SomeBase File
      pSomeFile = someFile . unpack . strip . pack

      loadFile :: SomeBase File -> Repl ()
      loadFile f = do
        entryPoint <- getReplEntryPoint f
        loadEntryPoint entryPoint

      printRoot :: String -> Repl ()
      printRoot _ = do
        r <- State.gets (^. replStateInvokeDir)
        liftIO $ putStrLn (pack (toFilePath r))

      displayVersion :: String -> Repl ()
      displayVersion _ = liftIO (putStrLn versionTag)

      inferObject :: String -> Repl ()
      inferObject gebMorphism = Repline.dontCrash $ do
        case Geb.runParser replPath (pack gebMorphism) of
          Left err -> printError (JuvixError err)
          Right (Geb.ExpressionMorphism morphism) -> do
            let inferRes = Geb.inferObject' morphism
            case inferRes of
              Right n -> renderOut (Geb.ppOut Geb.defaultEvaluatorOptions n)
              Left err -> printError err
          Right _ -> liftIO . putStrLn $ "Geb object has to be a Geb object?"

      command :: String -> Repl ()
      command gebProgram =
        Repline.dontCrash $ do
          let evalRes =
                Geb.runEval $
                  Geb.RunEvalArgs
                    { _runEvalArgsContent = pack gebProgram,
                      _runEvalArgsInputFile = replPath,
                      _runEvalArgsEvaluatorOptions = Geb.defaultEvaluatorOptions
                    }
          case evalRes of
            Left err -> printError err
            Right n -> renderOut (Geb.ppOut Geb.defaultEvaluatorOptions n)

      options :: [(String, String -> Repl ())]
      options =
        [ ("help", Repline.dontCrash . helpText),
          -- `multiline` is included here for auto-completion purposes only.
          -- `repline`'s `multilineCommand` logic overrides this no-op.
          (multilineCmd, Repline.dontCrash . \_ -> return ()),
          ("quit", quit),
          ("load", Repline.dontCrash . loadFile . pSomeFile),
          ("reload", Repline.dontCrash . reloadFile),
          ("root", printRoot),
          ("type", inferObject),
          ("version", displayVersion)
        ]

      optsCompleter :: Repline.WordCompleter ReplS
      optsCompleter n = do
        let names = (":" <>) . fst <$> options
        return (filter (isPrefixOf n) names)

      prefix :: Maybe Char
      prefix = Just ':'

      multilineCommand :: Maybe String
      multilineCommand = Just multilineCmd

      tabComplete :: Repline.CompleterStyle ReplS
      tabComplete = Repline.Prefix (Repline.wordCompleter optsCompleter) defaultMatcher

      replAction :: ReplS ()
      replAction =
        Repline.evalReplOpts
          Repline.ReplOpts
            { prefix,
              multilineCommand,
              initialiser,
              finaliser,
              tabComplete,
              command,
              options,
              banner
            }

  invokeDir <- askInvokeDir
  globalOptions <- askGlobalOptions
  embed
    ( State.evalStateT
        replAction
        ( ReplState
            { _replStateContext = Nothing,
              _replStateGlobalOptions = globalOptions,
              _replStateInvokeDir = invokeDir
            }
        )
    )

welcomeMsg :: Repl ()
welcomeMsg =
  liftIO . putStrLn . pack $
    unlines
      [ formatIntro <> "Welcome to the Juvix Geb REPL!" <> end,
        mainFormat <> "Juvix " <> unpack versionTag ++ ": https://juvix.org." <> end,
        "Run :help for help" ++ end
      ]

restartText :: Text
restartText = "REPL restarted"

promptText :: String
promptText = formatPrompt ++ "geb> " ++ end

defaultMatcher :: [(String, CompletionFunc ReplS)]
defaultMatcher = [(":load", Repline.fileCompleter)]

banner :: Repline.MultiLine -> Repl String
banner = \case
  Repline.MultiLine -> return "... "
  Repline.SingleLine -> return promptText

helpText :: String -> Repl ()
helpText _ =
  liftIO . putStrLn . pack $
    unlines
      [ mainFormat,
        "EXPRESSION          Evaluate a Geb morphism",
        ":help               Print this help",
        ":load FILE          Load a file into the REPL",
        ":reload             Reload the currently loaded file",
        ":type EXPRESSION    Infer the type of a Geb expression",
        ":version            Display the Juvix version",
        ":multiline          Enter multiline mode",
        ":root               Print the root directory of the REPL",
        ":version            Display the Juvix version",
        ":quit               Exit the REPL" <> end
      ]

multilineCmd :: String
multilineCmd = "multiline"

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

initialiser :: Repl ()
initialiser = welcomeMsg

finaliser :: Repl Repline.ExitDecision
finaliser = return Repline.Exit

replMakeAbsolute :: SomeBase b -> Repl (Path Abs b)
replMakeAbsolute = \case
  Abs p -> return p
  Rel r -> do
    invokeDir <- State.gets (^. replStateInvokeDir)
    return (invokeDir <//> r)

render' :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
render' t = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stdout)
  liftIO (P.renderIO (not (opts ^. globalNoColors) && hasAnsi) t)

renderOut :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
renderOut t = render' t >> liftIO (putStrLn "")

printError :: JuvixError -> Repl ()
printError e = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stderr)
  liftIO $ hPutStrLn stderr $ run (runReader (project' @GenericOptions opts) (Error.render (not (opts ^. globalNoColors) && hasAnsi) False e))
