{-# LANGUAGE QuasiQuotes #-}

module Commands.Dev.Geb.Repl where

import Commands.Base hiding (command)
import Commands.Dev.Geb.Repl.Format
import Commands.Dev.Geb.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty.Values qualified as GebValue
import Juvix.Data.Error.GenericError qualified as Error
import Juvix.Extra.Version
import Juvix.Prelude.Pretty qualified as P
import System.Console.ANSI qualified as Ansi
import System.Console.Haskeline
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

replPath :: Path Abs File
replPath = $(mkAbsFile "/repl.geb")

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
  invokeDir <- askInvokeDir
  globalOptions <- askGlobalOptions
  embed
    ( State.evalStateT
        (replAction getReplEntryPoint)
        ( ReplState
            { _replStateContext = Nothing,
              _replStateGlobalOptions = globalOptions,
              _replStateInvokeDir = invokeDir
            }
        )
    )

loadEntryPoint :: EntryPoint -> Repl ()
loadEntryPoint ep = do
  State.modify
    ( set
        replStateContext
        (Just (ReplContext {_replContextEntryPoint = ep}))
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
  printEvalResult evalRes

reloadFile :: String -> Repl ()
reloadFile _ = do
  mentryPoint <- State.gets (fmap (^. replContextEntryPoint) . (^. replStateContext))
  maybe noFileLoadedMsg loadEntryPoint mentryPoint

pSomeFile :: String -> SomeBase File
pSomeFile = someFile . unpack . strip . pack

type ReplEntryPoint = SomeBase File -> Repl EntryPoint

loadFile :: ReplEntryPoint -> SomeBase File -> Repl ()
loadFile getReplEntryPoint f = do
  entryPoint <- getReplEntryPoint f
  loadEntryPoint entryPoint

inferObject :: String -> Repl ()
inferObject gebMorphism = Repline.dontCrash $ do
  case Geb.runParser replPath (pack gebMorphism) of
    Left err -> printError (JuvixError err)
    Right (Geb.ExpressionMorphism morphism) -> do
      case Geb.inferObject' morphism of
        Right obj -> renderOut (Geb.ppOut Geb.defaultEvaluatorOptions obj)
        Left err -> printError err
    Right _ -> liftIO . putStrLn $ "No object inferred for a Geb object"

checkTypedMorphism :: String -> Repl ()
checkTypedMorphism gebMorphism = Repline.dontCrash $ do
  case Geb.runParser' replPath (pack gebMorphism) of
    Left err -> printError (JuvixError err)
    Right tyMorphism@(Geb.TypedMorphism {}) -> do
      case Geb.check tyMorphism of
        Right obj -> renderOut (Geb.ppOut Geb.defaultEvaluatorOptions obj)
        Left err -> printError err

runReplCommand :: String -> Repl ()
runReplCommand input =
  Repline.dontCrash $
    do
      let evalRes =
            Geb.runEval $
              Geb.RunEvalArgs
                { _runEvalArgsContent = pack input,
                  _runEvalArgsInputFile = replPath,
                  _runEvalArgsEvaluatorOptions = Geb.defaultEvaluatorOptions
                }
      printEvalResult evalRes

normaliseMorphism :: String -> Repl ()
normaliseMorphism input =
  Repline.dontCrash $ do
    let evalRes =
          Geb.runEval $
            Geb.RunEvalArgs
              { _runEvalArgsContent = pack input,
                _runEvalArgsInputFile = replPath,
                _runEvalArgsEvaluatorOptions =
                  Geb.defaultEvaluatorOptions
                    { Geb._evaluatorOptionsNormalise = True
                    }
              }
    printEvalResult evalRes

options :: ReplEntryPoint -> [(String, String -> Repl ())]
options replEntryPoint =
  [ ("help", Repline.dontCrash . helpText),
    -- `multiline` is included here for auto-completion purposes only.
    -- `repline`'s `multilineCommand` logic overrides this no-op.
    (multilineCmd, Repline.dontCrash . \_ -> return ()),
    ("quit", quit),
    ("load", Repline.dontCrash . loadFile replEntryPoint . pSomeFile),
    ("reload", Repline.dontCrash . reloadFile),
    ("root", printRoot),
    ("type", inferObject),
    ("check", checkTypedMorphism),
    ("normalise", normaliseMorphism),
    ("version", displayVersion)
  ]

optsCompleter :: ReplEntryPoint -> Repline.WordCompleter ReplS
optsCompleter replEntryPoint n = do
  let names = (":" <>) . fst <$> options replEntryPoint
  return (filter (isPrefixOf n) names)

prefix :: Maybe Char
prefix = Just ':'

multilineCommand :: Maybe String
multilineCommand = Just multilineCmd

tabComplete :: ReplEntryPoint -> Repline.CompleterStyle ReplS
tabComplete replEntryPoint =
  Repline.Prefix
    (Repline.wordCompleter (optsCompleter replEntryPoint))
    defaultMatcher

replAction :: ReplEntryPoint -> ReplS ()
replAction replEntryPoint =
  Repline.evalReplOpts
    Repline.ReplOpts
      { prefix,
        multilineCommand,
        initialiser = welcomeMsg,
        finaliser = endSession,
        command = runReplCommand,
        options = options replEntryPoint,
        tabComplete = tabComplete replEntryPoint,
        banner
      }

defaultMatcher :: [(String, CompletionFunc ReplS)]
defaultMatcher = [(":load", Repline.fileCompleter)]

banner :: Repline.MultiLine -> Repl String
banner = \case
  Repline.MultiLine -> return "... "
  Repline.SingleLine -> replPromptText

noFileLoadedMsg :: Repl ()
noFileLoadedMsg =
  renderOut
    . ReplMessageDoc
    $ P.annotate
      ReplError
      "No file loaded. Load a file using the `:load FILE` command."
      <> P.line

welcomeMsg :: Repl ()
welcomeMsg =
  renderOut
    . ReplMessageDoc
    $ P.annotate ReplIntro "Welcome to the Juvix Geb REPL!"
      <> P.line
      <> normal ("Juvix v" <> versionTag <> ": https://juvix.org.")
      <> P.line
      <> normal ("Type :help for help.")
      <> P.line

restartText :: Text
restartText = "REPL restarted"

replPromptText :: Repl String
replPromptText = replString . ReplMessageDoc $ P.annotate ReplPrompt "geb> "

helpText :: String -> Repl ()
helpText _ =
  renderOut
    . ReplMessageDoc
    . normal
    . pack
    $ unlines
      [ "EXPRESSION              Evaluate a Geb morphism",
        ":help                   Print this help",
        ":load FILE              Load a file into the REPL",
        ":reload                 Reload the currently loaded file",
        ":type EXPRESSION        Infer the type of a Geb morphism",
        ":normalise EXPRESSION   Return the normal form of a Geb morphism",
        ":check EXPRESSION       Check the type of a Geb morphism",
        ":version                Display the Juvix version",
        ":multiline              Enter multiline mode",
        ":root                   Print the root directory of the REPL",
        ":version                Display the Juvix version",
        ":quit                   Exit the REPL"
      ]

multilineCmd :: String
multilineCmd = "multiline"

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

endSession :: Repl Repline.ExitDecision
endSession = return Repline.Exit

printRoot :: String -> Repl ()
printRoot _ = do
  r <- State.gets (^. replStateInvokeDir)
  renderOut
    . ReplMessageDoc
    . normal
    $ pack (toFilePath r)

displayVersion :: String -> Repl ()
displayVersion _ =
  renderOut
    . ReplMessageDoc
    $ normal versionTag

replMakeAbsolute :: SomeBase b -> Repl (Path Abs b)
replMakeAbsolute = \case
  Abs p -> return p
  Rel r -> do
    invokeDir <- State.gets (^. replStateInvokeDir)
    return (invokeDir <//> r)

replString :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl String
replString t = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stdout)
  return . unpack $ P.toAnsiText (not (opts ^. globalNoColors) && hasAnsi) t

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
  liftIO
    . hPutStrLn stderr
    . run
    . runReader (project' @GenericOptions opts)
    $ Error.render (not (opts ^. globalNoColors) && hasAnsi) False e

printEvalResult :: Either JuvixError Geb.RunEvalResult -> Repl ()
printEvalResult = \case
  Left err -> printError err
  Right (Geb.RunEvalResultGebValue v) ->
    renderOut (GebValue.ppOut Geb.defaultEvaluatorOptions v)
  Right (Geb.RunEvalResultMorphism morphism) ->
    renderOut (Geb.ppOut Geb.defaultEvaluatorOptions morphism)
