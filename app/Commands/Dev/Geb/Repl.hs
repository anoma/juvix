{-# LANGUAGE QuasiQuotes #-}

module Commands.Dev.Geb.Repl where

import Commands.Base hiding
  ( command,
  )
import Commands.Dev.Geb.Repl.Format
import Commands.Dev.Geb.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Data.String.Interpolate (i, __i)
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Error
import Juvix.Data.Error.GenericError qualified as Error
import Juvix.Extra.Paths
import Juvix.Extra.Version
import Juvix.Prelude.Pretty qualified as P
import System.Console.ANSI qualified as Ansi
import System.Console.Haskeline
import System.Console.Repline qualified as Repline

type ReplS = State.StateT ReplState IO

type Repl a = Repline.HaskelineT ReplS a

data ReplState = ReplState
  { _replContextEntryPoint :: Maybe EntryPoint,
    _replStateInvokeDir :: Path Abs Dir,
    _replStateGlobalOptions :: GlobalOptions
  }

makeLenses ''ReplState

runCommand :: (Members '[Embed IO, App] r) => GebReplOptions -> Sem r ()
runCommand replOpts = do
  invokeDir <- askInvokeDir
  roots <- askRoots
  globalOptions <- askGlobalOptions
  let getReplEntryPoint :: SomeBase File -> Repl EntryPoint
      getReplEntryPoint inputFile = do
        gopts <- State.gets (^. replStateGlobalOptions)
        absInputFile :: Path Abs File <- replMakeAbsolute inputFile
        set entryPointTarget Backend.TargetGeb
          <$> liftIO (entryPointFromGlobalOptions roots absInputFile gopts)
  embed
    ( State.evalStateT
        (replAction replOpts getReplEntryPoint)
        ( ReplState
            { _replContextEntryPoint = Nothing,
              _replStateGlobalOptions = globalOptions,
              _replStateInvokeDir = invokeDir
            }
        )
    )

loadEntryPoint :: EntryPoint -> Repl ()
loadEntryPoint ep = do
  State.modify
    ( set
        replContextEntryPoint
        (Just ep)
    )
  let epPath :: Maybe (Path Abs File) = ep ^. entryPointModulePaths . _headMaybe
  whenJust epPath $ \path -> do
    let filepath = toFilePath path
    liftIO (putStrLn . pack $ "OK loaded " <> filepath)
    content <- liftIO (readFile filepath)
    let evalRes =
          Geb.runEval $
            Geb.RunEvalArgs
              { _runEvalArgsContent = content,
                _runEvalArgsInputFile = path,
                _runEvalArgsEvaluatorOptions = Geb.defaultEvaluatorOptions
              }
    printEvalResult evalRes

reloadFile :: String -> Repl ()
reloadFile _ = do
  mentryPoint <- State.gets ((^. replContextEntryPoint))
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
  case Geb.runParser gebReplPath (pack gebMorphism) of
    Left err -> printError (JuvixError err)
    Right (Geb.ExpressionMorphism morphism) -> do
      case Geb.inferObject' morphism of
        Right obj -> renderOut (Geb.ppOut Geb.defaultEvaluatorOptions obj)
        Left err -> printError (JuvixError err)
    Right (Geb.ExpressionTypedMorphism _) ->
      checkTypedMorphism gebMorphism
    Right _ -> printError (error "Inference only works on Geb morphisms.")

checkTypedMorphism :: String -> Repl ()
checkTypedMorphism gebMorphism = Repline.dontCrash $ do
  case Geb.runParser gebReplPath (pack gebMorphism) of
    Left err -> printError (JuvixError err)
    Right (Geb.ExpressionTypedMorphism tyMorphism) -> do
      case run . runError @CheckingError $ Geb.check' tyMorphism of
        Right obj -> renderOut (Geb.ppOut Geb.defaultEvaluatorOptions obj)
        Left err -> printError (JuvixError err)
    Right _ -> printError (error "Checking only works on typed Geb morphisms.")

runReplCommand :: String -> Repl ()
runReplCommand input =
  Repline.dontCrash $
    do
      let evalRes =
            Geb.runEval $
              Geb.RunEvalArgs
                { _runEvalArgsContent = pack input,
                  _runEvalArgsInputFile = gebReplPath,
                  _runEvalArgsEvaluatorOptions = Geb.defaultEvaluatorOptions
                }
      printEvalResult evalRes

evalAndOutputMorphism :: String -> Repl ()
evalAndOutputMorphism input =
  Repline.dontCrash $ do
    let evalRes =
          Geb.runEval $
            Geb.RunEvalArgs
              { _runEvalArgsContent = pack input,
                _runEvalArgsInputFile = gebReplPath,
                _runEvalArgsEvaluatorOptions =
                  Geb.defaultEvaluatorOptions
                    { Geb._evaluatorOptionsOutputMorphism = True
                    }
              }
    printEvalResult evalRes

options :: ReplEntryPoint -> [(String, String -> Repl ())]
options replEntryPoint =
  [ ("help", Repline.dontCrash . helpText),
    -- `multiline` is included here for auto-completion purposes only.
    -- `repline`'s `multilineCommand` logic overrides this no-op.
    (multilineCmd, Repline.dontCrash . \_ -> return ()),
    ("check", checkTypedMorphism),
    ("load", Repline.dontCrash . loadFile replEntryPoint . pSomeFile),
    ("morphism", evalAndOutputMorphism),
    ("quit", quit),
    ("reload", Repline.dontCrash . reloadFile),
    ("root", printRoot),
    ("type", inferObject),
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

replAction :: GebReplOptions -> ReplEntryPoint -> ReplS ()
replAction replOpts replEntryPoint =
  Repline.evalReplOpts
    Repline.ReplOpts
      { prefix,
        multilineCommand,
        initialiser = initSession replOpts,
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

initSession :: GebReplOptions -> Repl ()
initSession replOpts
  | replOpts ^. gebReplOptionsSilent = return ()
  | otherwise = renderOut welcomeMsg

welcomeMsg :: ReplMessageDoc
welcomeMsg =
  ReplMessageDoc $
    P.annotate ReplIntro "Welcome to the Juvix Geb REPL!"
      <> P.line
      <> normal [i|Juvix v#{versionTag}: https://juvix.org.|]
      <> P.line
      <> normal "Type :help for help."
      <> P.line

replPromptText :: Repl String
replPromptText = do
  r <- replText . ReplMessageDoc $ P.annotate ReplPrompt "geb> "
  return (unpack r)

helpText :: String -> Repl ()
helpText _ =
  renderOutNormal
    [__i|
      EXPRESSION              Evaluate a Geb morphism
      :help                   Print this help
      :load FILE              Load a file into the REPL
      :reload                 Reload the currently loaded file
      :check EXPRESSION       Check the type of a Geb morphism
      :type EXPRESSION        Infer the type of a Geb morphism
      :morphism EXPRESSION    Read back after evaluate a Geb morphism
      :version                Display the Juvix version
      :multiline              Enter multiline mode
      :root                   Print the root directory of the REPL
      :version                Display the Juvix version
      :quit                   Exit the REPL
  |]

multilineCmd :: String
multilineCmd = "multiline"

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

endSession :: Repl Repline.ExitDecision
endSession = return Repline.Exit

printRoot :: String -> Repl ()
printRoot _ = do
  r <- State.gets (^. replStateInvokeDir)
  renderOutNormal $ pack (toFilePath r)

displayVersion :: String -> Repl ()
displayVersion _ = renderOutNormal versionTag

replMakeAbsolute :: SomeBase b -> Repl (Path Abs b)
replMakeAbsolute = \case
  Abs p -> return p
  Rel r -> do
    invokeDir <- State.gets (^. replStateInvokeDir)
    return (invokeDir <//> r)

replText :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl Text
replText t = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stdout)
  return $ P.toAnsiText (not (opts ^. globalNoColors) && hasAnsi) t

render' :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
render' t = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stdout)
  liftIO (P.renderIO (not (opts ^. globalNoColors) && hasAnsi) t)

renderOut :: (P.HasAnsiBackend a, P.HasTextBackend a) => a -> Repl ()
renderOut t = render' t >> liftIO (putStrLn "")

renderOutNormal :: Text -> Repl ()
renderOutNormal = renderOut . ReplMessageDoc . normal

printError :: JuvixError -> Repl ()
printError e = do
  opts <- State.gets (^. replStateGlobalOptions)
  hasAnsi <- liftIO (Ansi.hSupportsANSIColor stderr)
  let useAnsi = (not (opts ^. globalNoColors) && hasAnsi)
  errorText <-
    replText . ReplMessageDoc
      $ P.annotate
        ReplError
      $ pretty
        ( run
            . runReader (project' @GenericOptions opts)
            $ Error.render useAnsi False e
        )
  liftIO $ hPutStrLn stderr errorText

printEvalResult :: Either JuvixError Geb.RunEvalResult -> Repl ()
printEvalResult = \case
  Left err -> printError err
  Right (Geb.RunEvalResultGebValue v) ->
    renderOut (Geb.ppOut Geb.defaultEvaluatorOptions v)
  Right (Geb.RunEvalResultMorphism morphism) ->
    renderOut (Geb.ppOut Geb.defaultEvaluatorOptions morphism)
