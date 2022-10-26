{-# LANGUAGE QuasiQuotes #-}

module Commands.Repl where

import Commands.Base hiding (command)
import Commands.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Control.Monad.State.Strict qualified as State
import Data.HashMap.Strict qualified as HashMap
import Data.String.Interpolate (i, __i)
import Evaluator
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Error qualified as Core
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Pretty qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data as Core
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Extra.Version
import System.Console.Haskeline
import System.Console.Repline
import System.Console.Repline qualified as Repline
import Text.Megaparsec qualified as M

data ReplState = ReplState
  { _replStateRoot :: FilePath,
    _replStateContext :: Maybe (BuiltinsState, ExpressionContext)
  }

initReplState :: FilePath -> ReplState
initReplState root = ReplState root Nothing

rootEntryPoint :: FilePath -> FilePath -> EntryPoint
rootEntryPoint mainFile root = (defaultEntryPoint mainFile) {_entryPointRoot = root}

makeLenses ''ReplState

type ReplS = State.StateT ReplState IO

type Repl a = HaskelineT ReplS a

helpTxt :: MonadIO m => m ()
helpTxt =
  liftIO
    ( putStrLn
        [__i|
  Type any expression to evaluate it in the context of the currently loaded module or use one of the following commands:
  :help
         Print help text and describe options
  :load FILE
         Load a file into the REPL
  :type EXPRESSION
         Infer the type of an expression
  :core EXPRESSION
         Translate the expression to JuvixCore
  :idents
         List the identifiers in the environment
  :multiline
         Start a multi-line input. Submit with <Ctrl-D>
  :root
         Print the current project root
  :quit
         Exit the REPL
  |]
    )

noFileLoadedMsg :: MonadIO m => m ()
noFileLoadedMsg = liftIO (putStrLn "No file loaded. Load a file using the `:load FILE` command.")

welcomeMsg :: MonadIO m => m ()
welcomeMsg = liftIO (putStrLn [i|Juvix REPL version #{versionTag}: https://juvix.org. Run :help for help|])

printError :: MonadIO m => JuvixError -> m ()
printError e = liftIO (runM (runReader defaultGenericOptions (printErrorAnsiSafe e)))

runCommand :: Members '[Embed IO, App] r => ReplOptions -> Sem r ()
runCommand opts = do
  let printHelpTxt :: String -> Repl ()
      printHelpTxt _ = helpTxt

      multilineCmd :: String
      multilineCmd = "multiline"

      quit :: String -> Repl ()
      quit _ = liftIO (throwIO Interrupt)

      loadFile :: String -> Repl ()
      loadFile args = do
        r <- State.gets (^. replStateRoot)
        let f = unpack (strip (pack args))
            entryPoint = rootEntryPoint f r
        tab <- liftIO (runIO' iniState entryPoint upToCore)
        State.modify (set replStateContext (Just (second expressionContext tab)))
        liftIO (putStrLn [i|OK loaded: #{f}|])

      listIdentifiers :: String -> Repl ()
      listIdentifiers _ = do
        ctx <- State.gets (^. replStateContext)
        case ctx of
          Just res -> do
            let identMap = res ^. _2 . contextCoreResult . Core.coreResultTable . Core.identMap
            liftIO $ forM_ (HashMap.keys identMap) putStrLn
          Nothing -> noFileLoadedMsg

      printRoot :: String -> Repl ()
      printRoot _ = do
        r <- State.gets (^. replStateRoot)
        liftIO $ putStrLn (pack r)

      command :: String -> Repl ()
      command input = Repline.dontCrash $ do
        ctx <- State.gets (^. replStateContext)
        case ctx of
          Just ctx' -> do
            evalRes <- compileThenEval ctx' input
            case evalRes of
              Left err -> printError err
              Right n -> liftIO (putStrLn (Core.ppPrint n))
          Nothing -> noFileLoadedMsg
        where
          defaultLoc :: Interval
          defaultLoc = singletonInterval (mkLoc 0 (M.initialPos ""))

          compileThenEval :: (BuiltinsState, ExpressionContext) -> String -> Repl (Either JuvixError Core.Node)
          compileThenEval (bs, ctx) s = bindEither compileString eval
            where
              eval :: Core.Node -> Repl (Either JuvixError Core.Node)
              eval n =
                liftIO $
                  mapLeft
                    (JuvixError @Core.CoreError)
                    <$> doEvalIO True defaultLoc (ctx ^. contextCoreResult . Core.coreResultTable) n

              compileString :: Repl (Either JuvixError Core.Node)
              compileString = liftIO $ compileExpressionIO "" ctx bs (pack s)

              bindEither :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
              bindEither x f = join <$> (x >>= mapM f)

      core :: String -> Repl ()
      core input = Repline.dontCrash $ do
        ctx <- State.gets (^. replStateContext)
        case ctx of
          Just (bs, res) -> do
            compileRes <- liftIO (compileExpressionIO "" res bs (pack input))
            case compileRes of
              Left err -> printError err
              Right n -> liftIO (putStrLn (Core.ppPrint n))
          Nothing -> noFileLoadedMsg

      inferType :: String -> Repl ()
      inferType input = Repline.dontCrash $ do
        ctx <- State.gets (^. replStateContext)
        case ctx of
          Just (bs, res) -> do
            compileRes <- liftIO (inferExpressionIO "" res bs (pack input))
            case compileRes of
              Left err -> printError err
              Right n -> liftIO (putStrLn (Internal.ppPrint n))
          Nothing -> noFileLoadedMsg

      options :: [(String, String -> Repl ())]
      options =
        [ ("help", Repline.dontCrash . printHelpTxt),
          -- `multiline` is included here for auto-completion purposes only.
          -- `repline`'s `multilineCommand` logic overrides this no-op.
          (multilineCmd, Repline.dontCrash . \_ -> return ()),
          ("quit", quit),
          ("load", Repline.dontCrash . loadFile),
          ("root", printRoot),
          ("idents", listIdentifiers),
          ("type", inferType),
          ("core", core)
        ]

      defaultMatcher :: [(String, CompletionFunc ReplS)]
      defaultMatcher = [(":load", fileCompleter)]

      optsCompleter :: WordCompleter ReplS
      optsCompleter n = do
        let names = (":" <>) . fst <$> options
        return (filter (isPrefixOf n) names)

      banner :: MultiLine -> Repl String
      banner = \case
        MultiLine -> return "... "
        SingleLine -> return "juvix> "

      prefix :: Maybe Char
      prefix = Just ':'

      multilineCommand :: Maybe String
      multilineCommand = Just multilineCmd

      initialiser :: Repl ()
      initialiser = do
        welcomeMsg
        whenJust ((^. pathPath) <$> (opts ^. replInputFile)) loadFile

      finaliser :: Repl ExitDecision
      finaliser = return Exit

      tabComplete :: CompleterStyle ReplS
      tabComplete = Prefix (wordCompleter optsCompleter) defaultMatcher

      replAction :: ReplS ()
      replAction = evalReplOpts ReplOpts {..}

  root <- askRoot
  embed (State.evalStateT replAction (initReplState root))
