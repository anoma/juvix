module Commands.Repl where

import Commands.Base hiding (command)
import Commands.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Control.Monad.State.Strict qualified as State
import Evaluator
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Translation
import System.Console.Haskeline
import System.Console.Repline
import System.Console.Repline qualified as Repline
import Juvix.Compiler.Core.Translation.FromInternal.Data as Core
import Juvix.Compiler.Pipeline
import Data.HashMap.Strict qualified as HashMap
import qualified Text.Megaparsec as M


data ReplState = ReplState
  { _replStateRoot :: FilePath,
    _replStateInfoTable :: Core.InfoTable
  }

initReplState :: FilePath -> ReplState
initReplState root = ReplState root Core.emptyInfoTable

rootEntryPoint :: FilePath -> FilePath -> EntryPoint
rootEntryPoint mainFile root = (defaultEntryPoint mainFile) { _entryPointRoot = root }

makeLenses ''ReplState

type ReplS = State.StateT ReplState IO

type ReplT a = HaskelineT ReplS a

runCommand :: Members '[Embed IO, App] r => ReplOptions -> Sem r ()
runCommand opts = do
  let helpTxt :: String -> ReplT ()
      helpTxt _ =
        liftIO
          ( putStrLn
              "Use one of the following commands:\n\
              \:help\n\
              \       Print help text and describe options\n\
              \:multiline\n\
              \       Start a multi-line input. Submit with <Ctrl-D>\n\
              \:root\n\
              \       Print the current project root\n\
              \:load\n\
              \       Load a file into the REPL\n\
              \:quit\n\
              \       Exit the REPL"
          )

      multilineCmd :: String
      multilineCmd = "multiline"

      quit :: String -> ReplT ()
      quit _ = liftIO (throwIO Interrupt)

      loadFile :: String -> ReplT ()
      loadFile args = do
        r <- State.gets (^. replStateRoot)
        let f = unpack (strip (pack args))
            entryPoint = rootEntryPoint f r
        tab <- liftIO ((^. Core.coreResultTable) <$> runIO' entryPoint upToCore)
        State.modify (set replStateInfoTable tab)

      listIdentifiers :: String -> ReplT ()
      listIdentifiers _ = do
        ctx <- State.gets (^. replStateInfoTable . Core.identMap)
        liftIO $ forM_ (HashMap.keys ctx) putStrLn

      -- eval :: String -> ReplT ()
      -- eval s = do
      --   tab <- State.gets (^. replStateInfoTable)
      --   identMap <- State.gets (^. replStateInfoTable . Core.identMap)
      --   ctx <- State.gets (^. replStateInfoTable . Core.identContext)
      --   let k = identMap HashMap.!? pack s
      --   case k of
      --     (Just (Core.IdentSym s)) -> do
      --       let (Just n) = ctx HashMap.!? s
      --       undefined

      --     _ -> error "sym not found"
      --   where
      --     defaultLoc = singletonInterval (mkLoc "stdin" 0 (M.initialPos "stdin"))


      printRoot :: String -> ReplT ()
      printRoot _ = do
        r <- State.gets (^. replStateRoot)
        liftIO $ putStrLn (pack r)

      command :: String -> ReplT ()
      command input = liftIO (putStrLn (pack input))

      options :: [(String, String -> ReplT ())]
      options =
        [ ("help", Repline.dontCrash . helpTxt),
          -- `multiline` is included here for auto-completion purposes only.
          -- `repline`'s `multilineCommand` logic overrides this no-op.
          (multilineCmd, Repline.dontCrash . \_ -> return ()),
          ("quit", quit),
          ("load", loadFile),
          ("root", printRoot),
          ("idents", listIdentifiers)
        ]

      defaultMatcher :: [(String, CompletionFunc ReplS)]
      defaultMatcher = [(":load", fileCompleter)]

      optsCompleter :: WordCompleter ReplS
      optsCompleter n = do
        let names = (":" <>) . fst <$> options
        return (filter (isPrefixOf n) names)

      banner :: MultiLine -> ReplT String
      banner = \case
        MultiLine -> return "... "
        SingleLine -> return ">>> "

      prefix :: Maybe Char
      prefix = Just ':'

      multilineCommand :: Maybe String
      multilineCommand = Just multilineCmd

      initialiser :: ReplT ()
      initialiser = return ()

      finaliser :: ReplT ExitDecision
      finaliser = return Exit

      tabComplete :: CompleterStyle ReplS
      tabComplete = Prefix (wordCompleter optsCompleter) defaultMatcher

      replAction :: ReplS ()
      replAction = evalReplOpts ReplOpts {..}

  root <- askRoot
  embed (State.evalStateT replAction (initReplState root))
