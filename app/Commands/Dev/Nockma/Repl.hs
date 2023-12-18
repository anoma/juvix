module Commands.Dev.Nockma.Repl where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Juvix.Compiler.Nockma.Evaluator (NockEvalError, evalRepl)
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty (ppPrint)
import Juvix.Compiler.Nockma.Translation.FromSource (parseProgramFile, parseReplExpression, parseText)
import Juvix.Parser.Error
import System.Console.Haskeline
import System.Console.Repline qualified as Repline
import Text.Megaparsec (errorBundlePretty)

type ReplS = State.StateT ReplState IO

data ReplState = ReplState
  { _replStateProgram :: Maybe (Program Natural),
    _replStateStack :: Maybe (Term Natural)
  }

type Repl a = Repline.HaskelineT ReplS a

makeLenses ''ReplState

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

printStack :: String -> Repl ()
printStack _ = Repline.dontCrash $ do
  stack <- getStack
  case stack of
    Nothing -> noStackErr
    Just s -> liftIO (putStrLn (ppPrint s))

noStackErr :: a
noStackErr = error "no stack is set. Use :set-stack <TERM> to set a stack."

setStack :: String -> Repl ()
setStack s = Repline.dontCrash $ do
  newStack <- readTerm s
  State.modify (set replStateStack (Just newStack))

loadFile :: String -> Repl ()
loadFile s = Repline.dontCrash $ do
  prog <- readProgram s
  State.modify (set replStateProgram (Just prog))

options :: [(String, String -> Repl ())]
options =
  [ ("quit", quit),
    ("get-stack", printStack),
    ("set-stack", setStack),
    ("load", loadFile)
  ]

banner :: Repline.MultiLine -> Repl String
banner = \case
  Repline.MultiLine -> return "... "
  Repline.SingleLine -> return "nockma> "

-- getStack :: Repl (Maybe (Term Natural))
-- getStack = do
--   ms <- State.gets (^. replStateStack)
--   case ms of
--     Just s -> return s
--     Nothing -> error "no stack is set. Use :set-stack <TERM> to set a stack."

getStack :: Repl (Maybe (Term Natural))
getStack = State.gets (^. replStateStack)

getProgram :: Repl (Maybe (Program Natural))
getProgram = State.gets (^. replStateProgram)

readProgram :: FilePath -> Repl (Program Natural)
readProgram s = fromMegaParsecError <$> parseProgramFile s

fromMegaParsecError :: Either MegaparsecError a -> a
fromMegaParsecError = \case
  Left (MegaparsecError e) -> error (pack (errorBundlePretty e))
  Right a -> a

readTerm :: String -> Repl (Term Natural)
readTerm s = return (fromMegaParsecError (parseText (strip (pack s))))

readExpression :: String -> Repl (ReplExpression Natural)
readExpression s = return (fromMegaParsecError (parseReplExpression (strip (pack s))))

evalExpression :: ReplExpression Natural -> Repl (Term Natural)
evalExpression t = do
  s <- getStack
  prog <- getProgram
  let et =
        run
          . runError @(ErrNockNatural Natural)
          . runError @NockEvalError
          $ evalRepl prog s t
  case et of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (show e)
      Right res -> return res

replCommand :: String -> Repl ()
replCommand input = Repline.dontCrash $ do
  et <- readExpression input >>= evalExpression
  liftIO (putStrLn (ppPrint et))

replAction :: ReplS ()
replAction =
  Repline.evalReplOpts
    Repline.ReplOpts
      { prefix = Just ':',
        command = replCommand,
        initialiser = return (),
        finaliser = return Repline.Exit,
        multilineCommand = Just "multiline",
        tabComplete = Repline.Word (\_ -> return []),
        options,
        banner
      }

runCommand :: forall r. (Members '[Embed IO, App] r) => NockmaReplOptions -> Sem r ()
runCommand _ = embed . (`State.evalStateT` iniState) $ replAction
  where
    iniState :: ReplState
    iniState =
      ReplState
        { _replStateStack = Nothing,
          _replStateProgram = Nothing
        }
