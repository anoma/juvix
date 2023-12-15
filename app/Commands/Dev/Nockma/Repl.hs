module Commands.Dev.Nockma.Repl where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Juvix.Compiler.Nockma.Evaluator (NockEvalError, eval)
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty (ppPrint)
import Juvix.Compiler.Nockma.Translation.FromSource (parseText)
import Juvix.Parser.Error
import System.Console.Haskeline
import System.Console.Repline qualified as Repline

type ReplS = State.StateT ReplState IO

newtype ReplState = ReplState
  {_replStateStack :: Maybe (Term Natural)}

type Repl a = Repline.HaskelineT ReplS a

makeLenses ''ReplState

quit :: String -> Repl ()
quit _ = liftIO (throwIO Interrupt)

printStack :: String -> Repl ()
printStack _ = Repline.dontCrash $ do
  stack <- getStack
  liftIO (putStrLn (ppPrint stack))

setStack :: String -> Repl ()
setStack s = Repline.dontCrash $ do
  newStack <- readTerm s
  State.modify (set replStateStack (Just newStack))

options :: [(String, String -> Repl ())]
options = [("quit", quit), ("get-stack", printStack), ("set-stack", setStack)]

banner :: Repline.MultiLine -> Repl String
banner = \case
  Repline.MultiLine -> return "... "
  Repline.SingleLine -> return "nockma> "

getStack :: Repl (Term Natural)
getStack = do
  ms <- State.gets (^. replStateStack)
  case ms of
    Just s -> return s
    Nothing -> error "no stack is set. Use :set-stack <TERM> to set a stack."

readTerm :: String -> Repl (Term Natural)
readTerm s = do
  let p = parseText (strip (pack s))
  case p of
    Left (e :: MegaparsecError) -> error (show e)
    Right t -> return t

evalTerm :: Term Natural -> Repl (Term Natural)
evalTerm t = do
  s <- getStack
  let et = run (runError @(ErrNockNatural Natural) (runError @NockEvalError (eval s t)))
  case et of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (show e)
      Right res -> return res

replCommand :: String -> Repl ()
replCommand input = Repline.dontCrash $ do
  t <- readTerm input
  et <- evalTerm t
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
runCommand _ = embed . (`State.evalStateT` (ReplState {_replStateStack = Nothing})) $ replAction
