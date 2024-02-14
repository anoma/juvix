module Commands.Dev.Nockma.Repl where

import Commands.Base hiding (Atom)
import Commands.Dev.Nockma.Repl.Options
import Control.Exception (throwIO)
import Control.Monad.State.Strict qualified as State
import Data.String.Interpolate (__i)
import Juvix.Compiler.Nockma.Evaluator (NockEvalError, evalRepl, fromReplTerm, programAssignments)
import Juvix.Compiler.Nockma.Evaluator.Options
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromSource (parseProgramFile, parseReplStatement, parseReplText, parseText)
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma
import Juvix.Parser.Error
import Juvix.Prelude qualified as Prelude
import System.Console.Haskeline
import System.Console.Repline qualified as Repline
import Prelude (read)

type ReplS = State.StateT ReplState IO

data ReplState = ReplState
  { _replStateProgram :: Maybe (Program Natural),
    _replStateStack :: Maybe (Term Natural),
    _replStateLoadedFile :: Maybe (Prelude.Path Abs File)
  }

type Repl a = Repline.HaskelineT ReplS a

makeLenses ''ReplState

printHelpTxt :: Repl ()
printHelpTxt = liftIO $ putStrLn helpTxt
  where
    helpTxt :: Text =
      [__i|
  EXPRESSION                      Evaluate a Nockma expression in the context of the current stack
  STACK_EXPRESSION / EXPRESSION   Evaluate a Nockma EXPRESSION in the context of STACK_EXPRESSION
  :load FILE                      Load a file containing Nockma assignments
  :reload                         Reload the current file
  :help                           Print help text and describe options
  :set-stack EXPRESSION           Set the current stack
  :get-stack                      Print the current stack
  :dir       NATURAL              Convert a natural number representing a position into a sequence of L and Rs. S means the empty sequence
  :quit                           Exit the REPL
          |]

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
  newStack <- readReplTerm s
  State.modify (set replStateStack (Just newStack))

loadFile :: Prelude.Path Abs File -> Repl ()
loadFile s = Repline.dontCrash $ do
  State.modify (set replStateLoadedFile (Just s))
  prog <- readProgram s
  State.modify (set replStateProgram (Just prog))

reloadFile :: Repl ()
reloadFile = Repline.dontCrash $ do
  fp <- State.gets (^. replStateLoadedFile)
  case fp of
    Nothing -> error "no file loaded"
    Just f -> do
      prog <- readProgram f
      State.modify (set replStateProgram (Just prog))

options :: [(String, String -> Repl ())]
options =
  [ ("quit", quit),
    ("get-stack", printStack),
    ("set-stack", setStack),
    ("load", loadFile . Prelude.absFile),
    ("reload", const reloadFile),
    ("dir", direction'),
    ("help", const printHelpTxt)
  ]

banner :: Repline.MultiLine -> Repl String
banner = \case
  Repline.MultiLine -> return "... "
  Repline.SingleLine -> return "nockma> "

getStack :: Repl (Maybe (Term Natural))
getStack = State.gets (^. replStateStack)

getProgram :: Repl (Maybe (Program Natural))
getProgram = State.gets (^. replStateProgram)

readProgram :: Prelude.Path Abs File -> Repl (Program Natural)
readProgram s = fromMegaParsecError <$> parseProgramFile s

direction' :: String -> Repl ()
direction' s = Repline.dontCrash $ do
  let n = read s :: Natural
      p = run (runFailDefault (error "invalid position") (decodePath (EncodedPath n)))
  liftIO (putStrLn (ppPrint p))

readTerm :: String -> Repl (Term Natural)
readTerm = return . fromMegaParsecError . parseText . strip . pack

readReplTerm :: String -> Repl (Term Natural)
readReplTerm s = do
  mprog <- getProgram
  let t =
        run
          . runError @(NockEvalError Natural)
          . fromReplTerm (programAssignments mprog)
          . fromMegaParsecError
          . parseReplText
          $ strip (pack s)
  case t of
    Left e -> error (ppTrace e)
    Right tv -> return tv

readStatement :: String -> Repl (ReplStatement Natural)
readStatement s = return (fromMegaParsecError (parseReplStatement (strip (pack s))))

evalStatement :: ReplStatement Natural -> Repl ()
evalStatement = \case
  ReplStatementAssignment as -> do
    prog <- fromMaybe (Program []) <$> getProgram
    let p' = over programStatements (++ [StatementAssignment as]) prog
    State.modify (set replStateProgram (Just p'))
  ReplStatementExpression t -> do
    s <- getStack
    prog <- getProgram
    et <-
      liftIO
        . runM
        . runReader defaultEvalOptions
        . runError @(ErrNockNatural Natural)
        . runError @(NockEvalError Natural)
        $ evalRepl (putStrLn . Nockma.ppTrace) prog s t
    case et of
      Left e -> error (show e)
      Right ev -> case ev of
        Left e -> error (ppTrace e)
        Right res -> liftIO (putStrLn (ppPrint res))

replCommand :: String -> Repl ()
replCommand input_ = Repline.dontCrash $ do
  readStatement input_ >>= evalStatement

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

runCommand :: forall r. (Members '[EmbedIO, App] r) => NockmaReplOptions -> Sem r ()
runCommand opts = do
  mt :: Maybe (Term Natural) <- mapM iniStack (opts ^. nockmaReplOptionsStackFile)
  embed . (`State.evalStateT` (iniState mt)) $ replAction
  where
    iniStack :: AppPath File -> Sem r (Term Natural)
    iniStack af = do
      afile <- fromAppPathFile af
      parsedTerm <- Nockma.parseTermFile (toFilePath afile)
      case parsedTerm of
        Left err -> exitJuvixError (JuvixError err)
        Right t -> return t

    iniState :: Maybe (Term Natural) -> ReplState
    iniState mt =
      ReplState
        { _replStateStack = mt,
          _replStateProgram = Nothing,
          _replStateLoadedFile = Nothing
        }
