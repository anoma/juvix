module Juvix.Compiler.Asm.Interpreter.Runtime
  ( module Juvix.Compiler.Asm.Interpreter.Runtime,
    module Juvix.Compiler.Asm.Interpreter.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Debug.Trace qualified as Debug
import GHC.Base qualified as GHC
import Juvix.Compiler.Asm.Data.Stack (Stack)
import Juvix.Compiler.Asm.Data.Stack qualified as Stack
import Juvix.Compiler.Asm.Interpreter.Error
import Juvix.Compiler.Asm.Language

{-
Memory consists of:
\* global heap
  - holds constructor data
  - shared between functions
  - referenced with ConstrRef
  - unbounded (theoretically)
\* global call stack
  - holds return addresses for recursive function invocations
  - manipulated by the Call and Return instructions (accessible only
    implicitly through these)
  - unbounded (theoretically)
\* local argument area
  - holds function arguments
  - local to one function invocation (activation frame)
  - referenced with ArgRef
  - constant number of arguments (depends on the function)
\* local temporary stack
  - holds temporary values
  - referenced with TempRef
  - constant maximum height (depends on the function)
  - current height of the local temporary stack is known at compile-time for each
    intruction; a program violating this assumption (e.g. by having a `Branch`
    instruction whose two branches result in different stack heights) is
    errorneous
  - compiled to a constant number of local variables / registers
  - Core.Let is compiled to store the value in the local temporary area
  - Core.Case is compiled to store the value in the local temporary area,
    to enable accessing constructor arguments in the branches
\* local value stack
  - holds temporary values
  - JuvixAsm instructions manipulate the current local value stack
  - one value stack per each function invocation, not shared among functions
    (or different invocations of the same function)
  - maximum constant height of a local value stack (depends on the function)
  - current height of the local value stack is known at compile-time for each
    intruction; a program violating this assumption is errorneous
  - compiled to a constant number of local variables / registers (unless the
    target IR itself is a stack machine)
-}

-- The heap does not need to be modelled explicitly. Heap values are simply
-- stored in the `Val` datastructure. Pointers are implicit.

newtype CallStack = CallStack
  { _callStack :: [Continuation]
  }

data ArgumentArea = ArgumentArea
  { _argumentArea :: HashMap Offset Val,
    _argumentAreaSize :: Int
  }

newtype TemporaryStack = TemporaryStack {_temporaryStack :: Stack Val}

newtype ValueStack = ValueStack
  { _valueStack :: [Val]
  }

-- | An activation frame contains the function-local memory (local argument area,
-- temporary stack, value stack) for a single function invocation.
data Frame = Frame
  { _frameArgs :: ArgumentArea,
    _frameTemp :: TemporaryStack,
    _frameStack :: ValueStack
  }

emptyFrame :: Frame
emptyFrame =
  Frame
    { _frameArgs = ArgumentArea mempty 0,
      _frameTemp = TemporaryStack Stack.empty,
      _frameStack = ValueStack []
    }

data Continuation = Continuation
  { _contFrame :: Frame,
    _contCode :: Code
  }

{-
  The following types of values may be stored in the heap or an activation
  frame.

  - Integer (arbitrary precision)
  - Boolean
  - String
  - Constructor data
  - Closure
-}

data Val
  = ValInteger Integer
  | ValBool Bool
  | ValString Text
  | ValUnit Unit
  | ValConstr Constr
  | ValClosure Closure
  deriving stock (Eq)

newtype Unit = Unit
  { _unitDisplay :: Bool
  }
  deriving stock (Eq)

data Constr = Constr
  { _constrTag :: Tag,
    _constrArgs :: [Val]
  }
  deriving stock (Eq)

data Closure = Closure
  { _closureSymbol :: Symbol,
    _closureArgs :: [Val]
  }
  deriving stock (Eq)

-- | JuvixAsm runtime state
data RuntimeState = RuntimeState
  { -- | global call stack
    _runtimeCallStack :: CallStack,
    -- | current frame
    _runtimeFrame :: Frame,
    -- | debug messages generated so far
    _runtimeMessages :: [Text]
  }

makeLenses ''CallStack
makeLenses ''Continuation
makeLenses ''ArgumentArea
makeLenses ''TemporaryStack
makeLenses ''ValueStack
makeLenses ''Frame
makeLenses ''Constr
makeLenses ''Closure
makeLenses ''RuntimeState

instance HasAtomicity Constr where
  atomicity Constr {..} =
    if
        | null _constrArgs -> Atom
        | otherwise -> Aggregate appFixity

instance HasAtomicity Closure where
  atomicity Closure {..} =
    if
        | null _closureArgs -> Atom
        | otherwise -> Aggregate appFixity

instance HasAtomicity Val where
  atomicity = \case
    ValInteger {} -> Atom
    ValBool {} -> Atom
    ValString {} -> Atom
    ValUnit {} -> Atom
    ValConstr c -> atomicity c
    ValClosure cl -> atomicity cl

data Runtime m a where
  HasCaller :: Runtime m Bool -- is the call stack non-empty?
  PushCallStack :: Code -> Runtime m ()
  PopCallStack :: Runtime m Continuation
  PushValueStack :: Val -> Runtime m ()
  PopValueStack :: Runtime m Val
  TopValueStack :: Runtime m Val
  NullValueStack :: Runtime m Bool
  ReplaceFrame :: Frame -> Runtime m ()
  ReadArg :: Offset -> Runtime m Val
  ReadTemp :: Offset -> Runtime m Val
  PushTempStack :: Val -> Runtime m ()
  PopTempStack :: Runtime m ()
  LogMessage :: Text -> Runtime m ()
  GetLogs :: Runtime m [Text]
  RuntimeError :: Text -> Runtime m a

makeSem ''Runtime

runRuntime :: forall r a. Sem (Runtime ': r) a -> Sem r (RuntimeState, a)
runRuntime = runState (RuntimeState (CallStack []) emptyFrame []) . interp
  where
    interp :: Sem (Runtime ': r) a -> Sem (State RuntimeState ': r) a
    interp = reinterpret $ \case
      HasCaller ->
        get >>= \s -> return (not (null (s ^. (runtimeCallStack . callStack))))
      PushCallStack code -> do
        s <- get
        let frm = s ^. runtimeFrame
        modify' (over runtimeCallStack (over callStack (Continuation frm code :)))
      PopCallStack -> do
        s <- get
        case s ^. (runtimeCallStack . callStack) of
          h : t -> do
            modify' (over runtimeCallStack (set callStack t))
            return h
          [] -> throwRuntimeError s "popping empty call stack"
      PushValueStack val ->
        modify' (over runtimeFrame (over frameStack (over valueStack (val :))))
      PopValueStack -> do
        s <- get
        case s ^. (runtimeFrame . frameStack . valueStack) of
          v : vs -> do
            modify' (over runtimeFrame (over frameStack (set valueStack vs)))
            return v
          [] -> throwRuntimeError s "popping empty value stack"
      TopValueStack -> do
        s <- get
        case s ^. (runtimeFrame . frameStack . valueStack) of
          v : _ -> return v
          [] -> throwRuntimeError s "accessing top of empty value stack"
      NullValueStack ->
        get >>= \s -> return $ null $ s ^. (runtimeFrame . frameStack . valueStack)
      ReplaceFrame frm ->
        modify' (set runtimeFrame frm)
      ReadArg off -> do
        s <- get
        return $
          fromMaybe
            (throwRuntimeError s "invalid argument area read")
            (HashMap.lookup off (s ^. (runtimeFrame . frameArgs . argumentArea)))
      ReadTemp off -> do
        s <- get
        return $
          fromMaybe
            (throwRuntimeError s "invalid temporary stack read")
            (Stack.nthFromBottom off (s ^. (runtimeFrame . frameTemp . temporaryStack)))
      PushTempStack val ->
        modify' (over runtimeFrame (over frameTemp (over temporaryStack (Stack.push val))))
      PopTempStack ->
        modify' (over runtimeFrame (over frameTemp (over temporaryStack Stack.pop)))
      LogMessage msg ->
        modify' (over runtimeMessages (msg :))
      GetLogs -> do
        s <- get
        return $ reverse (s ^. runtimeMessages)
      RuntimeError msg -> do
        s <- get
        throwRuntimeError s msg

    throwRuntimeError :: forall b. RuntimeState -> Text -> b
    throwRuntimeError s msg =
      let logs = reverse (s ^. runtimeMessages)
       in map (\x -> Debug.trace (fromText x) ()) logs `GHC.seq`
            throwRunError msg -- TODO: print stacktrace

hEvalRuntime :: forall r a. Member (Embed IO) r => Handle -> Sem (Runtime ': r) a -> Sem r a
hEvalRuntime h r = do
  (s, a) <- runRuntime r
  mapM_ (embed . hPutStrLn h) (reverse (s ^. runtimeMessages))
  return a

evalRuntime :: forall r a. Member (Embed IO) r => Sem (Runtime ': r) a -> Sem r a
evalRuntime = hEvalRuntime stdout
