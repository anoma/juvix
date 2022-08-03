module Juvix.Asm.Interpreter.Runtime where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Asm.Language

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
\* local temporary area
  - holds temporary values
  - referenced with TempRef
  - constant size (depends on the function)
  - write-once: it is an error to have to  write at the same offset in the
    temporary area within a single function invocation
  - Core.Let is compiled to store the value in the local temporary area
  - Core.Case is compiled to store the value in the local temporary area,
    to enable accessing constructor arguments in the branches
\* local value stack
  - holds temporary values
  - JuvixAsm instructions manipulate the current local value stack
  - one value stack per each function invocation, not shared among functions
    (or different invocations of the same function)
  - maximum constant height of a local value stack (depends on the function)
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

data TemporaryArea = TemporaryArea
  { _temporaryArea :: HashMap Offset Val,
    _temporaryAreaSize :: Int
  }

newtype ValueStack = ValueStack
  { _valueStack :: [Val]
  }

-- An activation frame contains the function-local memory (local argument area,
-- temporary area, value stack) for a single function invocation.
data Frame = Frame
  { _frameArgs :: ArgumentArea,
    _frameTemp :: TemporaryArea,
    _frameStack :: ValueStack
  }

emptyFrame :: Frame
emptyFrame =
  Frame
    { _frameArgs = ArgumentArea mempty 0,
      _frameTemp = TemporaryArea mempty 0,
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
  - Constructor data
  - Closure
-}

data Val
  = ValInteger Integer
  | ValBool Bool
  | ValConstr Constr
  | ValClosure Closure

data Constr = Constr
  { _constrTag :: Tag,
    _constrArgs :: [Val]
  }

data Closure = Closure
  { _closureSymbol :: Symbol,
    _closureArgs :: [Val]
  }

-- JuvixAsm runtime state
data RuntimeState = RuntimeState
  { _runtimeCallStack :: CallStack, -- global call stack
    _runtimeFrame :: Frame -- current frame
  }

makeLenses ''CallStack
makeLenses ''Continuation
makeLenses ''ArgumentArea
makeLenses ''TemporaryArea
makeLenses ''ValueStack
makeLenses ''Frame
makeLenses ''Constr
makeLenses ''Closure
makeLenses ''RuntimeState

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
  WriteTemp :: Offset -> Val -> Runtime m ()

makeSem ''Runtime

runRuntime :: forall r a. Sem (Runtime ': r) a -> Sem r (RuntimeState, a)
runRuntime = runState (RuntimeState (CallStack []) emptyFrame) . interp
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
          [] -> error "popping empty call stack"
      PushValueStack val ->
        modify' (over runtimeFrame (over frameStack (over valueStack (val :))))
      PopValueStack -> do
        s <- get
        case s ^. (runtimeFrame . frameStack . valueStack) of
          v : vs -> do
            modify' (over runtimeFrame (over frameStack (set valueStack vs)))
            return v
          [] -> error "popping empty value stack"
      TopValueStack -> do
        s <- get
        case s ^. (runtimeFrame . frameStack . valueStack) of
          v : _ -> return v
          [] -> error "accessing top of empty value stack"
      NullValueStack ->
        get >>= \s -> return $ null $ s ^. (runtimeFrame . frameStack . valueStack)
      ReplaceFrame frm ->
        modify' (set runtimeFrame frm)
      ReadArg off -> do
        s <- get
        return $
          fromMaybe
            (error "invalid argument area read")
            (HashMap.lookup off (s ^. (runtimeFrame . frameArgs . argumentArea)))
      ReadTemp off -> do
        s <- get
        return $
          fromMaybe
            (error "invalid temporary area read")
            (HashMap.lookup off (s ^. (runtimeFrame . frameTemp . temporaryArea)))
      WriteTemp off val ->
        modify'
          ( over
              runtimeFrame
              ( over
                  frameTemp
                  ( \(TemporaryArea a n) ->
                      if
                          | off < n && not (HashMap.member off a) ->
                              TemporaryArea (HashMap.insert off val a) n
                          | otherwise ->
                              error "invalid temporary area write"
                  )
              )
          )

evalRuntime :: forall r a. Sem (Runtime ': r) a -> Sem r a
evalRuntime = fmap snd . runRuntime
