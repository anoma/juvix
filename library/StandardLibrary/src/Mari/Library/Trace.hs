{-# LANGUAGE TemplateHaskell #-}

-- | The Trace library represents the ability to properly trace code
-- throughout Haskell.
--
-- - The structure requires your code to exist in the =Trace.Eff=
--   effect.
-- - The =Eff= Variant of functions are over the =Trace.Eff= effect
--   rather than the =Trace.T= type itself.
--   + It is just as valid to use the non =Eff= version of the
--     functions
module Mari.Library.Trace
  ( module Mari.Library.Trace.Types,

    -- * Core API
    withScope,
    info,
    stackTrace,
    finishedAtPoint,
    fullTrace,
    break,

    -- ** Trace Enabling/Disabling
    enableEff,
    disableEff,
    disableRecursiveEff,
    enable,
    disable,
    disableRecursive,
    traceAll,
    traceAllEff,

    -- ** Initialization
    empty,

    -- ** Trace Level Setup
    setLevel,

    -- * Functions for testing
    traceInfo,
    traceStmt,
  )
where

import Control.Lens (over, set, (^.))
import Mari.Library hiding (break, empty)
import qualified Mari.Library.HashMap as HashMap
import qualified Mari.Library.NameSymbol as NameSymbol
import qualified Mari.Library.Trace.Format as Format
import Mari.Library.Trace.Types hiding (enable)
import qualified Mari.Library.Trace.Types as Lens (enable)

--------------------------------------------------------------------------------
-- Core API
--------------------------------------------------------------------------------

withScope :: (Eff m, Show b) => NameSymbol.T -> [Text] -> m b -> m b
withScope name args f = do
  let newStack =
        Stack
          { stackName = name,
            stackStart = args,
            stackBetween = [],
            stackOutput = Nothing
          }
  modify @"trace" (`startScope` newStack)
  ret <- f
  modify @"trace" (finishScope . (`registerOutput` show ret))
  pure ret

-- | @stackTrace@ dumps the current Stack Trace, Ignoring any disabling behavior
stackTrace :: (Eff m, MonadIO m) => m ()
stackTrace = do
  trace <- get @"trace"
  putStrLn (traceStmt trace)

-- | @finishedAtPoint@ dumps the finished trace calls at point. like
-- @stackTrace@ the printed out terms are done immediately, rather
-- than being introspected on after computation has finished/aborted.
finishedAtPoint :: (Eff m, MonadIO m) => m ()
finishedAtPoint = do
  trace <- get @"trace"
  fullTrace trace

-- | @empty@ is the empty stack trace with no functions enabled
empty :: T
empty = T Empty [] mempty Nothing

-- | @info@ is called on the computational trace, if we exit early,
-- then we dump the stack trace, if computation finishes we then trace
-- all the finished calls. @info@ can be called with @fullTrace@ when
-- computation ends early to have a good idea of the computation stack
-- by the point of failure.
info :: MonadIO m => T -> m ()
info = putStrLn . traceInfo

-- | @break@ dumps the stack by throwing an exception and returning the
-- call stack to that point
break :: (Eff m, HasThrow "error" Error m) => m b
break = get @"trace" >>= throw @"error" . Error

-- | @fullTrace@ dumps the finished calls from the Trace
fullTrace :: MonadIO m => T -> m ()
fullTrace = putStrLn . traceFullStack

-- | @enableEff@ allows you to enable more traced functions while
-- in the middle of effectful computations, rather than before or after
-- having a complete trace.
enableEff :: (Eff m, Traversable f) => f NameSymbol.T -> m ()
enableEff xs = modify @"trace" (`enable` xs)

-- | @disableEff@ see @enableEff@ except this removes the current
-- call from being traced (default behavior)
disableEff :: (Eff m, Traversable f) => f NameSymbol.T -> m ()
disableEff xs = modify @"trace" (`disable` xs)

-- | @disableRecursiveEff@ see @enableEff@ except this
-- removes any recursive call of the trace
disableRecursiveEff :: (Eff m, Traversable f) => f NameSymbol.T -> m ()
disableRecursiveEff xs = modify @"trace" (`disableRecursive` xs)

-- | @traceAllEff@ enables tracing for every single function that has
-- been called. Warning this is quite a lot of functions
traceAllEff :: Eff m => m ()
traceAllEff = modify @"trace" traceAll

-- | @traceAll@ enables tracing for every single function that has
-- been called. Warning this is quite a lot of functions
traceAll :: T -> T
traceAll =
  set debugLevel (Just 10000)

-- | @enable@ allows you to enable traces before or after
-- commencing a trace.
enable :: Traversable f => T -> f NameSymbol.T -> T
enable t =
  overEnabled t (set Lens.enable Enabled)

-- | @disable@ see @enable@ except this removes the current
-- call from being traced (default behavior)
disable :: Traversable f => T -> f NameSymbol.T -> T
disable t =
  overEnabled t (set Lens.enable Disabled)

-- | @disableRecursive@ see @enable@ except this
-- removes any recursive call of the trace
disableRecursive :: Traversable f => T -> f NameSymbol.T -> T
disableRecursive t =
  overEnabled t (set Lens.enable DisableRecursive)

-- | @setLevel@ sets the trace
setLevel :: Traversable f => T -> Natural -> f NameSymbol.T -> T
setLevel t lev =
  overEnabled t (set level lev)

--------------------------------------------------------------------------------
-- Helper Functionality
--------------------------------------------------------------------------------

overEnabled ::
  Traversable f => T -> (MetaInfo -> MetaInfo) -> f NameSymbol.T -> T
overEnabled t f xs =
  over enabled (\enabled -> foldr (HashMap.alter fDefault) enabled xs) t
  where
    fDefault Nothing = Just (f (MetaInfo Disabled defaultTrace))
    fDefault (Just m) = Just (f m)

-- Put in mitigation logic that either dumps the current stack if we
-- are corrupted or the logs if we finish
traceInfo :: T -> [Char]
traceInfo t =
  case t ^. current of
    Empty -> traceFullStack t
    StackChain {} -> traceStmt t

traceFullStack :: T -> [Char]
traceFullStack t =
  Format.fullTrace t (+ 2)
    |> intersperse "\n"
    |> fold

traceStmt :: HasCurrent s StackChain => s -> [Char]
traceStmt trace =
  "Stack Trace:" :
  Format.currentStackChain (trace ^. current) (const 1)
    |> intersperse "\n"
    |> fold

registerOutput :: T -> Text -> T
registerOutput t result =
  case t ^. current of
    Empty -> t
    StackChain a stack ->
      set current (StackChain a (set output (Just result) stack)) t

startScope :: T -> Stack -> T
startScope t stack =
  over current (`StackChain` stack) t

finishScope :: T -> T
finishScope t =
  case t ^. current of
    Empty -> t
    StackChain Empty stack ->
      t |> over traces (<> [stack]) |> set current Empty
    StackChain (StackChain grandParent parent) stack ->
      set current (StackChain grandParent (consLog stack parent)) t

consLog :: HasBetween t [a] => a -> t -> t
consLog currentStack =
  over between (<> [currentStack])
