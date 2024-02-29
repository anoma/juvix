module Juvix.Prelude.Effects.Base
  ( module Juvix.Prelude.Effects.Base,
    module Effectful,
    module Effectful.Reader.Static,
    module Effectful.State.Static.Local,
    module Effectful.Error.Static,
    module Effectful.Dispatch.Dynamic,
    module Effectful.TH,
    module Effectful.Dispatch.Static,
    module Effectful.Provider,
    module Juvix.Prelude.Effects.Base.Internal,
  )
where

import Data.Kind qualified as GHC
import Effectful hiding (Eff, (:>))
import Effectful qualified as E
import Effectful.Dispatch.Dynamic (LocalEnv, SharedSuffix, impose, interpose, localLift, localLiftUnlift, localLiftUnliftIO, localSeqLift, localSeqUnlift, localSeqUnliftIO, localUnlift, localUnliftIO, withLiftMap, withLiftMapIO)
import Effectful.Dispatch.Dynamic qualified as E
import Effectful.Dispatch.Static
import Effectful.Error.Static hiding (runError)
import Effectful.Internal.Env (getEnv, putEnv)
import Effectful.Provider
import Effectful.Reader.Static
import Effectful.State.Static.Local hiding (runState, state)
import Effectful.State.Static.Local qualified as State
import Effectful.TH
import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base.Internal
import Language.Haskell.TH.Syntax qualified as GHC

type Sem = E.Eff

type EmbedIO = IOE

type Member (e :: Effect) (r :: [Effect]) = e E.:> r

-- | First order effect handler
type EffectHandlerFO (e :: Effect) (r :: [Effect]) =
  forall a localEs.
  (HasCallStack, Member e localEs) =>
  e (Sem localEs) a ->
  Sem r a

-- | Type signature of the effect handler.
type EffectHandler e es =
  forall a localEs.
  (HasCallStack, Member e localEs) =>
  -- | Capture of the local environment for handling local 'Eff' computations
  -- when @e@ is a higher order effect.
  LocalEnv localEs es ->
  -- | The effect performed in the local environment.
  e (Sem localEs) a ->
  Sem es a

type Members :: [Effect] -> [Effect] -> GHC.Constraint
type family Members es r where
  Members '[] _ = ()
  Members (e ': es) r = (Member e r, Members es r)

makeSem :: GHC.Name -> Q [GHC.Dec]
makeSem = makeEffect

overStaticRep ::
  forall e r sideEffects.
  ( DispatchOf e ~ 'Static sideEffects,
    Member e r
  ) =>
  (StaticRep e -> StaticRep e) ->
  Sem r ()
overStaticRep f = unsafeEff $ \r -> f <$> getEnv r >>= putEnv r

mapReader ::
  (Member (Reader e1) r) => (e1 -> e2) -> Sem (Reader e2 ': r) a -> Sem r a
mapReader f s = do
  e <- ask
  runReader (f e) s

runState :: forall a s r. s -> Sem (State s ': r) a -> Sem r (s, a)
runState s = fmap swap . State.runState s

-- | TODO can we make it strict?
modify' :: (Member (State s) r) => (s -> s) -> Sem r ()
modify' = State.modify

mapError :: (Member (Error b) r) => (a -> b) -> Sem (Error a ': r) x -> Sem r x
mapError f = runErrorWith (\_ e -> throwError (f e))

runM :: (MonadIO m) => Sem '[EmbedIO] a -> m a
runM = liftIO . E.runEff

run :: Sem ('[] :: [Effect]) a -> a
run = E.runPureEff

throw :: (Member (Error err) r) => err -> Sem r a
throw = throwError

runError :: Sem (Error err ': r) x -> Sem r (Either err x)
runError = runErrorNoCallStack

catch ::
  forall e r a.
  (Member (Error e) r) =>
  Sem r a ->
  (e -> Sem r a) ->
  Sem r a
catch m handler = catchError m (const handler)

raiseUnder :: forall (e1 :: Effect) (e2 :: Effect) (r :: [Effect]) a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': r) a
raiseUnder = inject

interpretTop3H ::
  forall (e1 :: Effect) (e2 :: Effect) (e3 :: Effect) (e4 :: Effect) (r :: [Effect]) a.
  (DispatchOf e1 ~ 'Dynamic) =>
  EffectHandler e1 (e4 ': e3 ': e2 ': r) ->
  Sem (e1 ': r) a ->
  Sem (e4 ': e3 ': e2 ': r) a
interpretTop3H i = E.interpret i . inject

interpretTop2H ::
  forall (e1 :: Effect) (e2 :: Effect) (e3 :: Effect) (r :: [Effect]) a.
  (DispatchOf e1 ~ 'Dynamic) =>
  EffectHandler e1 (e3 ': e2 ': r) ->
  Sem (e1 ': r) a ->
  Sem (e3 ': e2 ': r) a
interpretTop2H i = E.interpret i . inject

interpretTopH ::
  forall (e1 :: Effect) (e2 :: Effect) (r :: [Effect]) a.
  (DispatchOf e1 ~ 'Dynamic) =>
  EffectHandler e1 (e2 ': r) ->
  Sem (e1 ': r) a ->
  Sem (e2 ': r) a
interpretTopH i = E.interpret i . raiseUnder

interpretTop3 ::
  forall (e1 :: Effect) (e2 :: Effect) (e3 :: Effect) (e4 :: Effect) (r :: [Effect]) a.
  (DispatchOf e1 ~ 'Dynamic) =>
  EffectHandlerFO e1 (e4 ': e3 ': e2 ': r) ->
  Sem (e1 ': r) a ->
  Sem (e4 ': e3 ': e2 ': r) a
interpretTop3 i = interpretTop3H (const i)

interpretTop ::
  forall (e1 :: Effect) (e2 :: Effect) (r :: [Effect]) a.
  (DispatchOf e1 ~ 'Dynamic) =>
  EffectHandlerFO e1 (e2 ': r) ->
  Sem (e1 ': r) a ->
  Sem (e2 ': r) a
interpretTop i = interpretTopH (const i)

interpret ::
  forall (e1 :: Effect) (r :: [Effect]) a.
  (DispatchOf e1 ~ 'Dynamic) =>
  EffectHandlerFO e1 r ->
  Sem (e1 ': r) a ->
  Sem r a
interpret i = E.interpret (const i)

interpretH ::
  forall (e1 :: Effect) (r :: [Effect]) a.
  (DispatchOf e1 ~ 'Dynamic) =>
  EffectHandler e1 r ->
  Sem (e1 ': r) a ->
  Sem r a
interpretH = E.interpret

-- | Type signature of the effect handler.
-- type EffectHandler e es
--   = forall a localEs. (HasCallStack, Member e localEs)
--   => LocalEnv localEs es
--   -- ^ Capture of the local environment for handling local 'Eff' computations
--   -- when @e@ is a higher order effect.
--   -> e (Sem localEs) a
--   -- ^ The effect performed in the local environment.
--   -> Sem es a
reinterpretHCommon2 ::
  (DispatchOf e ~ 'Dynamic) =>
  (Sem (e2 ': e1 ': r) a -> Sem r b) ->
  EffectHandler e (e2 ': e1 ': r) ->
  Sem (e ': r) a ->
  Sem r b
reinterpretHCommon2 = E.reinterpret

reinterpretH ::
  (DispatchOf e ~ 'Dynamic) =>
  (Sem handlerEs a -> Sem r b) ->
  EffectHandler e handlerEs ->
  Sem (e ': r) a ->
  Sem r b
reinterpretH = E.reinterpret

reinterpret ::
  (DispatchOf e ~ 'Dynamic) =>
  (Sem handlerEs a -> Sem r b) ->
  EffectHandlerFO e handlerEs ->
  Sem (e ': r) a ->
  Sem r b
reinterpret re i = reinterpretH re (const i)

-- TODO maybe think of a better name
runTSimpleEff ::
  forall (handlerEs :: [Effect]) (localEs :: [Effect]) (r :: [Effect]) x.
  (SharedSuffix r handlerEs) =>
  LocalEnv localEs handlerEs ->
  Sem localEs x ->
  Sem r x
runTSimpleEff locEnv ma =
  let lifter :: (forall y. Sem localEs y -> Sem r y) -> Sem r x
      lifter f = f ma
   in localSeqUnlift locEnv lifter

imposeCommon ::
  forall e r e' a b.
  (DispatchOf e ~ 'Dynamic, Member e r) =>
  -- | Introduction of effects encapsulated within the handler.
  (Sem (e' ': r) a -> Sem r b) ->
  -- | The effect handler.
  EffectHandler e (e' ': r) ->
  Sem r a ->
  Sem r b
imposeCommon = impose

imposeCommon2 ::
  forall e r e2 e1 a b.
  (DispatchOf e ~ 'Dynamic, Member e r) =>
  -- | Introduction of effects encapsulated within the handler.
  (Sem (e2 ': e1 ': r) a -> Sem r b) ->
  -- | The effect handler.
  EffectHandler e (e2 ': e1 ': r) ->
  Sem r a ->
  Sem r b
imposeCommon2 = impose

localSeqUnliftCommon ::
  forall r' e1 localEs b.
  LocalEnv localEs (e1 ': r') ->
  ((forall y. Sem localEs y -> Sem r' y) -> Sem r' b) ->
  Sem r' b
localSeqUnliftCommon = localSeqUnlift

localSeqUnliftCommon2 ::
  forall r' e2 e1 localEs b.
  LocalEnv localEs (e2 ': e1 ': r') ->
  ((forall y. Sem localEs y -> Sem r' y) -> Sem r' b) ->
  Sem r' b
localSeqUnliftCommon2 = localSeqUnlift
