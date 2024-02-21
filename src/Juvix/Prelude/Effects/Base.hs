module Juvix.Prelude.Effects.Base
  ( module Juvix.Prelude.Effects.Base,
    module Effectful,
    module Effectful.Reader.Static,
    module Effectful.State.Static.Local,
    module Effectful.Error.Static,
    module Effectful.Dispatch.Dynamic,
    module Effectful.TH,
    module Effectful.Dispatch.Static,
  )
where

import Data.Kind qualified as GHC
import Effectful hiding (Eff, (:>))
import Effectful qualified as E
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.Dispatch.Static
import Effectful.Error.Static hiding (runError)
import Effectful.Internal.Env (getEnv, putEnv)
import Effectful.Reader.Static
import Effectful.State.Static.Local hiding (runState)
import Effectful.State.Static.Local qualified as State
import Effectful.TH
import Juvix.Prelude.Base.Foundation
import Language.Haskell.TH.Syntax qualified as GHC

type Sem = E.Eff

type EmbedIO = IOE

type Member (e :: Effect) (r :: [Effect]) = e E.:> r

-- | First order effect handler
type EffectHandlerFO  (e :: Effect) (r :: [Effect]) =
  forall a localEs. (HasCallStack, Member e localEs)
  => e (Sem localEs) a
  -> Sem r a

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

runState :: s -> Sem (State s ': r) a -> Sem r (s, a)
runState s = fmap swap . State.runState s

-- | TODO can we make it strict?
modify' :: (Member (State s) r) => (s -> s) -> Sem r ()
modify' = State.modify

mapError :: (Member (Error b) r) => (a -> b) -> Sem (Error a ': r) x -> Sem r x
mapError f = runErrorWith (\_ e -> throwError (f e))

throw :: (Member (Error err) r) => err -> Sem r a
throw = throwError

runError :: Sem (Error err ': r) x -> Sem r (Either err x)
runError = runErrorNoCallStack

raiseUnder :: forall (e1 :: Effect) (e2 :: Effect) (r :: [Effect]) a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': r) a
raiseUnder = inject

interpretTopH :: forall (e1 :: Effect) (e2 :: Effect) (r :: [Effect]) a. (DispatchOf e1 ~ 'Dynamic) => EffectHandler e1 (e2 ': r) -> Sem (e1 ': r) a -> E.Eff (e2 ': r) a
interpretTopH i = interpret i . raiseUnder

interpretTop :: forall (e1 :: Effect) (e2 :: Effect) (r :: [Effect]) a. (DispatchOf e1 ~ 'Dynamic) => EffectHandler e1 (e2 ': r) -> Sem (e1 ': r) a -> E.Eff (e2 ': r) a
interpretTop i = interpret i . raiseUnder

-- reinterpretTop :: forall (e1 :: Effect) (e2 :: Effect) (r :: [Effect]) a. (DispatchOf e1 ~ 'Dynamic) => EffectHandler e1 (e2 ': r) -> Sem (e1 ': r) a -> E.Eff (e2 ': r) a
-- reinterpretTop i = interpret i . raiseUnder
