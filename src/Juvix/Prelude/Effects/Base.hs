module Juvix.Prelude.Effects.Base
  ( module Juvix.Prelude.Effects.Base,
    module Effectful,
    module Effectful.Reader.Static,
    module Effectful.State.Static.Local,
    module Effectful.Error.Static,
    module Effectful.TH,
    module Effectful.Dispatch.Static,
  )
where

import Data.Kind qualified as GHC
import Effectful hiding (Eff, (:>))
import Effectful qualified as E
import Effectful.Dispatch.Static
import Effectful.Error.Static hiding (runError)
import Effectful.Internal.Env (getEnv, putEnv)
import Effectful.Reader.Static
import Effectful.State.Static.Local hiding (runState)
import Effectful.State.Static.Local qualified as State
import Effectful.TH
import Juvix.Prelude.Base.Foundation

type Sem = E.Eff

type Member (e :: Effect) (r :: [Effect]) = e E.:> r

-- type Members (e :: [Effect]) (r :: [Effect]) = e E.:>> r

type Members :: [Effect] -> [Effect] -> GHC.Constraint
type family Members es r where
  Members '[] _ = ()
  Members (e ': es) r = (Member e r, Members es r)

overStaticRep ::
  ( DispatchOf e ~ 'Static sideEffects,
    Member e r
  ) =>
  (StaticRep e -> StaticRep e) ->
  Sem r ()
overStaticRep f = unsafeEff $ \r -> f <$> getEnv r >>= putEnv r

runState ::
  forall s es a.
  s ->
  Sem (State s ': es) a ->
  Sem es (a, s)
runState = State.runState

mapError :: (Member (Error b) r) => (a -> b) -> Sem (Error a ': r) x -> Sem r x
mapError f = runErrorWith (\_ e -> throwError (f e))

runError :: Sem (Error err ': r) x -> Sem r (Either err x)
runError = runErrorNoCallStack
