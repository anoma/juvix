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

import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Effectful.Internal.Env (getEnv, putEnv)
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Effectful.TH
import Juvix.Prelude.Base.Foundation

overStaticRep :: (DispatchOf e ~ 'Static sideEffects, e :> r) => (StaticRep e -> StaticRep e) -> Eff r ()
overStaticRep f = unsafeEff $ \r -> do
  e' <- f <$> getEnv r
  putEnv r e'
