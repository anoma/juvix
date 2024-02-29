module Juvix.Prelude.Effects.Base.Internal (localSeqHandle, localSeqHandle2) where

import Data.Primitive.PrimArray
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad
import Juvix.Prelude.Base.Foundation

localSeqHandle2 ::
  forall e es localEs handlerEs a.
  (e :> es, SharedSuffix es handlerEs) =>
  LocalEnv localEs handlerEs ->
  -- | Continuation with the local handler in scope.
  (LocalEnv (e ': localEs) handlerEs -> (forall r. Eff (e ': localEs) r -> Eff localEs r) -> Eff es a) ->
  Eff es a
localSeqHandle2 (LocalEnv les) k = unsafeEff $ \es -> do
  eles :: Env (e ': localEs) <- copyRef es les
  let lenv :: LocalEnv (e ': localEs) handlerEs = LocalEnv eles
  -- seqUnliftIO eles $ \unlift -> (`unEff` es) $ k $ (unsafeEff_) . unlift
  seqUnliftIO eles $ \unlift -> (`unEff` es) (k lenv ((unsafeEff_) . unlift))

-- | copied from module Effectful.Dispatch.Dynamic
-- Will be available in the next effectful release (current release: 2.3.0.1)
localSeqHandle ::
  (e :> es, SharedSuffix es handlerEs) =>
  LocalEnv localEs handlerEs ->
  -- | Continuation with the local handler in scope.
  ((forall r. Eff (e ': localEs) r -> Eff localEs r) -> Eff es a) ->
  Eff es a
localSeqHandle (LocalEnv les) k = unsafeEff $ \es -> do
  eles <- copyRef es les
  seqUnliftIO eles $ \unlift -> (`unEff` es) $ k $ unsafeEff_ . unlift

-- | copied from module Effectful.Dispatch.Dynamic
-- We only need this because of `localSeqHandle`
copyRef ::
  forall e es localEs.
  (e :> es) =>
  Env es ->
  Env localEs ->
  IO (Env (e ': localEs))
copyRef (Env hoffset hrefs hstorage) (Env offset refs0 storage) = do
  when (hstorage /= storage) $ do
    error "storages do not match"
  let size = sizeofPrimArray refs0 - offset
      i = 2 * reifyIndex @e @es
  mrefs <- newPrimArray (size + 2)
  copyPrimArray mrefs 0 hrefs (hoffset + i) 2
  copyPrimArray mrefs 2 refs0 offset size
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
