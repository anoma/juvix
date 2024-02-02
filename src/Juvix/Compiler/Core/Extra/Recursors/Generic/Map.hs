{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Core.Extra.Recursors.Generic.Map where

import Juvix.Compiler.Core.Extra.Recursors.Generic.Base

-- | `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG ::
  forall c m n d ch b.
  (IsNode n d ch b) =>
  (Monad m) =>
  Collector (Int, [b]) c ->
  (c -> n -> m n) ->
  n ->
  m n
umapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> n -> m n
    go c n =
      let ni = gDestruct n
       in do
            ns <- mapM (\n' -> go ((coll ^. cCollect) (gBindersNum n', gBinders n') c) (gChild n')) (gChildren ni)
            f c (gReassemble ni ns)

dmapG ::
  forall c m n d ch b.
  (IsNode n d ch b) =>
  (Monad m) =>
  Collector (Int, [b]) c ->
  (c -> n -> m (Recur' n c)) ->
  n ->
  m n
dmapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> n -> m n
    go c n = do
      r <- f c n
      case r of
        End' n' -> return n'
        Recur' (c', n') ->
          let ni = gDestruct n'
           in gReassemble ni <$> mapM goChild (gChildren ni)
          where
            goChild :: ch -> m n
            goChild ch = go ((coll ^. cCollect) (gBindersNum ch, gBinders ch) c') (gChild ch)

fromSimple :: (Functor g) => c -> g n -> g (Recur' n c)
fromSimple c = fmap (\x -> Recur' (c, x))

fromRecur :: (Functor g) => c -> g (Recur n) -> g (Recur' n c)
fromRecur c =
  fmap
    ( \case
        End x -> End' x
        Recur x -> Recur' (c, x)
    )

fromPair :: (Functor g) => d -> g (c, n) -> g (Recur' n (c, d))
fromPair d = fmap (\(c, x) -> Recur' ((c, d), x))

fromRecur' :: (Functor g) => d -> g (Recur' n c) -> g (Recur' n (c, d))
fromRecur' d =
  fmap
    ( \case
        End' x -> End' x
        Recur' (c, x) -> Recur' ((c, d), x)
    )
