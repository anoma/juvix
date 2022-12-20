{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Core.Extra.Recursors.Map where

import Data.Functor.Identity
import Data.Kind qualified as GHC
import Data.Singletons.TH
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Parameters

type DirTy :: Direction -> GHC.Type -> GHC.Type
type family DirTy d c = res | res -> d where
  DirTy 'TopDown c = Recur' c
  DirTy 'BottomUp _ = Node -- For bottom up maps we never recur on the children

-- | `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG ::
  forall c m.
  Monad m =>
  Collector (Int, [Binder]) c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
umapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n =
      let ni = destruct n
       in do
            ns <- mapM (\n' -> go ((coll ^. cCollect) (n' ^. childBindersNum, n' ^. childBinders) c) (n' ^. childNode)) (ni ^. nodeChildren)
            f c (reassembleDetails ni ns)

dmapG ::
  forall c m.
  Monad m =>
  Collector (Int, [Binder]) c ->
  (c -> Node -> m (Recur' c)) ->
  Node ->
  m Node
dmapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n = do
      r <- f c n
      case r of
        End' n' -> return n'
        Recur' (c', n') ->
          let ni = destruct n'
           in reassembleDetails ni <$> mapM goChild (ni ^. nodeChildren)
          where
            goChild :: NodeChild -> m Node
            goChild ch = go ((coll ^. cCollect) (ch ^. childBindersNum, ch ^. childBinders) c') (ch ^. childNode)

type OverIdentity' :: GHC.Type -> GHC.Type
type family OverIdentity' t = res where
  OverIdentity' (a -> b) = a -> OverIdentity' b
  OverIdentity' leaf = Identity leaf

type OverIdentity :: GHC.Type -> GHC.Type
type family OverIdentity t = res where
  OverIdentity ((), b) = ((), OverIdentity' b)
  OverIdentity (BinderList Binder, b) = (BinderList Binder, OverIdentity' b)
  OverIdentity (Index, b) = (Index, OverIdentity' b)
  OverIdentity leaf = OverIdentity' leaf

class EmbedIdentity a where
  embedIden :: a -> OverIdentity a

class EmbedIdentity' a where
  embedIden' :: a -> OverIdentity' a

instance EmbedIdentity' b => EmbedIdentity' (a -> b) where
  embedIden' f = embedIden' . f

instance EmbedIdentity' b => EmbedIdentity ((), b) where
  embedIden (a, b) = (a, embedIden' b)

instance EmbedIdentity' b => EmbedIdentity (Index, b) where
  embedIden (a, b) = (a, embedIden' b)

instance EmbedIdentity' b => EmbedIdentity (BinderList Binder, b) where
  embedIden (a, b) = (a, embedIden' b)

instance EmbedIdentity' b => EmbedIdentity (a -> b) where
  embedIden a = embedIden' a

instance EmbedIdentity' (c, Node) where
  embedIden' = Identity

instance EmbedIdentity' Node where
  embedIden' = Identity

instance EmbedIdentity' Recur where
  embedIden' = Identity

instance EmbedIdentity' (Recur' c) where
  embedIden' = Identity

fromSimple :: Functor g => c -> g Node -> g (Recur' c)
fromSimple c = fmap (\x -> Recur' (c, x))

fromRecur :: Functor g => c -> g Recur -> g (Recur' c)
fromRecur c =
  fmap
    ( \case
        End x -> End' x
        Recur x -> Recur' (c, x)
    )

fromPair :: Functor g => d -> g (c, Node) -> g (Recur' (c, d))
fromPair d = fmap (\(c, x) -> Recur' ((c, d), x))

fromRecur' :: Functor g => d -> g (Recur' c) -> g (Recur' (c, d))
fromRecur' d =
  fmap
    ( \case
        End' x -> End' x
        Recur' (c, x) -> Recur' ((c, d), x)
    )

nodeMapG' ::
  Monad m =>
  Sing dir ->
  Collector (Int, [Binder]) c ->
  (c -> Node -> m (DirTy dir c)) ->
  Node ->
  m Node
nodeMapG' sdir = case sdir of
  STopDown -> dmapG
  SBottomUp -> umapG
