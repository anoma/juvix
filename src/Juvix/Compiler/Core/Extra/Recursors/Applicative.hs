-- | Applicative recursors over 'Node'.
module Juvix.Compiler.Core.Extra.Recursors.Applicative where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base

umapLeavesG ::
  forall c f.
  Applicative f =>
  Collector (Int, [Info]) c ->
  (c -> Node -> f Node) ->
  Node ->
  f Node
umapLeavesG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> f Node
    go c n
      | null (ni ^. nodeChildren) = f c n
      | otherwise = do
          ns <-
            sequenceA $
              zipWith3Exact
                (\n' k bis -> go ((coll ^. cCollect) (k, bis) c) n')
                (ni ^. nodeChildren)
                (ni ^. nodeChildBindersNum)
                (ni ^. nodeChildBindersInfo)
          return ((ni ^. nodeReassemble) (ni ^. nodeInfo) ns)
      where
        ni = destruct n

ufoldG' ::
  forall c a f.
  Applicative f =>
  Collector (Int, [Info]) c ->
  (a -> [a] -> a) ->
  (c -> Node -> f a) ->
  Node ->
  f a
ufoldG' coll uplus f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> f a
    go c n = do
      mas' <- sequenceA mas
      n' <- f c n
      pure (uplus n' mas')
      where
        ni :: NodeDetails
        ni = destruct n
        mas :: [f a]
        mas =
          zipWith3Exact
            (\n' k bis -> go ((coll ^. cCollect) (k, bis) c) n')
            (ni ^. nodeChildren)
            (ni ^. nodeChildBindersNum)
            (ni ^. nodeChildBindersInfo)

-- `ufoldG` folds the tree bottom-up. The `uplus` argument combines the values -
-- it should be commutative and associative.
ufoldG ::
  forall c a f.
  Applicative f =>
  Collector (Int, [Info]) c ->
  (a -> a -> a) ->
  (c -> Node -> f a) ->
  Node ->
  f a
ufoldG coll uplus = ufoldG' coll uplus'
  where
    uplus' :: a -> [a] -> a
    uplus' = foldr uplus

ufoldA :: Applicative f => (a -> a -> a) -> (Node -> f a) -> Node -> f a
ufoldA uplus f = ufoldG unitCollector uplus (const f)

ufoldAB :: Applicative f => (a -> a -> a) -> (BinderList Info -> Node -> f a) -> Node -> f a
ufoldAB uplus f = ufoldG binderInfoCollector uplus f

ufoldAN :: Applicative f => (a -> a -> a) -> (Index -> Node -> f a) -> Node -> f a
ufoldAN uplus f = ufoldG binderNumCollector uplus f

ufoldAN' :: Applicative f => (a -> [a] -> a) -> (Index -> Node -> f a) -> Node -> f a
ufoldAN' uplus f = ufoldG' binderNumCollector uplus f

walk :: Applicative f => (Node -> f ()) -> Node -> f ()
walk = ufoldA mappend

walkN :: Applicative f => (Index -> Node -> f ()) -> Node -> f ()
walkN = ufoldAN mappend

walkB :: Applicative f => (BinderList Info -> Node -> f ()) -> Node -> f ()
walkB = ufoldAB mappend

umapLeavesN :: Applicative f => (Index -> Node -> f Node) -> Node -> f Node
umapLeavesN f = umapLeavesG binderNumCollector f

umapLeaves :: Applicative f => (Node -> f Node) -> Node -> f Node
umapLeaves f = umapLeavesG unitCollector (const f)
