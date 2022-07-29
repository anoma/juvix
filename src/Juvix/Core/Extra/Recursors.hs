module Juvix.Core.Extra.Recursors where

import Data.Functor.Identity
import Juvix.Core.Language
import Juvix.Core.Language.Info.BinderInfo
import Juvix.Core.Extra.Base

{---------------------------------------------------------------------------------}
{- General recursors on Node  -}

-- a collector collects information top-down on a single path in the program
-- tree
data Collector a c = Collector
  { _cEmpty :: c,
    _cCollect :: a -> c -> c
  }

makeLenses ''Collector

unitCollector :: Collector a ()
unitCollector = Collector () (\_ _ -> ())

binderInfoCollector :: Collector (Int, Maybe [BinderInfo]) [Maybe BinderInfo]
binderInfoCollector =
  Collector
    []
    (\(k, bi) c -> if k == 0 then c else map Just (fromJust bi) ++ c)

binderNumCollector :: Collector (Int, Maybe [BinderInfo]) Index
binderNumCollector = Collector 0 (\(k, _) c -> c + k)

-- `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG ::
  forall c m.
  Monad m =>
  Collector (Int, Maybe [BinderInfo]) c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
umapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n =
      let ni = destruct n
       in do
            ns <-
              sequence $
                zipWith3Exact
                  (\n' k bis -> go ((coll ^. cCollect) (k, bis) c) n')
                  (ni ^. nodeChildren)
                  (ni ^. nodeChildBindersNum)
                  (ni ^. nodeChildBindersInfo)
            f c ((ni ^. nodeReassemble) (ni ^. nodeInfo) ns)

umapM :: Monad m => (Node -> m Node) -> Node -> m Node
umapM f = umapG unitCollector (const f)

umapMB :: Monad m => ([Maybe BinderInfo] -> Node -> m Node) -> Node -> m Node
umapMB f = umapG binderInfoCollector f

umapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
umapMN f = umapG binderNumCollector f

umap :: (Node -> Node) -> Node -> Node
umap f n = runIdentity $ umapM (return . f) n

umapB :: ([Maybe BinderInfo] -> Node -> Node) -> Node -> Node
umapB f n = runIdentity $ umapMB (\is -> return . f is) n

umapN :: (Index -> Node -> Node) -> Node -> Node
umapN f n = runIdentity $ umapMN (\idx -> return . f idx) n

-- `dmapG` maps the nodes top-down
dmapG ::
  forall c m.
  Monad m =>
  Collector (Int, Maybe [BinderInfo]) c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
dmapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n = do
      n' <- f c n
      let ni = destruct n'
      ns <-
        sequence $
          zipWith3Exact
            (\n'' k bis -> go ((coll ^. cCollect) (k, bis) c) n'')
            (ni ^. nodeChildren)
            (ni ^. nodeChildBindersNum)
            (ni ^. nodeChildBindersInfo)
      return ((ni ^. nodeReassemble) (ni ^. nodeInfo) ns)

dmapM :: Monad m => (Node -> m Node) -> Node -> m Node
dmapM f = dmapG unitCollector (const f)

dmapMB :: Monad m => ([Maybe BinderInfo] -> Node -> m Node) -> Node -> m Node
dmapMB f = dmapG binderInfoCollector f

dmapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
dmapMN f = dmapG binderNumCollector f

dmap :: (Node -> Node) -> Node -> Node
dmap f n = runIdentity $ dmapM (return . f) n

dmapB :: ([Maybe BinderInfo] -> Node -> Node) -> Node -> Node
dmapB f n = runIdentity $ dmapMB (\is -> return . f is) n

dmapN :: (Index -> Node -> Node) -> Node -> Node
dmapN f n = runIdentity $ dmapMN (\idx -> return . f idx) n

-- `ufoldG` folds the tree bottom-up. The `uplus` argument combines the values -
-- it should be commutative and associative.
ufoldG ::
  forall c a m.
  Monad m =>
  Collector (Int, Maybe [BinderInfo]) c ->
  (a -> a -> a) ->
  (c -> Node -> m a) ->
  Node ->
  m a
ufoldG coll uplus f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m a
    go c n = foldr (liftM2 uplus) (f c n) mas
      where
        ni :: NodeDetails
        ni = destruct n
        mas :: [m a]
        mas =
          zipWith3Exact
            (\n' k bis -> go ((coll ^. cCollect) (k, bis) c) n')
            (ni ^. nodeChildren)
            (ni ^. nodeChildBindersNum)
            (ni ^. nodeChildBindersInfo)

ufoldM :: Monad m => (a -> a -> a) -> (Node -> m a) -> Node -> m a
ufoldM uplus f = ufoldG unitCollector uplus (const f)

ufoldMB :: Monad m => (a -> a -> a) -> ([Maybe BinderInfo] -> Node -> m a) -> Node -> m a
ufoldMB uplus f = ufoldG binderInfoCollector uplus f

ufoldMN :: Monad m => (a -> a -> a) -> (Index -> Node -> m a) -> Node -> m a
ufoldMN uplus f = ufoldG binderNumCollector uplus f

ufold :: (a -> a -> a) -> (Node -> a) -> Node -> a
ufold uplus f n = runIdentity $ ufoldM uplus (return . f) n

ufoldB :: (a -> a -> a) -> ([Maybe BinderInfo] -> Node -> a) -> Node -> a
ufoldB uplus f n = runIdentity $ ufoldMB uplus (\is -> return . f is) n

ufoldN :: (a -> a -> a) -> (Index -> Node -> a) -> Node -> a
ufoldN uplus f n = runIdentity $ ufoldMN uplus (\idx -> return . f idx) n

walk :: Monad m => (Node -> m ()) -> Node -> m ()
walk = ufoldM mappend

walkB :: Monad m => ([Maybe BinderInfo] -> Node -> m ()) -> Node -> m ()
walkB = ufoldMB mappend

walkN :: Monad m => (Index -> Node -> m ()) -> Node -> m ()
walkN = ufoldMN mappend

gather :: (a -> Node -> a) -> a -> Node -> a
gather f acc n = run $ execState acc (walk (\n' -> modify (`f` n')) n)

gatherB :: ([Maybe BinderInfo] -> a -> Node -> a) -> a -> Node -> a
gatherB f acc n = run $ execState acc (walkB (\is n' -> modify (\a -> f is a n')) n)

gatherN :: (Index -> a -> Node -> a) -> a -> Node -> a
gatherN f acc n = run $ execState acc (walkN (\idx n' -> modify (\a -> f idx a n')) n)
