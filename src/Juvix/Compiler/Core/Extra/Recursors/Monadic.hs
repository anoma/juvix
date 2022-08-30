module Juvix.Compiler.Core.Extra.Recursors.Monadic where

import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base

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

umapMB :: Monad m => (BinderList (Maybe BinderInfo) -> Node -> m Node) -> Node -> m Node
umapMB f = umapG binderInfoCollector f

umapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
umapMN f = umapG binderNumCollector f

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

dmapMB :: Monad m => (BinderList (Maybe BinderInfo) -> Node -> m Node) -> Node -> m Node
dmapMB f = dmapG binderInfoCollector f

dmapMN :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
dmapMN f = dmapG binderNumCollector f
