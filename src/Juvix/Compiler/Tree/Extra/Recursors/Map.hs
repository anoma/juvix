module Juvix.Compiler.Tree.Extra.Recursors.Map where

import Juvix.Compiler.Tree.Extra.Base
import Juvix.Compiler.Tree.Extra.Recursors.Base

-- | `umapG` maps the nodes bottom-up, i.e., when invoking the mapper function the
-- recursive subnodes have already been mapped
umapG ::
  forall c m.
  (Monad m) =>
  Collector (Int, [TempVarInfo]) c ->
  (c -> Node -> m Node) ->
  Node ->
  m Node
umapG coll f = go (coll ^. cEmpty)
  where
    go :: c -> Node -> m Node
    go c n =
      let ni = destruct n
       in do
            ns <- mapM (\n' -> go ((coll ^. cCollect) (fromEnum (isJust (n' ^. childTempVarInfo)), toList (n' ^. childTempVarInfo)) c) (n' ^. childNode)) (ni ^. nodeChildren)
            f c (reassembleDetails ni ns)

dmapG ::
  forall c m.
  (Monad m) =>
  Collector (Int, [TempVarInfo]) c ->
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
            goChild ch = go ((coll ^. cCollect) (fromEnum (isJust (ch ^. childTempVarInfo)), toList (ch ^. childTempVarInfo)) c') (ch ^. childNode)

fromSimple :: (Functor g) => c -> g Node -> g (Recur' c)
fromSimple c = fmap (\x -> Recur' (c, x))

fromRecur :: (Functor g) => c -> g Recur -> g (Recur' c)
fromRecur c =
  fmap
    ( \case
        End x -> End' x
        Recur x -> Recur' (c, x)
    )

fromPair :: (Functor g) => d -> g (c, Node) -> g (Recur' (c, d))
fromPair d = fmap (\(c, x) -> Recur' ((c, d), x))

fromRecur' :: (Functor g) => d -> g (Recur' c) -> g (Recur' (c, d))
fromRecur' d =
  fmap
    ( \case
        End' x -> End' x
        Recur' (c, x) -> Recur' ((c, d), x)
    )
