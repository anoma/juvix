{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Core.Extra.Recursors.SMap where

import Data.Functor.Identity
import Data.Kind qualified as GHC
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Recursors.Base

-- | `sumapG` is the shallow version of `umapG`, i.e., it doesn't recurse under binders
sumapG ::
  forall m.
  (Monad m) =>
  (Node -> m Node) ->
  Node ->
  m Node
sumapG f = go
  where
    go :: Node -> m Node
    go n =
      let ni = destruct n
       in do
            ns <- mapM goChild (ni ^. nodeChildren)
            f (reassembleDetails ni ns)
      where
        goChild :: NodeChild -> m Node
        goChild nc
          | nc ^. childBindersNum == 0 = go (nc ^. childNode)
          | otherwise = return $ nc ^. childNode

sdmapG ::
  forall m.
  (Monad m) =>
  (Node -> m Recur) ->
  Node ->
  m Node
sdmapG f = go
  where
    go :: Node -> m Node
    go n = do
      r <- f n
      case r of
        End n' -> return n'
        Recur n' ->
          let ni = destruct n'
           in reassembleDetails ni <$> mapM goChild (ni ^. nodeChildren)
          where
            goChild :: NodeChild -> m Node
            goChild ch
              | ch ^. childBindersNum == 0 = go (ch ^. childNode)
              | otherwise = return $ ch ^. childNode

type OverIdentity :: GHC.Type -> GHC.Type
type family OverIdentity t = res where
  OverIdentity (a -> b) = a -> OverIdentity b
  OverIdentity leaf = Identity leaf

class EmbedIdentity a where
  embedIden :: a -> OverIdentity a

instance (EmbedIdentity b) => EmbedIdentity (a -> b) where
  embedIden f = embedIden . f

instance EmbedIdentity Node where
  embedIden = Identity

instance EmbedIdentity Recur where
  embedIden = Identity
