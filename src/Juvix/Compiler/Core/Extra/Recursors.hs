module Juvix.Compiler.Core.Extra.Recursors
  ( module Juvix.Compiler.Core.Extra.Recursors,
    module Juvix.Compiler.Core.Extra.Recursors.Applicative,
    module Juvix.Compiler.Core.Extra.Recursors.Collector,
    module Juvix.Compiler.Core.Extra.Recursors.Monadic,
  )
where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Applicative
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Collector
import Juvix.Compiler.Core.Extra.Recursors.Monadic

dmap :: (Node -> Node) -> Node -> Node
dmap f = runIdentity . dmapM (return . f)

dmapB :: (BinderList Info -> Node -> Node) -> Node -> Node
dmapB f = runIdentity . dmapMB (\is -> return . f is)

dmapN :: (Index -> Node -> Node) -> Node -> Node
dmapN f = runIdentity . dmapMN (\idx -> return . f idx)

ufold :: (a -> a -> a) -> (Node -> a) -> Node -> a
ufold uplus f n = runIdentity $ ufoldA uplus (return . f) n

ufoldB :: (a -> a -> a) -> (BinderList Info -> Node -> a) -> Node -> a
ufoldB uplus f = runIdentity . ufoldAB uplus (\is -> return . f is)

ufoldN :: (a -> a -> a) -> (Index -> Node -> a) -> Node -> a
ufoldN uplus f = runIdentity . ufoldAN uplus (\idx -> return . f idx)

gather :: (a -> Node -> a) -> a -> Node -> a
gather f acc = run . execState acc . walk (\n' -> modify' (`f` n'))

gatherB :: (BinderList Info -> a -> Node -> a) -> a -> Node -> a
gatherB f acc = run . execState acc . walkB (\is n' -> modify' (\a -> f is a n'))

gatherN :: (Index -> a -> Node -> a) -> a -> Node -> a
gatherN f acc = run . execState acc . walkN (\idx n' -> modify' (\a -> f idx a n'))

umap :: (Node -> Node) -> Node -> Node
umap f = runIdentity . umapM (return . f)

umapB :: (BinderList Info -> Node -> Node) -> Node -> Node
umapB f = runIdentity . umapMB (\is -> return . f is)

umapN :: (Index -> Node -> Node) -> Node -> Node
umapN f = runIdentity . umapMN (\idx -> return . f idx)
