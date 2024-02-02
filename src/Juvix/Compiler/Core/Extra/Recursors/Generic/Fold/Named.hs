module Juvix.Compiler.Core.Extra.Recursors.Generic.Fold.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Extra.Recursors.Generic.Base
import Juvix.Compiler.Core.Extra.Recursors.Generic.Fold

{-

There are three major versions of folding recursors.

1. `ufold f g t` folds the term `t` bottom-up, first using `g` to map the
   current subterm into a value `a`, and then using `f` to combine `a` with the
   values for children.
2. `walk f t` combines the applicative actions obtained by applying `f` to each
   subterm of `t`.
3. `gather f a t` goes through all subterms of `t` applying `f` to the current
   value and the subterm to obtain the next value, with `a` being the initial
   value.

The suffixes of `ufold`, etc., indicate the exact form of the folding functions,
with similar conventions as with the mapping recursors (see
Core/Extra/Recursors/Map/Named.hs).

\* A: Applicative version.
\* L: Provide the binder list.
\* N: Provide the de Bruijn level.

-}

ufoldA :: (IsNode n d ch b, Applicative f) => (a -> [a] -> a) -> (n -> f a) -> n -> f a
ufoldA uplus f = ufoldG unitCollector uplus (const f)

ufoldLA :: (IsNode n d ch b, Applicative f) => (a -> [a] -> a) -> (BinderList b -> n -> f a) -> n -> f a
ufoldLA uplus f = ufoldG binderInfoCollector uplus f

ufoldNA :: (IsNode n d ch b, Applicative f) => (a -> [a] -> a) -> (Level -> n -> f a) -> n -> f a
ufoldNA uplus f = ufoldG binderNumCollector uplus f

walk :: (IsNode n d ch b, Applicative f) => (n -> f ()) -> n -> f ()
walk = ufoldA (foldr mappend)

walkN :: (IsNode n d ch b, Applicative f) => (Level -> n -> f ()) -> n -> f ()
walkN = ufoldNA (foldr mappend)

walkL :: (IsNode n d ch b, Applicative f) => (BinderList b -> n -> f ()) -> n -> f ()
walkL = ufoldLA (foldr mappend)

ufold :: (IsNode n d ch b) => (a -> [a] -> a) -> (n -> a) -> n -> a
ufold uplus f = runIdentity . ufoldA uplus (return . f)

ufoldL :: (IsNode n d ch b) => (a -> [a] -> a) -> (BinderList b -> n -> a) -> n -> a
ufoldL uplus f = runIdentity . ufoldLA uplus (\is -> return . f is)

ufoldN :: (IsNode n d ch b) => (a -> [a] -> a) -> (Level -> n -> a) -> n -> a
ufoldN uplus f = runIdentity . ufoldNA uplus (\idx -> return . f idx)

gather :: (IsNode n d ch b) => (a -> n -> a) -> a -> n -> a
gather f acc = run . execState acc . walk (\n' -> modify' (`f` n'))

gatherL :: (IsNode n d ch b) => (BinderList b -> a -> n -> a) -> a -> n -> a
gatherL f acc = run . execState acc . walkL (\is n' -> modify' (\a -> f is a n'))

gatherN :: (IsNode n d ch b) => (Index -> a -> n -> a) -> a -> n -> a
gatherN f acc = run . execState acc . walkN (\idx n' -> modify' (\a -> f idx a n'))
