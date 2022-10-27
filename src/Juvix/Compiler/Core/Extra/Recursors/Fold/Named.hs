module Juvix.Compiler.Core.Extra.Recursors.Fold.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Fold

ufoldA :: Applicative f => (a -> [a] -> a) -> (Node -> f a) -> Node -> f a
ufoldA uplus f = ufoldG unitCollector uplus (const f)

ufoldLA :: Applicative f => (a -> [a] -> a) -> (BinderList Binder -> Node -> f a) -> Node -> f a
ufoldLA uplus f = ufoldG binderInfoCollector uplus f

ufoldNA :: Applicative f => (a -> [a] -> a) -> (Index -> Node -> f a) -> Node -> f a
ufoldNA uplus f = ufoldG binderNumCollector uplus f

walk :: Applicative f => (Node -> f ()) -> Node -> f ()
walk = ufoldA (foldr mappend)

walkN :: Applicative f => (Index -> Node -> f ()) -> Node -> f ()
walkN = ufoldNA (foldr mappend)

walkL :: Applicative f => (BinderList Binder -> Node -> f ()) -> Node -> f ()
walkL = ufoldLA (foldr mappend)

ufold :: (a -> [a] -> a) -> (Node -> a) -> Node -> a
ufold uplus f = runIdentity . ufoldA uplus (return . f)

ufoldL :: (a -> [a] -> a) -> (BinderList Binder -> Node -> a) -> Node -> a
ufoldL uplus f = runIdentity . ufoldLA uplus (\is -> return . f is)

ufoldN :: (a -> [a] -> a) -> (Index -> Node -> a) -> Node -> a
ufoldN uplus f = runIdentity . ufoldNA uplus (\idx -> return . f idx)

gather :: (a -> Node -> a) -> a -> Node -> a
gather f acc = run . execState acc . walk (\n' -> modify' (`f` n'))

gatherL :: (BinderList Binder -> a -> Node -> a) -> a -> Node -> a
gatherL f acc = run . execState acc . walkL (\is n' -> modify' (\a -> f is a n'))

gatherN :: (Index -> a -> Node -> a) -> a -> Node -> a
gatherN f acc = run . execState acc . walkN (\idx n' -> modify' (\a -> f idx a n'))
