module Juvix.Compiler.Core.Extra.Recursors.Map.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Map
import Juvix.Compiler.Core.Extra.Recursors.Parameters

{-

The mapping recursors come in three major variants: dmap, umap and rmap. They
map each subterm of a given term.

1. `dmap f t` goes through the node `t` top-down, applying the function `f` to
`t` first, and then recursively descending into the children of `f t`.

2. `umap f t` goes through the term `t` bottom-up, first recursively descending
into the children of `t` and mapping them with `umap f`, then reassembling `t`
with the mapped children into `t'`, and finally applying `f` to `t'`.

3. `rmap f t`: see Recursors.RMap.Named.

The suffixes of `dmap`, `umap` and `rmap` indicate the exact form of the mapping
function `f`, what arguments are provided to it and how its return value is
interpreted.

- M: Monadic version. The return value of the mapping function `f` is wrapped in
  a monad.
- L: The function `f` receives as an argument the list of binders upwards in the
  term. The n-th element of the binder list corresponds to the free variable of
  the current subterm with de Bruijn index n.
- N: The function `f` receives as an argument the number of binders upwards in
  the term, i.e., the current de Bruijn level.
- ': When combined with L or N, makes it possible to supply the initial binder
  list or de Bruijn level. This is useful when mapping a subterm with free
  variables.
- R: The function `f` returns an element of the `Recur` (or `Recur'`) datatype,
  indicating whether `dmap` should descend into the children or stop the
  traversal.
- C: Enables collecting an arbitrary value while going downward in the term tree
  with `dmap`. The initial value is provided to `dmap`. The function `f`
  receives as an argument the current collected value and returns the value for
  the children, in addition to the new node.

-}

dmapLRM :: (Monad m) => (BinderList Binder -> Node -> m Recur) -> Node -> m Node
dmapLRM f = dmapLRM' (mempty, f)

dmapLM :: (Monad m) => (BinderList Binder -> Node -> m Node) -> Node -> m Node
dmapLM f = dmapLM' (mempty, f)

umapLM :: (Monad m) => (BinderList Binder -> Node -> m Node) -> Node -> m Node
umapLM f = nodeMapG' SBottomUp binderInfoCollector f

dmapNRM :: (Monad m) => (Level -> Node -> m Recur) -> Node -> m Node
dmapNRM f = dmapNRM' (0, f)

dmapNM :: (Monad m) => (Level -> Node -> m Node) -> Node -> m Node
dmapNM f = dmapNM' (0, f)

umapNM :: (Monad m) => (Level -> Node -> m Node) -> Node -> m Node
umapNM f = nodeMapG' SBottomUp binderNumCollector f

dmapRM :: (Monad m) => (Node -> m Recur) -> Node -> m Node
dmapRM f = nodeMapG' STopDown unitCollector (const (fromRecur mempty . f))

dmapM :: (Monad m) => (Node -> m Node) -> Node -> m Node
dmapM f = nodeMapG' STopDown unitCollector (const (fromSimple mempty . f))

umapM :: (Monad m) => (Node -> m Node) -> Node -> m Node
umapM f = nodeMapG' SBottomUp unitCollector (const f)

dmapLRM' :: (Monad m) => (BinderList Binder, BinderList Binder -> Node -> m Recur) -> Node -> m Node
dmapLRM' f = nodeMapG' STopDown (binderInfoCollector' (fst f)) (\bi -> fromRecur bi . snd f bi)

dmapLM' :: (Monad m) => (BinderList Binder, BinderList Binder -> Node -> m Node) -> Node -> m Node
dmapLM' f = nodeMapG' STopDown (binderInfoCollector' (fst f)) (\bi -> fromSimple bi . snd f bi)

umapLM' :: (Monad m) => (BinderList Binder, BinderList Binder -> Node -> m Node) -> Node -> m Node
umapLM' f = nodeMapG' SBottomUp (binderInfoCollector' (fst f)) (snd f)

dmapNRM' :: (Monad m) => (Level, Level -> Node -> m Recur) -> Node -> m Node
dmapNRM' f = nodeMapG' STopDown (binderNumCollector' (fst f)) (\bi -> fromRecur bi . snd f bi)

dmapNM' :: (Monad m) => (Level, Level -> Node -> m Node) -> Node -> m Node
dmapNM' f = nodeMapG' STopDown (binderNumCollector' (fst f)) (\bi -> fromSimple bi . snd f bi)

umapNM' :: (Monad m) => (Level, Level -> Node -> m Node) -> Node -> m Node
umapNM' f = nodeMapG' SBottomUp (binderNumCollector' (fst f)) (snd f)

dmapLR :: (BinderList Binder -> Node -> Recur) -> Node -> Node
dmapLR f = runIdentity . dmapLRM (embedIden f)

dmapL :: (BinderList Binder -> Node -> Node) -> Node -> Node
dmapL f = runIdentity . dmapLM (embedIden f)

umapL :: (BinderList Binder -> Node -> Node) -> Node -> Node
umapL f = runIdentity . umapLM (embedIden f)

dmapNR :: (Level -> Node -> Recur) -> Node -> Node
dmapNR f = runIdentity . dmapNRM (embedIden f)

dmapN :: (Level -> Node -> Node) -> Node -> Node
dmapN f = runIdentity . dmapNM (embedIden f)

umapN :: (Level -> Node -> Node) -> Node -> Node
umapN f = runIdentity . umapNM (embedIden f)

dmapR :: (Node -> Recur) -> Node -> Node
dmapR f = runIdentity . dmapRM (embedIden f)

dmap :: (Node -> Node) -> Node -> Node
dmap f = runIdentity . dmapM (embedIden f)

umap :: (Node -> Node) -> Node -> Node
umap f = runIdentity . umapM (embedIden f)

dmapLR' :: (BinderList Binder, BinderList Binder -> Node -> Recur) -> Node -> Node
dmapLR' f = runIdentity . dmapLRM' (embedIden f)

dmapL' :: (BinderList Binder, BinderList Binder -> Node -> Node) -> Node -> Node
dmapL' f = runIdentity . dmapLM' (embedIden f)

umapL' :: (BinderList Binder, BinderList Binder -> Node -> Node) -> Node -> Node
umapL' f = runIdentity . umapLM' (embedIden f)

dmapNR' :: (Level, Level -> Node -> Recur) -> Node -> Node
dmapNR' f = runIdentity . dmapNRM' (embedIden f)

dmapN' :: (Level, Level -> Node -> Node) -> Node -> Node
dmapN' f = runIdentity . dmapNM' (embedIden f)

umapN' :: (Level, Level -> Node -> Node) -> Node -> Node
umapN' f = runIdentity . umapNM' (embedIden f)

dmapCLM' :: (Monad m) => (BinderList Binder, c -> BinderList Binder -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCLM' f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) (binderInfoCollector' (fst f))) (\(c, bi) -> fromPair bi . snd f c bi)

dmapCLRM' :: (Monad m) => (BinderList Binder, c -> BinderList Binder -> Node -> m (Recur' c)) -> c -> Node -> m Node
dmapCLRM' f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) (binderInfoCollector' (fst f))) (\(c, bi) -> fromRecur' bi . snd f c bi)

dmapCNRM' :: (Monad m) => (Level, c -> Level -> Node -> m (Recur' c)) -> c -> Node -> m Node
dmapCNRM' f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) (binderNumCollector' (fst f))) (\(c, bi) -> fromRecur' bi . snd f c bi)

dmapCLM :: (Monad m) => (c -> BinderList Binder -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCLM f = dmapCLM' (mempty, f)

dmapCNM :: (Monad m) => (c -> Level -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCNM f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) binderNumCollector) (\(c, bi) -> fromPair bi . f c bi)

dmapCM :: (Monad m) => (c -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCM f ini = nodeMapG' STopDown (identityCollector ini) (\c -> fmap Recur' . f c)

dmapCL' :: (BinderList Binder, c -> BinderList Binder -> Node -> (c, Node)) -> c -> Node -> Node
dmapCL' f ini = runIdentity . dmapCLM' (embedIden f) ini

dmapCLR' :: (BinderList Binder, c -> BinderList Binder -> Node -> Recur' c) -> c -> Node -> Node
dmapCLR' f ini = runIdentity . dmapCLRM' (embedIden f) ini

dmapCNR' :: (Level, c -> Level -> Node -> Recur' c) -> c -> Node -> Node
dmapCNR' f ini = runIdentity . dmapCNRM' (embedIden f) ini

dmapCL :: (c -> BinderList Binder -> Node -> (c, Node)) -> c -> Node -> Node
dmapCL f ini = runIdentity . dmapCLM (embedIden f) ini

dmapCN :: (c -> Level -> Node -> (c, Node)) -> c -> Node -> Node
dmapCN f ini = runIdentity . dmapCNM (embedIden f) ini

dmapC :: (c -> Node -> (c, Node)) -> c -> Node -> Node
dmapC f ini = runIdentity . dmapCM (embedIden f) ini
