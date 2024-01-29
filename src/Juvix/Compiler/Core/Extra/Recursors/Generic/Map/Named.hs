module Juvix.Compiler.Core.Extra.Recursors.Generic.Map.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Data.BinderList (BinderList)
import Juvix.Compiler.Core.Extra.Recursors.Generic.Base
import Juvix.Compiler.Core.Extra.Recursors.Generic.Map

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

dmapLRM :: (IsNode n d ch b, Monad m) => (BinderList b -> n -> m (Recur n)) -> n -> m n
dmapLRM f = dmapLRM' (mempty, f)

dmapLM :: (IsNode n d ch b, Monad m) => (BinderList b -> n -> m n) -> n -> m n
dmapLM f = dmapLM' (mempty, f)

umapLM :: (IsNode n d ch b, Monad m) => (BinderList b -> n -> m n) -> n -> m n
umapLM f = umapG binderInfoCollector f

dmapNRM :: (IsNode n d ch b, Monad m) => (Level -> n -> m (Recur n)) -> n -> m n
dmapNRM f = dmapNRM' (0, f)

dmapNM :: (IsNode n d ch b, Monad m) => (Level -> n -> m n) -> n -> m n
dmapNM f = dmapNM' (0, f)

umapNM :: (IsNode n d ch b, Monad m) => (Level -> n -> m n) -> n -> m n
umapNM f = umapG binderNumCollector f

dmapRM :: (IsNode n d ch b, Monad m) => (n -> m (Recur n)) -> n -> m n
dmapRM f = dmapG unitCollector (const (fromRecur mempty . f))

dmapM :: (IsNode n d ch b, Monad m) => (n -> m n) -> n -> m n
dmapM f = dmapG unitCollector (const (fromSimple mempty . f))

umapM :: (IsNode n d ch b, Monad m) => (n -> m n) -> n -> m n
umapM f = umapG unitCollector (const f)

dmapLRM' :: (IsNode n d ch b, Monad m) => (BinderList b, BinderList b -> n -> m (Recur n)) -> n -> m n
dmapLRM' f = dmapG (binderInfoCollector' (fst f)) (\bi -> fromRecur bi . snd f bi)

dmapLM' :: (IsNode n d ch b, Monad m) => (BinderList b, BinderList b -> n -> m n) -> n -> m n
dmapLM' f = dmapG (binderInfoCollector' (fst f)) (\bi -> fromSimple bi . snd f bi)

umapLM' :: (IsNode n d ch b, Monad m) => (BinderList b, BinderList b -> n -> m n) -> n -> m n
umapLM' f = umapG (binderInfoCollector' (fst f)) (snd f)

dmapNRM' :: (IsNode n d ch b, Monad m) => (Level, Level -> n -> m (Recur n)) -> n -> m n
dmapNRM' f = dmapG (binderNumCollector' (fst f)) (\bi -> fromRecur bi . snd f bi)

dmapNM' :: (IsNode n d ch b, Monad m) => (Level, Level -> n -> m n) -> n -> m n
dmapNM' f = dmapG (binderNumCollector' (fst f)) (\bi -> fromSimple bi . snd f bi)

umapNM' :: (IsNode n d ch b, Monad m) => (Level, Level -> n -> m n) -> n -> m n
umapNM' f = umapG (binderNumCollector' (fst f)) (snd f)

dmapLR :: (IsNode n d ch b) => (BinderList b -> n -> Recur n) -> n -> n
dmapLR f = runIdentity . dmapLRM (embedIden2 f)

dmapL :: (IsNode n d ch b) => (BinderList b -> n -> n) -> n -> n
dmapL f = runIdentity . dmapLM (embedIden2 f)

umapL :: (IsNode n d ch b) => (BinderList b -> n -> n) -> n -> n
umapL f = runIdentity . umapLM (embedIden2 f)

dmapNR :: (IsNode n d ch b) => (Level -> n -> Recur n) -> n -> n
dmapNR f = runIdentity . dmapNRM (embedIden2 f)

dmapN :: (IsNode n d ch b) => (Level -> n -> n) -> n -> n
dmapN f = runIdentity . dmapNM (embedIden2 f)

umapN :: (IsNode n d ch b) => (Level -> n -> n) -> n -> n
umapN f = runIdentity . umapNM (embedIden2 f)

dmapR :: (IsNode n d ch b) => (n -> Recur n) -> n -> n
dmapR f = runIdentity . dmapRM (embedIden1 f)

dmap :: (IsNode n d ch b) => (n -> n) -> n -> n
dmap f = runIdentity . dmapM (embedIden1 f)

umap :: (IsNode n d ch b) => (n -> n) -> n -> n
umap f = runIdentity . umapM (embedIden1 f)

dmapLR' :: (IsNode n d ch b) => (BinderList b, BinderList b -> n -> Recur n) -> n -> n
dmapLR' f = runIdentity . dmapLRM' (embedIdenP2 f)

dmapL' :: (IsNode n d ch b) => (BinderList b, BinderList b -> n -> n) -> n -> n
dmapL' f = runIdentity . dmapLM' (embedIdenP2 f)

umapL' :: (IsNode n d ch b) => (BinderList b, BinderList b -> n -> n) -> n -> n
umapL' f = runIdentity . umapLM' (embedIdenP2 f)

dmapNR' :: (IsNode n d ch b) => (Level, Level -> n -> Recur n) -> n -> n
dmapNR' f = runIdentity . dmapNRM' (embedIdenP2 f)

dmapN' :: (IsNode n d ch b) => (Level, Level -> n -> n) -> n -> n
dmapN' f = runIdentity . dmapNM' (embedIdenP2 f)

umapN' :: (IsNode n d ch b) => (Level, Level -> n -> n) -> n -> n
umapN' f = runIdentity . umapNM' (embedIdenP2 f)

dmapCLM' :: (IsNode n d ch b, Monad m) => (BinderList b, c -> BinderList b -> n -> m (c, n)) -> c -> n -> m n
dmapCLM' f ini = dmapG (pairCollector (identityCollector ini) (binderInfoCollector' (fst f))) (\(c, bi) -> fromPair bi . snd f c bi)

dmapCLRM' :: (IsNode n d ch b, Monad m) => (BinderList b, c -> BinderList b -> n -> m (Recur' n c)) -> c -> n -> m n
dmapCLRM' f ini = dmapG (pairCollector (identityCollector ini) (binderInfoCollector' (fst f))) (\(c, bi) -> fromRecur' bi . snd f c bi)

dmapCNRM' :: (IsNode n d ch b, Monad m) => (Level, c -> Level -> n -> m (Recur' n c)) -> c -> n -> m n
dmapCNRM' f ini = dmapG (pairCollector (identityCollector ini) (binderNumCollector' (fst f))) (\(c, bi) -> fromRecur' bi . snd f c bi)

dmapCLM :: (IsNode n d ch b, Monad m) => (c -> BinderList b -> n -> m (c, n)) -> c -> n -> m n
dmapCLM f = dmapCLM' (mempty, f)

dmapCNM' :: (IsNode n d ch b, Monad m) => (Level, c -> Level -> n -> m (c, n)) -> c -> n -> m n
dmapCNM' f ini = dmapG (pairCollector (identityCollector ini) (binderNumCollector' (fst f))) (\(c, bi) -> fromPair bi . snd f c bi)

dmapCNM :: (IsNode n d ch b, Monad m) => (c -> Level -> n -> m (c, n)) -> c -> n -> m n
dmapCNM f = dmapCNM' (0, f)

dmapCM :: (IsNode n d ch b, Monad m) => (c -> n -> m (c, n)) -> c -> n -> m n
dmapCM f ini = dmapG (identityCollector ini) (\c -> fmap Recur' . f c)

dmapCL' :: (IsNode n d ch b) => (BinderList b, c -> BinderList b -> n -> (c, n)) -> c -> n -> n
dmapCL' f ini = runIdentity . dmapCLM' (embedIdenP3 f) ini

dmapCLR' :: (IsNode n d ch b) => (BinderList b, c -> BinderList b -> n -> Recur' n c) -> c -> n -> n
dmapCLR' f ini = runIdentity . dmapCLRM' (embedIdenP3 f) ini

dmapCN' :: (IsNode n d ch b) => (Level, c -> Level -> n -> (c, n)) -> c -> n -> n
dmapCN' f ini = runIdentity . dmapCNM' (embedIdenP3 f) ini

dmapCNR' :: (IsNode n d ch b) => (Level, c -> Level -> n -> Recur' n c) -> c -> n -> n
dmapCNR' f ini = runIdentity . dmapCNRM' (embedIdenP3 f) ini

dmapCL :: (IsNode n d ch b) => (c -> BinderList b -> n -> (c, n)) -> c -> n -> n
dmapCL f ini = runIdentity . dmapCLM (embedIden3 f) ini

dmapCN :: (IsNode n d ch b) => (c -> Level -> n -> (c, n)) -> c -> n -> n
dmapCN f ini = runIdentity . dmapCNM (embedIden3 f) ini

dmapC :: (IsNode n d ch b) => (c -> n -> (c, n)) -> c -> n -> n
dmapC f ini = runIdentity . dmapCM (embedIden2 f) ini
