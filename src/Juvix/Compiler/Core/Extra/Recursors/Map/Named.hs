module Juvix.Compiler.Core.Extra.Recursors.Map.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Map
import Juvix.Compiler.Core.Extra.Recursors.Parameters

dmapLRM :: Monad m => (BinderList Binder -> Node -> m Recur) -> Node -> m Node
dmapLRM f = nodeMapG' STopDown binderInfoCollector (\bi -> fromRecur bi . f bi)

dmapLM :: Monad m => (BinderList Binder -> Node -> m Node) -> Node -> m Node
dmapLM f = nodeMapG' STopDown binderInfoCollector (\bi -> fromSimple bi . f bi)

umapLM :: Monad m => (BinderList Binder -> Node -> m Node) -> Node -> m Node
umapLM f = nodeMapG' SBottomUp binderInfoCollector f

dmapNRM :: Monad m => (Index -> Node -> m Recur) -> Node -> m Node
dmapNRM f = nodeMapG' STopDown binderNumCollector (\bi -> fromRecur bi . f bi)

dmapNM :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
dmapNM f = nodeMapG' STopDown binderNumCollector (\bi -> fromSimple bi . f bi)

umapNM :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
umapNM f = nodeMapG' SBottomUp binderNumCollector f

dmapRM :: Monad m => (Node -> m Recur) -> Node -> m Node
dmapRM f = nodeMapG' STopDown unitCollector (const (fromRecur mempty . f))

dmapM :: Monad m => (Node -> m Node) -> Node -> m Node
dmapM f = nodeMapG' STopDown unitCollector (const (fromSimple mempty . f))

umapM :: Monad m => (Node -> m Node) -> Node -> m Node
umapM f = nodeMapG' SBottomUp unitCollector (const f)

dmapLRM' :: Monad m => (BinderList Binder, BinderList Binder -> Node -> m Recur) -> Node -> m Node
dmapLRM' f = nodeMapG' STopDown (binderInfoCollector' (fst f)) (\bi -> fromRecur bi . snd f bi)

dmapLM' :: Monad m => (BinderList Binder, BinderList Binder -> Node -> m Node) -> Node -> m Node
dmapLM' f = nodeMapG' STopDown (binderInfoCollector' (fst f)) (\bi -> fromSimple bi . snd f bi)

umapLM' :: Monad m => (BinderList Binder, BinderList Binder -> Node -> m Node) -> Node -> m Node
umapLM' f = nodeMapG' SBottomUp (binderInfoCollector' (fst f)) (snd f)

dmapNRM' :: Monad m => (Index, Index -> Node -> m Recur) -> Node -> m Node
dmapNRM' f = nodeMapG' STopDown (binderNumCollector' (fst f)) (\bi -> fromRecur bi . snd f bi)

dmapNM' :: Monad m => (Index, Index -> Node -> m Node) -> Node -> m Node
dmapNM' f = nodeMapG' STopDown (binderNumCollector' (fst f)) (\bi -> fromSimple bi . snd f bi)

umapNM' :: Monad m => (Index, Index -> Node -> m Node) -> Node -> m Node
umapNM' f = nodeMapG' SBottomUp (binderNumCollector' (fst f)) (snd f)

dmapLR :: (BinderList Binder -> Node -> Recur) -> Node -> Node
dmapLR f = runIdentity . dmapLRM (embedIden f)

dmapL :: (BinderList Binder -> Node -> Node) -> Node -> Node
dmapL f = runIdentity . dmapLM (embedIden f)

umapL :: (BinderList Binder -> Node -> Node) -> Node -> Node
umapL f = runIdentity . umapLM (embedIden f)

dmapNR :: (Index -> Node -> Recur) -> Node -> Node
dmapNR f = runIdentity . dmapNRM (embedIden f)

dmapN :: (Index -> Node -> Node) -> Node -> Node
dmapN f = runIdentity . dmapNM (embedIden f)

umapN :: (Index -> Node -> Node) -> Node -> Node
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

dmapNR' :: (Index, Index -> Node -> Recur) -> Node -> Node
dmapNR' f = runIdentity . dmapNRM' (embedIden f)

dmapN' :: (Index, Index -> Node -> Node) -> Node -> Node
dmapN' f = runIdentity . dmapNM' (embedIden f)

umapN' :: (Index, Index -> Node -> Node) -> Node -> Node
umapN' f = runIdentity . umapNM' (embedIden f)

dmapCLM :: Monad m => (c -> BinderList Binder -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCLM f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) binderInfoCollector) (\(c, bi) -> fromPair bi . f c bi)

dmapCNM :: Monad m => (c -> Index -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCNM f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) binderNumCollector) (\(c, bi) -> fromPair bi . f c bi)

dmapCM :: Monad m => (c -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCM f ini = nodeMapG' STopDown (identityCollector ini) (\c -> fmap Recur' . f c)

dmapCL :: (c -> BinderList Binder -> Node -> (c, Node)) -> c -> Node -> Node
dmapCL f ini = runIdentity . dmapCLM (embedIden f) ini

dmapCN :: (c -> Index -> Node -> (c, Node)) -> c -> Node -> Node
dmapCN f ini = runIdentity . dmapCNM (embedIden f) ini

dmapC :: (c -> Node -> (c, Node)) -> c -> Node -> Node
dmapC f ini = runIdentity . dmapCM (embedIden f) ini
