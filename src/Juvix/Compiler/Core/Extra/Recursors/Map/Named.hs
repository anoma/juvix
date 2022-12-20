module Juvix.Compiler.Core.Extra.Recursors.Map.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Map
import Juvix.Compiler.Core.Extra.Recursors.Parameters

dmapLRM :: Monad m => (BinderList Binder -> Node -> m Recur) -> Node -> m Node
dmapLRM f = dmapLRM' (mempty, f)

dmapLM :: Monad m => (BinderList Binder -> Node -> m Node) -> Node -> m Node
dmapLM f = dmapLM' (mempty, f)

umapLM :: Monad m => (BinderList Binder -> Node -> m Node) -> Node -> m Node
umapLM f = nodeMapG' SBottomUp binderInfoCollector f

dmapNRM :: Monad m => (Level -> Node -> m Recur) -> Node -> m Node
dmapNRM f = dmapNRM' (0, f)

dmapNM :: Monad m => (Level -> Node -> m Node) -> Node -> m Node
dmapNM f = dmapNM' (0, f)

umapNM :: Monad m => (Level -> Node -> m Node) -> Node -> m Node
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

dmapNRM' :: Monad m => (Level, Level -> Node -> m Recur) -> Node -> m Node
dmapNRM' f = nodeMapG' STopDown (binderNumCollector' (fst f)) (\bi -> fromRecur bi . snd f bi)

dmapNM' :: Monad m => (Level, Level -> Node -> m Node) -> Node -> m Node
dmapNM' f = nodeMapG' STopDown (binderNumCollector' (fst f)) (\bi -> fromSimple bi . snd f bi)

umapNM' :: Monad m => (Level, Level -> Node -> m Node) -> Node -> m Node
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

dmapCLM' :: Monad m => (BinderList Binder, c -> BinderList Binder -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCLM' f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) (binderInfoCollector' (fst f))) (\(c, bi) -> fromPair bi . snd f c bi)

dmapCLRM' :: Monad m => (BinderList Binder, c -> BinderList Binder -> Node -> m (Recur' c)) -> c -> Node -> m Node
dmapCLRM' f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) (binderInfoCollector' (fst f))) (\(c, bi) -> fromRecur' bi . snd f c bi)

dmapCNRM' :: Monad m => (Level, c -> Level -> Node -> m (Recur' c)) -> c -> Node -> m Node
dmapCNRM' f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) (binderNumCollector' (fst f))) (\(c, bi) -> fromRecur' bi . snd f c bi)

dmapCLM :: Monad m => (c -> BinderList Binder -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCLM f = dmapCLM' (mempty, f)

dmapCNM :: Monad m => (c -> Level -> Node -> m (c, Node)) -> c -> Node -> m Node
dmapCNM f ini = nodeMapG' STopDown (pairCollector (identityCollector ini) binderNumCollector) (\(c, bi) -> fromPair bi . f c bi)

dmapCM :: Monad m => (c -> Node -> m (c, Node)) -> c -> Node -> m Node
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
