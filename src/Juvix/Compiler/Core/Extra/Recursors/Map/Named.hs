module Juvix.Compiler.Core.Extra.Recursors.Map.Named where

import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.Map
import Juvix.Compiler.Core.Extra.Recursors.Parameters

dmapLRM :: Monad m => (BinderList Binder -> Node -> m Recur) -> Node -> m Node
dmapLRM = nodeMapI STopDown

dmapLM :: Monad m => (BinderList Binder -> Node -> m Node) -> Node -> m Node
dmapLM = nodeMapI STopDown

umapLM :: Monad m => (BinderList Binder -> Node -> m Node) -> Node -> m Node
umapLM = nodeMapI SBottomUp

dmapNRM :: Monad m => (Index -> Node -> m Recur) -> Node -> m Node
dmapNRM = nodeMapI STopDown

dmapNM :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
dmapNM = nodeMapI STopDown

umapNM :: Monad m => (Index -> Node -> m Node) -> Node -> m Node
umapNM = nodeMapI STopDown

dmapRM :: Monad m => (Node -> m Recur) -> Node -> m Node
dmapRM = nodeMapI STopDown

dmapM :: Monad m => (Node -> m Node) -> Node -> m Node
dmapM = nodeMapI STopDown

umapM :: Monad m => (Node -> m Node) -> Node -> m Node
umapM = nodeMapI SBottomUp

dmapLRM' :: Monad m => (BinderList Binder, BinderList Binder -> Node -> m Recur) -> Node -> m Node
dmapLRM' = nodeMapI STopDown

dmapLM' :: Monad m => (BinderList Binder, BinderList Binder -> Node -> m Node) -> Node -> m Node
dmapLM' = nodeMapI STopDown

umapLM' :: Monad m => (BinderList Binder, BinderList Binder -> Node -> m Node) -> Node -> m Node
umapLM' = nodeMapI SBottomUp

dmapNRM' :: Monad m => (Index, Index -> Node -> m Recur) -> Node -> m Node
dmapNRM' = nodeMapI STopDown

dmapNM' :: Monad m => (Index, Index -> Node -> m Node) -> Node -> m Node
dmapNM' = nodeMapI STopDown

umapNM' :: Monad m => (Index, Index -> Node -> m Node) -> Node -> m Node
umapNM' = nodeMapI SBottomUp

dmapLR :: (BinderList Binder -> Node -> Recur) -> Node -> Node
dmapLR = nodeMapI STopDown

dmapL :: (BinderList Binder -> Node -> Node) -> Node -> Node
dmapL = nodeMapI STopDown

umapL :: (BinderList Binder -> Node -> Node) -> Node -> Node
umapL = nodeMapI SBottomUp

dmapNR :: (Index -> Node -> Recur) -> Node -> Node
dmapNR = nodeMapI STopDown

dmapN :: (Index -> Node -> Node) -> Node -> Node
dmapN = nodeMapI STopDown

umapN :: (Index -> Node -> Node) -> Node -> Node
umapN = nodeMapI SBottomUp

dmapR :: (Node -> Recur) -> Node -> Node
dmapR = nodeMapI STopDown

dmap :: (Node -> Node) -> Node -> Node
dmap = nodeMapI STopDown

umap :: (Node -> Node) -> Node -> Node
umap = nodeMapI SBottomUp

dmapLR' :: (BinderList Binder, BinderList Binder -> Node -> Recur) -> Node -> Node
dmapLR' = nodeMapI STopDown

dmapL' :: (BinderList Binder, BinderList Binder -> Node -> Node) -> Node -> Node
dmapL' = nodeMapI STopDown

umapL' :: (BinderList Binder, BinderList Binder -> Node -> Node) -> Node -> Node
umapL' = nodeMapI SBottomUp

dmapNR' :: (Index, Index -> Node -> Recur) -> Node -> Node
dmapNR' = nodeMapI STopDown

dmapN' :: (Index, Index -> Node -> Node) -> Node -> Node
dmapN' = nodeMapI STopDown

umapN' :: (Index, Index -> Node -> Node) -> Node -> Node
umapN' = nodeMapI SBottomUp
