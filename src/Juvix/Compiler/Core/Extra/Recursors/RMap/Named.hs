module Juvix.Compiler.Core.Extra.Recursors.RMap.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.RMap

{-
`rmap f t` goes through the node top-down, but in contrast to `dmap` the
recursion must be performed explicitly with a function provided to `f`, and it
is possible to specify the changes in binders. In `rmap f t` the function `f`
receives as its first argument the function `recur :: [BinderChange] -> Node ->
Node` which changes/shifts binders and recurses into the children of the given
node.

The first argument to `recur` specifies the changes in binders on top of the
call to `recur`:
- `BCAdd n` indicates that `n` new binders are added,
- `BCKeep b` indicates that an original binder `b` is preserved,
- `BCRemove (BinderRemove b node)` indicates that a binder `b` is removed and
  all bound variables referring to it replaced with `node`. The de Bruijn
  indices in `node` are relative to the result at the point of `recur`
  invocation, i.e., adjusted by the shifts caused by the binder changes before
  it in the binder change list.

The function `recur` automatically adjusts the de Bruijn indices in the result
node, by shifting them according to the binder shift at their binding point. For
example, the invocation (in pseudocode)
```
rmap go (\x \y x$1 + y$0)
where
  go :: ([BinderChange] -> Node -> Node) -> Node -> Node
  go recur node = case node of
    NLam {} -> mkLambda' mkDynamic' (recur [BCAdd 1] node)
    _ -> recur [] node
```
produces
```
\_ \x \_ \y x$2 + y$0
```

The invocation
```
rmapN go (\x \y x$1 + y$0)
where
  go :: ([BinderChange] -> Node -> Node) -> Level -> Node -> Node
  go recur k node = case node of
    NLam Lambda {..} ->
      mkLet
        _lambdaInfo
        (over binderType (cont []) _lambdaBinder)
        (mkConstant' (ConstInteger (fromIntegral k)))
        (cont [BCAdd 1, BCRemove (BinderRemove _lambdaBinder (mkVar' 0))] _lambdaBody)
    _ ->
      recur [] node
    where
      cont :: Level -> [BinderChange] -> Node -> Node
      cont bcs = go (recur . (bcs ++)) (k + bindersNumFromBinderChange bcs)
```
produces
```
let x := 0 in let y := 1 in x$1 + y$0
```

The invocation
```
rmap go (\x \z \y \_ x$3 + y$1 + z$2)
where
  go :: ([BinderChange] -> Node -> Node) -> Node -> Node
  go recur node = case node of
    NLam lam1@(Lambda _ _ (NLam lam2)) ->
      mkLambda
        (lam1 ^. lambdaInfo)
        (over binderType (cont []) (lam1 ^. lambdaBinder))
        (cont
          [
            BCKeep (lam1 ^. lambdaBinder),
            BCRemove (BinderRemove (lam2 ^. lambdaBinder) (mkVar' 0))
          ]
          (lam2 ^. lambdaBody))
    _ ->
      recur [] node
    where
      cont :: [BinderChange] -> Node -> Node
      cont bcs = go (recur . (bcs ++))
```
produces
```
\x \y x$1 + y$0 + x$1
```

For the meaning of the suffixes to `rmap` see the comments in
Core.Extra.Recursors.Map.Named
-}

rmapLM :: (Monad m) => (([BinderChange] -> Node -> m Node) -> BinderList Binder -> Node -> m Node) -> Node -> m Node
rmapLM f = rmapLM' (\recur bl -> f (recur bl) bl)

rmapNM :: (Monad m) => (([BinderChange] -> Node -> m Node) -> Level -> Node -> m Node) -> Node -> m Node
rmapNM f = rmapNM' (\recur bl -> f (recur bl) bl)

rmapM :: (Monad m) => (([BinderChange] -> Node -> m Node) -> Node -> m Node) -> Node -> m Node
rmapM f = rmapG unitCollector (\recur _ -> f (recur ()))

rmapLM' :: (Monad m) => ((BinderList Binder -> [BinderChange] -> Node -> m Node) -> BinderList Binder -> Node -> m Node) -> Node -> m Node
rmapLM' f = rmapG binderInfoCollector f

rmapNM' :: (Monad m) => ((Level -> [BinderChange] -> Node -> m Node) -> Level -> Node -> m Node) -> Node -> m Node
rmapNM' f = rmapG binderNumCollector f

rmapL :: (([BinderChange] -> Node -> Node) -> BinderList Binder -> Node -> Node) -> Node -> Node
rmapL f = runIdentity . rmapLM (rmapEmbedIden' f)

rmapN :: (([BinderChange] -> Node -> Node) -> Level -> Node -> Node) -> Node -> Node
rmapN f = runIdentity . rmapNM (rmapEmbedIden' f)

rmap :: (([BinderChange] -> Node -> Node) -> Node -> Node) -> Node -> Node
rmap f = runIdentity . rmapM (rmapEmbedIden f)

rmapL' :: ((BinderList Binder -> [BinderChange] -> Node -> Node) -> BinderList Binder -> Node -> Node) -> Node -> Node
rmapL' f = runIdentity . rmapLM' (rmapEmbedIden'' f)

rmapN' :: ((Level -> [BinderChange] -> Node -> Node) -> Level -> Node -> Node) -> Node -> Node
rmapN' f = runIdentity . rmapNM' (rmapEmbedIden'' f)

rmapCLM' :: (Monad m) => ((c -> BinderList Binder -> [BinderChange] -> Node -> m Node) -> c -> BinderList Binder -> Node -> m Node) -> c -> Node -> m Node
rmapCLM' f ini = rmapG (pairCollector (identityCollector ini) binderInfoCollector) (uncurry . f . curry)

rmapCLM :: (Monad m) => ((c -> [BinderChange] -> Node -> m Node) -> c -> BinderList Binder -> Node -> m Node) -> c -> Node -> m Node
rmapCLM f = rmapCLM' (\recur c bl -> f (`recur` bl) c bl)

rmapCNM' :: (Monad m) => ((c -> Level -> [BinderChange] -> Node -> m Node) -> c -> Level -> Node -> m Node) -> c -> Node -> m Node
rmapCNM' f ini = rmapG (pairCollector (identityCollector ini) binderNumCollector) (uncurry . f . curry)

rmapCNM :: (Monad m) => ((c -> [BinderChange] -> Node -> m Node) -> c -> Level -> Node -> m Node) -> c -> Node -> m Node
rmapCNM f = rmapCNM' (\recur c bl -> f (`recur` bl) c bl)

rmapCM :: (Monad m) => ((c -> [BinderChange] -> Node -> m Node) -> c -> Node -> m Node) -> c -> Node -> m Node
rmapCM f ini = rmapG (identityCollector ini) f

rmapCL' :: ((c -> BinderList Binder -> [BinderChange] -> Node -> Node) -> c -> BinderList Binder -> Node -> Node) -> c -> Node -> Node
rmapCL' f ini = runIdentity . rmapCLM' (rmapCEmbedIden'' f) ini

rmapCL :: ((c -> [BinderChange] -> Node -> Node) -> c -> BinderList Binder -> Node -> Node) -> c -> Node -> Node
rmapCL f ini = runIdentity . rmapCLM (rmapCEmbedIden' f) ini

rmapCN' :: ((c -> Level -> [BinderChange] -> Node -> Node) -> c -> Level -> Node -> Node) -> c -> Node -> Node
rmapCN' f ini = runIdentity . rmapCNM' (rmapCEmbedIden'' f) ini

rmapCN :: ((c -> [BinderChange] -> Node -> Node) -> c -> Level -> Node -> Node) -> c -> Node -> Node
rmapCN f ini = runIdentity . rmapCNM (rmapCEmbedIden' f) ini

rmapC :: ((c -> [BinderChange] -> Node -> Node) -> c -> Node -> Node) -> c -> Node -> Node
rmapC f ini = runIdentity . rmapCM (rmapCEmbedIden f) ini
