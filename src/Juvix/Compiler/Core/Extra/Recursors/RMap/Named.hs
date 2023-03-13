module Juvix.Compiler.Core.Extra.Recursors.RMap.Named where

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
- `BCKeep n` indicates that `n` original binders are preserved,
- `BCRemove node` indicates that a binder is removed and all bound variables
  referring to it replaced with `node`. The de Bruijn indices in `node` are
  relative to the result at the point of `recur` invocation, i.e., adjusted by
  the shifts caused by the binder changes before it in the binder change list.

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
  go :: ([BinderChange] -> Level -> Node -> Node) -> Node -> Node
  go recur k node = case node of
    NLam Lambda {..} ->
      mkLet (ConstInt k) (cont [BCAdd 1, BCRemove (BinderRemove _lambdaBinder mkVar' 0)] k _lambdaBody)
    _ ->
      recur [] node

  cont :: [BinderChange] -> Level -> Node -> Node
  cont bcs = go (recur . (bcs ++))
```
produces
```
let x := 0 in let y := 1 in x$1 + y$0
```

-}
