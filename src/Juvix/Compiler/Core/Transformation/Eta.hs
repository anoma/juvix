module Juvix.Compiler.Core.Transformation.Eta
  ( module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

etaExpandBuiltins :: Node -> Node
etaExpandBuiltins = umap go
  where
    go :: Node -> Node
    go n = case n of
      BuiltinApp {..}
        | builtinOpArgsNum builtinOp > length builtinArgs ->
            etaExpand (builtinOpArgsNum builtinOp - length builtinArgs) n
      _ -> n

etaExpandConstrs :: (Tag -> Int) -> Node -> Node
etaExpandConstrs argsNum = umap go
  where
    go :: Node -> Node
    go n = case n of
      ConstrApp {..}
        | k > length constrArgs ->
            etaExpand (k - length constrArgs) n
        where
          k = argsNum constrTag
      _ -> n

squashApps :: Node -> Node
squashApps = dmap go
  where
    go :: Node -> Node
    go n =
      let (l, args) = unfoldApp n
       in case l of
            ConstrApp i tag args' -> ConstrApp i tag (args' ++ args)
            BuiltinApp i op args' -> BuiltinApp i op (args' ++ args)
            _ -> n

etaExpandApps :: InfoTable -> Node -> Node
etaExpandApps tab =
  squashApps . etaExpandConstrs constrArgsNum . etaExpandBuiltins . squashApps
  where
    constrArgsNum :: Tag -> Int
    constrArgsNum tag =
      case HashMap.lookup tag (tab ^. infoConstructors) of
        Just ci -> ci ^. constructorArgsNum
        Nothing -> 0

etaExpansionApps :: Transformation
etaExpansionApps tab = mapT (etaExpandApps tab) tab
