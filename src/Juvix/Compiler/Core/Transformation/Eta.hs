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
      NBlt BuiltinApp {..}
        | builtinOpArgsNum _builtinAppOp > length _builtinAppArgs ->
            etaExpand (builtinOpArgsNum _builtinAppOp - length _builtinAppArgs) n
      _ -> n

etaExpandConstrs :: (Tag -> Int) -> Node -> Node
etaExpandConstrs argsNum = umap go
  where
    go :: Node -> Node
    go n = case n of
      NCtr Constr {..}
        | k > length _constrArgs ->
            etaExpand (k - length _constrArgs) n
        where
          k = argsNum _constrTag
      _ -> n

squashApps :: Node -> Node
squashApps = dmap go
  where
    go :: Node -> Node
    go n =
      let (l, args) = unfoldApps' n
       in case l of
            NCtr (Constr i tag args') -> mkConstr i tag (args' ++ args)
            NBlt (BuiltinApp i op args') -> mkBuiltinApp i op (args' ++ args)
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
etaExpansionApps tab = mapT (const (etaExpandApps tab)) tab
