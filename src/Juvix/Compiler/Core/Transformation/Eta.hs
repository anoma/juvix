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

etaExpandTypeConstrs :: (Symbol -> Int) -> Node -> Node
etaExpandTypeConstrs argsNum = umap go
  where
    go :: Node -> Node
    go n = case n of
      NTyp TypeConstr {..}
        | k > length _typeConstrArgs ->
            etaExpand (k - length _typeConstrArgs) n
        where
          k = argsNum _typeConstrSymbol
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
            NTyp (TypeConstr i sym args') -> mkTypeConstr i sym (args' ++ args)
            _ -> n

etaExpandApps :: InfoTable -> Node -> Node
etaExpandApps tab =
  squashApps
    . etaExpandTypeConstrs typeConstrArgsNum
    . etaExpandConstrs constrArgsNum
    . etaExpandBuiltins
    . squashApps
  where
    constrArgsNum :: Tag -> Int
    constrArgsNum tag =
      case HashMap.lookup tag (tab ^. infoConstructors) of
        Just ci -> ci ^. constructorArgsNum
        Nothing -> 0

    typeConstrArgsNum :: Symbol -> Int
    typeConstrArgsNum sym =
      case HashMap.lookup sym (tab ^. infoInductives) of
        Just ci -> length (ci ^. inductiveParams)
        Nothing -> 0

etaExpansionApps :: InfoTable -> InfoTable
etaExpansionApps tab = mapT (const (etaExpandApps tab)) tab
