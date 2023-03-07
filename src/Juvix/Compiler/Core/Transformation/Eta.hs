module Juvix.Compiler.Core.Transformation.Eta
  ( module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

substDrop :: [Node] -> [Node] -> [Node]
substDrop args argtys =
  reverse $ snd $ foldl' (\(args', acc) ty -> (mkVar' 0 : map (shift 1) args', substs args' ty : acc)) (reverse args, []) (drop k argtys)
  where
    k = length args

etaExpandBuiltins :: Node -> Node
etaExpandBuiltins = umap go
  where
    go :: Node -> Node
    go n = case n of
      NBlt BuiltinApp {..}
        | builtinOpArgsNum _builtinAppOp > length _builtinAppArgs ->
            etaExpand (substDrop _builtinAppArgs (builtinOpArgTypes _builtinAppOp)) n
      _ -> n

etaExpandConstrs :: (Tag -> [Type]) -> Node -> Node
etaExpandConstrs getArgtys = umap go
  where
    go :: Node -> Node
    go n = case n of
      NCtr Constr {..}
        | length argtys > length _constrArgs ->
            etaExpand (substDrop _constrArgs argtys) n
        where
          argtys = getArgtys _constrTag
      _ -> n

etaExpandTypeConstrs :: (Symbol -> [Type]) -> Node -> Node
etaExpandTypeConstrs getArgtys = umap go
  where
    go :: Node -> Node
    go n = case n of
      NTyp TypeConstr {..}
        | length argtys > length _typeConstrArgs ->
            etaExpand (substDrop _typeConstrArgs argtys) n
        where
          argtys = getArgtys _typeConstrSymbol
      _ -> n

etaExpandApps :: InfoTable -> Node -> Node
etaExpandApps tab =
  squashApps
    . etaExpandTypeConstrs typeConstrArgtys
    . etaExpandConstrs constrArgtys
    . etaExpandBuiltins
    . squashApps
  where
    constrArgtys :: Tag -> [Type]
    constrArgtys tag =
      case HashMap.lookup tag (tab ^. infoConstructors) of
        Just ci -> typeArgs (ci ^. constructorType)
        Nothing -> []

    typeConstrArgtys :: Symbol -> [Type]
    typeConstrArgtys sym =
      case HashMap.lookup sym (tab ^. infoInductives) of
        Just ci -> map (^. paramKind) (ci ^. inductiveParams)
        Nothing -> []

etaExpansionApps :: InfoTable -> InfoTable
etaExpansionApps tab = mapAllNodes (etaExpandApps tab) tab
