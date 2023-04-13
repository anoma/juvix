module Juvix.Compiler.Core.Transformation.Eta
  ( module Juvix.Compiler.Core.Transformation.Eta,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

etaExpandBuiltins :: Node -> Node
etaExpandBuiltins = umap go
  where
    go :: Node -> Node
    go n = case n of
      NBlt BuiltinApp {..}
        | builtinOpArgsNum _builtinAppOp > length _builtinAppArgs ->
            mkApps'
              (etaExpand (builtinOpArgTypes _builtinAppOp) (mkBuiltinApp _builtinAppInfo _builtinAppOp []))
              _builtinAppArgs
      _ -> n

etaExpandConstrs :: (Tag -> [Type]) -> Node -> Node
etaExpandConstrs getArgtys = umap go
  where
    go :: Node -> Node
    go n = case n of
      NCtr Constr {..}
        | length argtys > length _constrArgs ->
            mkApps'
              (etaExpand argtys (mkConstr _constrInfo _constrTag []))
              _constrArgs
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
            mkApps'
              (etaExpand argtys (mkTypeConstr _typeConstrInfo _typeConstrSymbol []))
              _typeConstrArgs
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
      case lookupConstructorInfo' tab tag of
        Just ci -> typeArgs (ci ^. constructorType)
        Nothing -> []

    typeConstrArgtys :: Symbol -> [Type]
    typeConstrArgtys sym =
      case lookupInductiveInfo' tab sym of
        Just ci -> map (^. paramKind) (ci ^. inductiveParams)
        Nothing -> []

etaExpansionApps :: InfoTable -> InfoTable
etaExpansionApps tab = mapAllNodes (etaExpandApps tab) tab
