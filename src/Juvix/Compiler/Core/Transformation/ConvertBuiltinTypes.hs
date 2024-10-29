module Juvix.Compiler.Core.Transformation.ConvertBuiltinTypes
  ( convertBuiltinTypes,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md = umap go
  where
    go :: Node -> Node
    go node = case node of
      NTyp TypeConstr {..} ->
        case ii ^. inductiveBuiltin of
          Just (BuiltinTypeInductive BuiltinBool) -> mkTypeBool'
          Just (BuiltinTypeInductive BuiltinNat) -> mkTypeInteger'
          Just (BuiltinTypeInductive BuiltinInt) -> mkTypeInteger'
          Just (BuiltinTypeAxiom BuiltinString) -> mkTypeString'
          Just (BuiltinTypeAxiom BuiltinField) -> mkTypeField'
          Just (BuiltinTypeAxiom BuiltinByte) -> mkTypeUInt8'
          Just (BuiltinTypeAxiom BuiltinByteArray) -> mkTypeByteArray'
          Just (BuiltinTypeAxiom BuiltinAnomaRandomGenerator) -> mkTypeRandomGenerator'
          _ -> node
        where
          ii = lookupInductiveInfo md _typeConstrSymbol
      _ -> node

convertBuiltinTypes :: Module -> Module
convertBuiltinTypes md =
  mapAllNodes (convertNode md) md
