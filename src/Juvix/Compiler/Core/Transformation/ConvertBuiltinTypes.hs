module Juvix.Compiler.Core.Transformation.ConvertBuiltinTypes
  ( convertBuiltinTypes,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: InfoTable -> Node -> Node
convertNode tab = umap go
  where
    go :: Node -> Node
    go node = case node of
      NTyp TypeConstr {..} ->
        case ii ^. inductiveBuiltin of
          Just (BuiltinTypeInductive BuiltinBool) -> mkTypeBool'
          Just (BuiltinTypeInductive BuiltinNat) -> mkTypeInteger'
          Just (BuiltinTypeInductive BuiltinInt) -> mkTypeInteger'
          Just (BuiltinTypeAxiom BuiltinString) -> mkTypeString'
          _ -> node
        where
          ii = fromJust $ tab ^. infoInductives . at _typeConstrSymbol
      _ -> node

convertBuiltinTypes :: InfoTable -> InfoTable
convertBuiltinTypes tab =
  mapAllNodes (convertNode tab) tab
