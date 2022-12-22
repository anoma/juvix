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
      NTyp (TypeConstr {..}) ->
        case ii ^. inductiveBuiltin of
          Just BuiltinBool -> mkTypeBool'
          _ -> node
        where
          ii = fromJust $ tab ^. infoInductives . at _typeConstrSymbol
      _ -> node

convertIdent :: InfoTable -> IdentifierInfo -> IdentifierInfo
convertIdent tab ii =
  ii
    { _identifierType = convertNode tab (ii ^. identifierType),
      _identifierArgsInfo = map (over argumentType (convertNode tab)) (ii ^. identifierArgsInfo)
    }

convertConstructor :: InfoTable -> ConstructorInfo -> ConstructorInfo
convertConstructor tab = over constructorType (convertNode tab)

convertInductive :: InfoTable -> InductiveInfo -> InductiveInfo
convertInductive tab ii =
  ii
    { _inductiveKind = convertNode tab (ii ^. inductiveKind),
      _inductiveParams = map (over paramKind (convertNode tab)) (ii ^. inductiveParams),
      _inductiveConstructors = map (convertConstructor tab) (ii ^. inductiveConstructors)
    }

convertAxiom :: InfoTable -> AxiomInfo -> AxiomInfo
convertAxiom tab = over axiomType (convertNode tab)

convertBuiltinTypes :: InfoTable -> InfoTable
convertBuiltinTypes tab =
  mapAxioms (convertAxiom tab) $
    mapInductives (convertInductive tab) $
      mapConstructors (convertConstructor tab) $
        mapIdents (convertIdent tab) $
          mapT (const (convertNode tab)) tab
