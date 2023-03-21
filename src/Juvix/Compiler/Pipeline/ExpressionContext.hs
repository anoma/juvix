module Juvix.Compiler.Pipeline.ExpressionContext where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Scope qualified as S
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra.Base (mkDynamic')
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Pipeline.Artifacts

mainModuleScope :: Artifacts -> S.Scope
mainModuleScope e = e ^?! artifactMainModuleScope . _Just

runTransformations ::
  Members '[State Artifacts, Error JuvixError] r =>
  [TransformationId] ->
  Node ->
  Sem r Node
runTransformations ts n = runCoreInfoTableBuilderArtifacts $ do
  sym <- freshSymbol
  registerIdentNode sym n
  -- `n` will get filtered out by the transformations unless it has a
  -- corresponding entry in `infoIdentifiers`
  tab <- getInfoTable
  let name = freshIdentName tab "_repl"
      idenInfo =
        IdentifierInfo
          { _identifierName = name,
            _identifierSymbol = sym,
            _identifierLocation = Nothing,
            _identifierArgsNum = 0,
            _identifierArgsInfo = [],
            _identifierType = mkDynamic',
            _identifierIsExported = False,
            _identifierBuiltin = Nothing
          }
  registerIdent name idenInfo
  tab' <- getInfoTable >>= applyTransformations ts
  modify' (set artifactCoreTable tab')
  let node' = HashMap.lookupDefault impossible sym (tab' ^. identContext)
  return node'
