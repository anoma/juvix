module Juvix.Compiler.Pipeline.ExpressionContext where

import Data.HashMap.Strict
import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Concrete.Data.InfoTable qualified as Scoper
import Juvix.Compiler.Concrete.Data.Scope qualified as S
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language qualified as C
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra.Base (mkDynamic')
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Context qualified as InternalArity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as InternalTyped

data ExpressionContext = ExpressionContext
  { _contextInternalTypedResult :: InternalTyped.InternalTypedResult,
    _contextInternalResult :: Internal.InternalResult,
    _contextScoperResult :: Scoper.ScoperResult,
    _contextScoperTable :: Scoper.InfoTable,
    _contextCoreResult :: Core.CoreResult
  }

expressionContext :: Core.CoreResult -> ExpressionContext
expressionContext _contextCoreResult = ExpressionContext {..}
  where
    _contextInternalTypedResult :: InternalTyped.InternalTypedResult
    _contextInternalTypedResult = _contextCoreResult ^. Core.coreResultInternalTypedResult

    _contextInternalResult :: Internal.InternalResult
    _contextInternalResult =
      _contextInternalTypedResult
        ^. InternalTyped.resultInternalArityResult
          . InternalArity.resultInternalResult

    _contextScoperResult :: Scoper.ScoperResult
    _contextScoperResult =
      _contextInternalResult
        ^. Internal.resultAbstract
          . Abstract.resultScoper

    _contextScoperTable :: Scoper.InfoTable
    _contextScoperTable =
      _contextScoperResult
        ^. Scoper.resultScoperTable

makeLenses ''ExpressionContext

moduleScope :: ExpressionContext -> C.TopModulePath -> Maybe S.Scope
moduleScope e p = e ^. contextScoperResult ^?! Scoper.resultScope . at p

mainModuleScope :: ExpressionContext -> S.Scope
mainModuleScope e = fromJust (moduleScope e (mainModuleTopPath e))

mainModuleTopPath :: ExpressionContext -> C.TopModulePath
mainModuleTopPath = (^. contextScoperResult . Scoper.mainModule . C.modulePath . S.nameConcrete)

runTransformations :: Member (Error JuvixError) r => [TransformationId] -> InfoTable -> Node -> Sem r (InfoTable, Node)
runTransformations ts tab n = snd <$> runInfoTableBuilder tab e
  where
    e = do
      sym <- freshSymbol
      registerIdentNode sym n
      -- `n` will get filtered out by the transformations unless it has a
      -- corresponding entry in `infoIdentifiers`
      let name = freshIdentName tab "_repl"
          ii =
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
      registerIdent name ii
      tab0 <- getInfoTable
      tab' <- applyTransformations ts tab0
      let node' = lookupDefault impossible sym (tab' ^. identContext)
      return (tab', node')
