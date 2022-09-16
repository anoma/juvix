module Juvix.Compiler.Core.Translation.FromInternal where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Data.Literal (LiteralLoc)
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Translation.FromInternal.Data
import Juvix.Compiler.Internal.Translation qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking qualified as InternalTyped
import Juvix.Extra.Strings qualified as Str

unsupported :: Text -> a
unsupported thing = error ("Internal to Core: Not yet supported: " <> thing)

fromInternal :: Internal.InternalTypedResult -> Sem k CoreResult
fromInternal i = do
  CoreResult . fst <$> runInfoTableBuilder emptyInfoTable f
  where
    f :: forall r. Members '[InfoTableBuilder] r => Sem r ()
    f = mapM_ coreModule (toList (i ^. InternalTyped.resultModules))
      where
        coreModule :: Internal.Module -> Sem r ()
        coreModule m = registerFunctionDefs m

registerFunctionDefs :: forall r. Members '[InfoTableBuilder] r => Internal.Module -> Sem r ()
registerFunctionDefs m = registerFunctionDefsBody (m ^. Internal.moduleBody)

registerFunctionDefsBody :: forall r. Members '[InfoTableBuilder] r => Internal.ModuleBody -> Sem r ()
registerFunctionDefsBody body = mapM_ go (body ^. Internal.moduleStatements)
  where
    go :: Internal.Statement -> Sem r ()
    go = \case
      Internal.StatementFunction f -> goFunctionDef f
      _ -> return ()

goFunctionDef ::
  forall r.
  Members '[InfoTableBuilder] r =>
  Internal.FunctionDef ->
  Sem r ()
goFunctionDef f
  | isJust (f ^. Internal.funDefBuiltin) = return ()
  | otherwise = do
      sym <- freshSymbol
      let info =
            IdentifierInfo
              { _identifierName = Just (f ^. Internal.funDefName),
                _identifierSymbol = sym,
                _identifierType = mkDynamic',
                _identifierArgsNum = 0,
                _identifierArgsInfo = [],
                _identifierIsExported = False
              }
      registerIdent info
      when (f ^. Internal.funDefName . Internal.nameText == Str.main) (registerMain sym)
      mapM_ (goFunctionClause sym) (f ^. Internal.funDefClauses)

goFunctionClause ::
  forall r.
  Members '[InfoTableBuilder] r =>
  Symbol ->
  Internal.FunctionClause ->
  Sem r ()
goFunctionClause sym clause = do
  body <- goExpression 0 HashMap.empty (clause ^. Internal.clauseBody)
  registerIdentNode sym body

goExpression ::
  Index ->
  HashMap Text Index ->
  Internal.Expression ->
  Sem r Node
goExpression _ _ = \case
  Internal.ExpressionLiteral l -> return (goLiteral l)
  x -> unsupported ("non-literal expressions" <> show (getLoc x))

goLiteral :: LiteralLoc -> Node
goLiteral l = case l ^. withLocParam of
  Internal.LitString s -> mkConstant (Info.singleton (LocationInfo (l ^. withLocInt))) (ConstString s)
  Internal.LitInteger i -> mkConstant (Info.singleton (LocationInfo (l ^. withLocInt))) (ConstInteger i)
