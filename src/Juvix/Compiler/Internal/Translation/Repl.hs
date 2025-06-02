module Juvix.Compiler.Internal.Translation.Repl
  ( typeCheckImport,
    typeCheckExpression,
    typeCheckExpressionType,
  )
where

import Juvix.Compiler.Concrete.Data.Highlight
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Store.Scoped.Data.InfoTable (infoBuiltins)
import Juvix.Prelude

typeCheckExpressionType ::
  forall r.
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType exp = do
  table <- extendedTableReplArtifacts exp
  stable <- gets (^. artifactScopeTable)
  runResultBuilderArtifacts
    . runNameIdGenArtifacts
    . evalHighlightBuilder
    . runReader table
    . runReader (stable ^. infoBuiltins)
    . runReader stable
    . withEmptyLocalVars
    . withEmptyInsertedArgsStack
    . mapError (JuvixError @TypeCheckerError)
    . runInferenceDef
    $ inferExpressionRepl Nothing exp
    >>= traverseOf typedType strongNormalize_

typeCheckExpression ::
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r Expression
typeCheckExpression exp = (^. typedExpression) <$> typeCheckExpressionType exp

typeCheckImport :: Import -> Sem r Import
typeCheckImport = return
