module Juvix.Compiler.Internal.Translation.Repl
  ( typeCheckImport,
    typeCheckExpression,
    typeCheckExpressionType,
  )
where

import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal
import Juvix.Compiler.Pipeline.Artifacts

typeCheckExpressionType ::
  forall r.
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r TypedExpression
typeCheckExpressionType exp = do
  table <- extendedTableReplArtifacts exp
  runResultBuilderArtifacts
    . runBuiltinsArtifacts
    . runNameIdGenArtifacts
    . ignoreHighlightBuilder
    . runReader table
    . withEmptyLocalVars
    . withEmptyInsertedArgsStack
    . mapError (JuvixError @TypeCheckerError)
    . runInferenceDef
    $ inferExpression Nothing exp
      >>= traverseOf typedType strongNormalize_

typeCheckExpression ::
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  Expression ->
  Sem r Expression
typeCheckExpression exp = (^. typedExpression) <$> typeCheckExpressionType exp

typeCheckImport :: Import -> Sem r Import
typeCheckImport = return
