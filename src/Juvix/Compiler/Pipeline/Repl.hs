module Juvix.Compiler.Pipeline.Repl where

import Juvix.Compiler.Abstract.Translation qualified as Abstract
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder.BuilderState qualified as C
import Juvix.Compiler.Concrete.Data.Scope qualified as Scoper
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

arityCheckExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Internal.Expression
arityCheckExpression p = do
  scopeTable <- gets (^. artifactScopeTable)
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
      . runScoperScopeArtifacts
    )
    $ Scoper.scopeCheckExpression scopeTable p
      >>= Abstract.fromConcreteExpression
      >>= Internal.fromAbstractExpression
      >>= Internal.arityCheckExpression

expressionUpToAtomsParsed ::
  Members '[State Artifacts, Error JuvixError] r =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Parsed)
expressionUpToAtomsParsed fp txt =
  runNameIdGenArtifacts
    . runBuiltinsArtifacts
    $ Parser.expressionFromTextSource fp txt

expressionUpToAtomsScoped ::
  Members '[State Artifacts, Error JuvixError] r =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Scoped)
expressionUpToAtomsScoped fp txt = do
  scopeTable <- gets (^. artifactScopeTable)
  runNameIdGenArtifacts
    . runBuiltinsArtifacts
    . runScoperScopeArtifacts
    $ Parser.expressionFromTextSource fp txt
      >>= Scoper.scopeCheckExpressionAtoms scopeTable

scopeCheckExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression p = do
  scopeTable <- gets (^. artifactScopeTable)
  runNameIdGenArtifacts
    . runBuiltinsArtifacts
    . runScoperScopeArtifacts
    . Scoper.scopeCheckExpression scopeTable
    $ p

openImportToInternal ::
  Members '[Reader EntryPoint, Error JuvixError, State Artifacts] r =>
  OpenModule 'Parsed ->
  Sem r (Maybe Internal.Include)
openImportToInternal o = do
  parsedModules <- gets (^. artifactParsing . C.stateModules)
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
      . runAbstractInfoTableBuilderArtifacts
      . runScoperInfoTableBuilderArtifacts
      . runScoperScopeArtifacts
      . runStateArtifacts artifactInternalTranslationState
      . runReaderArtifacts artifactScopeExports
      . runReader (Scoper.ScopeParameters mempty parsedModules)
      . runStateArtifacts artifactAbstractModuleCache
      . runStateArtifacts artifactScoperState
    )
    $ do
      mTopModule <-
        Scoper.scopeCheckOpenModule o
          >>= Abstract.fromConcreteOpenImport
      case mTopModule of
        Nothing -> return Nothing
        Just m -> Internal.fromAbstractImport m

importToInternal ::
  Members '[Reader EntryPoint, Error JuvixError, State Artifacts] r =>
  Import 'Parsed ->
  Sem r (Maybe Internal.Include)
importToInternal i = do
  parsedModules <- gets (^. artifactParsing . C.stateModules)
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
      . runAbstractInfoTableBuilderArtifacts
      . runScoperInfoTableBuilderArtifacts
      . runScoperScopeArtifacts
      . runStateArtifacts artifactInternalTranslationState
      . runReaderArtifacts artifactScopeExports
      . runReader (Scoper.ScopeParameters mempty parsedModules)
      . runStateArtifacts artifactAbstractModuleCache
      . runStateArtifacts artifactScoperState
    )
    $ Scoper.scopeCheckImport i
      >>= Abstract.fromConcreteImport
      >>= Internal.fromAbstractImport

importToInternal' ::
  Members '[Reader EntryPoint, Error JuvixError, State Artifacts] r =>
  Internal.Include ->
  Sem r Internal.Include
importToInternal' = Internal.arityCheckInclude >=> Internal.typeCheckInclude

parseReplInput ::
  Members '[PathResolver, Files, State Artifacts, Error JuvixError] r =>
  Path Abs File ->
  Text ->
  Sem r Parser.ReplInput
parseReplInput fp txt =
  ( runNameIdGenArtifacts
      . runBuiltinsArtifacts
      . runParserInfoTableBuilderArtifacts
  )
    $ Parser.replInputFromTextSource fp txt

expressionUpToTyped ::
  Members '[Error JuvixError, State Artifacts] r =>
  Path Abs File ->
  Text ->
  Sem r Internal.TypedExpression
expressionUpToTyped fp txt = do
  p <- expressionUpToAtomsParsed fp txt
  arityCheckExpression p
    >>= Internal.typeCheckExpressionType

compileExpression ::
  Members '[Error JuvixError, State Artifacts] r =>
  ExpressionAtoms 'Parsed ->
  Sem r Core.Node
compileExpression p = do
  arityCheckExpression p
    >>= Internal.typeCheckExpression
    >>= fromInternalExpression

registerImport ::
  Members '[Error JuvixError, State Artifacts, Reader EntryPoint] r =>
  Import 'Parsed ->
  Sem r ()
registerImport i = do
  mInclude <- importToInternal i
  whenJust mInclude (importToInternal' >=> fromInternalInclude)

registerOpenImport ::
  Members '[Error JuvixError, State Artifacts, Reader EntryPoint] r =>
  OpenModule 'Parsed ->
  Sem r ()
registerOpenImport o = do
  mInclude <- openImportToInternal o
  whenJust mInclude (importToInternal' >=> fromInternalInclude)

fromInternalInclude :: Members '[State Artifacts] r => Internal.Include -> Sem r ()
fromInternalInclude i = do
  let table = Internal.buildTable [i ^. Internal.includeModule]
  runReader table
    . runCoreInfoTableBuilderArtifacts
    . runFunctionsTableArtifacts
    . readerTypesTableArtifacts
    . runReader Core.initIndexTable
    $ Core.goModule (i ^. Internal.includeModule)

fromInternalExpression :: Members '[State Artifacts] r => Internal.Expression -> Sem r Core.Node
fromInternalExpression exp = do
  typedTable <- gets (^. artifactInternalTypedTable)
  runReader typedTable
    . tmpCoreInfoTableBuilderArtifacts
    . runFunctionsTableArtifacts
    . readerTypesTableArtifacts
    . runReader Core.initIndexTable
    $ Core.goExpression exp

data ReplPipelineResult
  = ReplPipelineResultNode Core.Node
  | ReplPipelineResultImport TopModulePath
  | ReplPipelineResultOpenImport Name

compileReplInputIO ::
  Members '[Reader EntryPoint, State Artifacts, Embed IO] r =>
  Path Abs File ->
  Text ->
  Sem r (Either JuvixError ReplPipelineResult)
compileReplInputIO fp txt =
  runError
    . runFilesIO
    . runPathResolverArtifacts
    $ do
      p <- parseReplInput fp txt
      case p of
        Parser.ReplExpression e -> ReplPipelineResultNode <$> compileExpression e
        Parser.ReplImport i -> registerImport i $> ReplPipelineResultImport (i ^. importModule)
        Parser.ReplOpenImport i -> registerOpenImport i $> ReplPipelineResultOpenImport (i ^. openModuleName)

expressionUpToTypedIO ::
  Members '[State Artifacts, Embed IO] r =>
  Path Abs File ->
  Text ->
  Sem r (Either JuvixError Internal.TypedExpression)
expressionUpToTypedIO fp txt = runError (expressionUpToTyped fp txt)
