module Juvix.Compiler.Pipeline.Repl where

import Juvix.Compiler.Builtins (Builtins)
import Juvix.Compiler.Concrete.Data.InfoTableBuilder qualified as Concrete
import Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder.BuilderState qualified as C
import Juvix.Compiler.Concrete.Data.Scope qualified as Scoper
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed qualified as Scoper
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Concrete.Translation.FromSource qualified as Parser
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Internal qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete qualified as FromConcrete
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.Artifacts
import Juvix.Compiler.Pipeline.Artifacts.PathResolver
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Data.Effect.Git.Process
import Juvix.Data.Effect.Git.Process.Error
import Juvix.Data.Effect.Process (runProcessIO)
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

arityCheckExpression ::
  (Members '[Error JuvixError, State Artifacts, Termination] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Internal.Expression
arityCheckExpression p = do
  scopeTable <- gets (^. artifactScopeTable)
  runBuiltinsArtifacts
    . runScoperScopeArtifacts
    . runStateArtifacts artifactScoperState
    $ runNameIdGenArtifacts (Scoper.scopeCheckExpression scopeTable p)
      >>= runNameIdGenArtifacts . Internal.fromConcreteExpression
      >>= Internal.arityCheckExpression

expressionUpToAtomsParsed ::
  (Members '[State Artifacts, Error JuvixError] r) =>
  Path Abs File ->
  Text ->
  Sem r (ExpressionAtoms 'Parsed)
expressionUpToAtomsParsed fp txt =
  runNameIdGenArtifacts
    . runBuiltinsArtifacts
    $ Parser.expressionFromTextSource fp txt

expressionUpToAtomsScoped ::
  (Members '[State Artifacts, Error JuvixError] r) =>
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
  (Members '[Error JuvixError, State Artifacts] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Expression
scopeCheckExpression p = do
  scopeTable <- gets (^. artifactScopeTable)
  runNameIdGenArtifacts
    . runBuiltinsArtifacts
    . runScoperScopeArtifacts
    . runStateArtifacts artifactScoperState
    . Scoper.scopeCheckExpression scopeTable
    $ p

runToInternal ::
  (Members '[Reader EntryPoint, State Artifacts, Error JuvixError] r) =>
  Sem
    ( State Scoper.ScoperState
        ': FromConcrete.MCache
        ': Reader Scoper.ScopeParameters
        ': Reader (HashSet NameId)
        ': State Scoper.Scope
        ': Concrete.InfoTableBuilder
        ': Builtins
        ': NameIdGen
        ': r
    )
    b ->
  Sem r b
runToInternal m = do
  parsedModules <- gets (^. artifactParsing . C.stateModules)
  runNameIdGenArtifacts
    . runBuiltinsArtifacts
    . runScoperInfoTableBuilderArtifacts
    . runScoperScopeArtifacts
    . runReaderArtifacts artifactScopeExports
    . runReader (Scoper.ScopeParameters mempty parsedModules)
    . runFromConcreteCache
    . runStateArtifacts artifactScoperState
    $ m

importToInternal ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts, Termination] r) =>
  Import 'Parsed ->
  Sem r Internal.Import
importToInternal i = runToInternal $ do
  Scoper.scopeCheckImport i
    >>= Internal.fromConcreteImport

importToInternalTyped ::
  (Members '[Reader EntryPoint, Error JuvixError, State Artifacts, Termination] r) =>
  Internal.Import ->
  Sem r Internal.Import
importToInternalTyped = Internal.arityCheckImport >=> Internal.typeCheckImport

parseReplInput ::
  (Members '[PathResolver, Files, State Artifacts, Error JuvixError] r) =>
  Path Abs File ->
  Text ->
  Sem r Parser.ReplInput
parseReplInput fp txt =
  runNameIdGenArtifacts
    . runBuiltinsArtifacts
    . runParserInfoTableBuilderArtifacts
    $ Parser.replInputFromTextSource fp txt

expressionUpToTyped ::
  (Members '[Error JuvixError, State Artifacts] r) =>
  Path Abs File ->
  Text ->
  Sem r Internal.TypedExpression
expressionUpToTyped fp txt = do
  p <- expressionUpToAtomsParsed fp txt
  runTerminationArtifacts
    ( arityCheckExpression p
        >>= Internal.typeCheckExpressionType
    )

compileExpression ::
  (Members '[Error JuvixError, State Artifacts] r) =>
  ExpressionAtoms 'Parsed ->
  Sem r Core.Node
compileExpression p =
  runTerminationArtifacts
    ( arityCheckExpression p
        >>= Internal.typeCheckExpression
    )
    >>= fromInternalExpression

registerImport ::
  (Members '[Error JuvixError, State Artifacts, Reader EntryPoint] r) =>
  Import 'Parsed ->
  Sem r ()
registerImport p =
  runTerminationArtifacts
    ( importToInternal p
        >>= importToInternalTyped
    )
    >>= fromInternalImport

fromInternalImport :: (Members '[State Artifacts] r) => Internal.Import -> Sem r ()
fromInternalImport i = do
  artiTable <- gets (^. artifactInternalTypedTable)
  let table = Internal.buildTable [i ^. Internal.importModule . Internal.moduleIxModule] <> artiTable
  runNameIdGenArtifacts
    . runReader table
    . runCoreInfoTableBuilderArtifacts
    . runFunctionsTableArtifacts
    . readerTypesTableArtifacts
    . runReader Core.initIndexTable
    -- TODO add cache in Artifacts
    . evalVisitEmpty Core.goModuleNoVisit
    $ Core.goModule (i ^. Internal.importModule . Internal.moduleIxModule)

fromInternalExpression :: (Members '[State Artifacts] r) => Internal.Expression -> Sem r Core.Node
fromInternalExpression exp = do
  typedTable <- gets (^. artifactInternalTypedTable)
  runNameIdGenArtifacts
    . runReader typedTable
    . tmpCoreInfoTableBuilderArtifacts
    . runFunctionsTableArtifacts
    . readerTypesTableArtifacts
    . runReader Core.initIndexTable
    $ Core.goExpression exp

data ReplPipelineResult
  = ReplPipelineResultNode Core.Node
  | ReplPipelineResultImport TopModulePath
  | ReplPipelineResultOpen Name

compileReplInputIO ::
  (Members '[Reader EntryPoint, State Artifacts, Embed IO] r) =>
  Path Abs File ->
  Text ->
  Sem r (Either JuvixError ReplPipelineResult)
compileReplInputIO fp txt = do
  hasInternet <- not <$> asks (^. entryPointOffline)
  runError
    . evalInternet hasInternet
    . runTaggedLockPermissive
    . runLogIO
    . runFilesIO
    . mapError (JuvixError @GitProcessError)
    . runProcessIO
    . runGitProcess
    . mapError (JuvixError @DependencyError)
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    . runPathResolverArtifacts
    $ do
      p <- parseReplInput fp txt
      case p of
        Parser.ReplExpression e -> ReplPipelineResultNode <$> compileExpression e
        Parser.ReplImport i -> registerImport i $> ReplPipelineResultImport (i ^. importModule)
        Parser.ReplOpenImport i -> return (ReplPipelineResultOpen (i ^. openModuleName))

expressionUpToTypedIO ::
  (Members '[State Artifacts, Embed IO] r) =>
  Path Abs File ->
  Text ->
  Sem r (Either JuvixError Internal.TypedExpression)
expressionUpToTypedIO fp txt = runError (expressionUpToTyped fp txt)
