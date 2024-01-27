module Commands.Dev.Core.Compile.Base where

import Commands.Base
import Commands.Dev.Core.Compile.Options
import Commands.Extra.Compile qualified as Compile
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Backend.Geb qualified as Geb
import Juvix.Compiler.Backend.VampIR.Translation qualified as VampIR
import Juvix.Compiler.Core.Data.Module qualified as Core
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Tree.Pretty qualified as Tree
import System.FilePath (takeBaseName)

data PipelineArg = PipelineArg
  { _pipelineArgOptions :: CompileOptions,
    _pipelineArgFile :: Path Abs File,
    _pipelineArgModule :: Core.Module
  }

getEntry :: (Members '[Embed IO, App, TaggedLock] r) => PipelineArg -> Sem r EntryPoint
getEntry PipelineArg {..} = do
  ep <- getEntryPoint (AppPath (preFileFromAbs _pipelineArgFile) True)
  return $
    ep
      { _entryPointTarget = getTarget (_pipelineArgOptions ^. compileTarget),
        _entryPointDebug = _pipelineArgOptions ^. compileDebug,
        _entryPointUnsafe = _pipelineArgOptions ^. compileUnsafe,
        _entryPointOptimizationLevel = fromMaybe defaultOptLevel (_pipelineArgOptions ^. compileOptimizationLevel),
        _entryPointInliningDepth = _pipelineArgOptions ^. compileInliningDepth
      }
  where
    getTarget :: CompileTarget -> Backend.Target
    getTarget = \case
      TargetWasm32Wasi -> Backend.TargetCWasm32Wasi
      TargetNative64 -> Backend.TargetCNative64
      TargetGeb -> Backend.TargetGeb
      TargetVampIR -> Backend.TargetVampIR
      TargetCore -> Backend.TargetCore
      TargetAsm -> Backend.TargetAsm
      TargetTree -> Backend.TargetTree
      TargetNockma -> Backend.TargetNockma

    defaultOptLevel :: Int
    defaultOptLevel
      | _pipelineArgOptions ^. compileDebug = 0
      | otherwise = defaultOptimizationLevel

runCPipeline ::
  forall r.
  (Members '[Embed IO, App, TaggedLock] r) =>
  PipelineArg ->
  Sem r ()
runCPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  C.MiniCResult {..} <- getRight (run (runReader entryPoint (runError (coreToMiniC _pipelineArgModule :: Sem '[Error JuvixError, Reader EntryPoint] C.MiniCResult))))
  cFile <- inputCFile _pipelineArgFile
  writeFileEnsureLn cFile _resultCCode
  outfile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  Compile.runCommand
    _pipelineArgOptions
      { _compileInputFile = Just (AppPath (preFileFromAbs cFile) False),
        _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
      }
  where
    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))

runGebPipeline ::
  forall r.
  (Members '[Embed IO, App, TaggedLock] r) =>
  PipelineArg ->
  Sem r ()
runGebPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  gebFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  let spec
        | _pipelineArgOptions ^. compileTerm = Geb.OnlyTerm
        | otherwise =
            Geb.LispPackage
              Geb.LispPackageSpec
                { _lispPackageName = fromString $ takeBaseName $ toFilePath gebFile,
                  _lispPackageEntry = "*entry*"
                }
  Geb.Result {..} <- getRight (run (runReader entryPoint (runError (coreToGeb spec _pipelineArgModule :: Sem '[Error JuvixError, Reader EntryPoint] Geb.Result))))
  writeFileEnsureLn gebFile _resultCode

runVampIRPipeline ::
  forall r.
  (Members '[Embed IO, App, TaggedLock] r) =>
  PipelineArg ->
  Sem r ()
runVampIRPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  vampirFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  VampIR.Result {..} <- getRight (run (runReader entryPoint (runError (coreToVampIR _pipelineArgModule :: Sem '[Error JuvixError, Reader EntryPoint] VampIR.Result))))
  writeFileEnsureLn vampirFile _resultCode

runAsmPipeline :: (Members '[Embed IO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runAsmPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  asmFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToAsm
      $ _pipelineArgModule
  tab' <- getRight r
  let code = Asm.ppPrint tab' tab'
  writeFileEnsureLn asmFile code

runTreePipeline :: (Members '[Embed IO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runTreePipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  treeFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToTree
      $ _pipelineArgModule
  tab' <- getRight r
  let code = Tree.ppPrint tab' tab'
  writeFileEnsureLn treeFile code

runNockmaPipeline :: (Members '[Embed IO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runNockmaPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  nockmaFile <- Compile.outputFile _pipelineArgOptions _pipelineArgFile
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToNockma
      $ _pipelineArgModule
  tab' <- getRight r
  let code = Nockma.ppSerialize tab'
  writeFileEnsureLn nockmaFile code
