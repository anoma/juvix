module Commands.Dev.Tree.CompileOld.Base where

import Commands.Base
import Commands.Dev.Tree.CompileOld.Options
import Commands.Extra.Compile qualified as Compile
import Data.Aeson qualified as JSON
import Juvix.Compiler.Asm.Pretty qualified as Asm
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromTree qualified as Nockma
import Juvix.Compiler.Reg.Pretty qualified as Reg
import Juvix.Compiler.Tree.Data.InfoTable qualified as Tree
import Juvix.Prelude.Pretty

data PipelineArg = PipelineArg
  { _pipelineArgOptions :: CompileOptions,
    _pipelineArgFile :: Path Abs File,
    _pipelineArgTable :: Tree.InfoTable
  }

getEntry :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r EntryPoint
getEntry PipelineArg {..} = do
  ep <- getEntryPoint (Just (AppPath (preFileFromAbs _pipelineArgFile) True))
  return
    ep
      { _entryPointTarget = Just (getTarget (_pipelineArgOptions ^. compileTarget)),
        _entryPointDebug = _pipelineArgOptions ^. compileDebug,
        _entryPointUnsafe = _pipelineArgOptions ^. compileUnsafe,
        _entryPointOptimizationLevel = fromMaybe defaultOptLevel (_pipelineArgOptions ^. compileOptimizationLevel),
        _entryPointInliningDepth = _pipelineArgOptions ^. compileInliningDepth
      }
  where
    getTarget :: CompileTarget -> Backend.Target
    getTarget = \case
      AppTargetWasm32Wasi -> Backend.TargetCWasm32Wasi
      AppTargetNative64 -> Backend.TargetCNative64
      AppTargetVampIR -> Backend.TargetVampIR
      AppTargetCore -> Backend.TargetCore
      AppTargetAsm -> Backend.TargetAsm
      AppTargetReg -> Backend.TargetReg
      AppTargetTree -> Backend.TargetTree
      AppTargetAnoma -> Backend.TargetAnoma
      AppTargetCasm -> Backend.TargetCairo
      AppTargetCairo -> Backend.TargetCairo
      AppTargetRiscZeroRust -> Backend.TargetRust

    defaultOptLevel :: Int
    defaultOptLevel
      | _pipelineArgOptions ^. compileDebug = 0
      | otherwise = defaultOptimizationLevel

runCPipeline ::
  forall r.
  (Members '[EmbedIO, App, TaggedLock] r) =>
  PipelineArg ->
  Sem r ()
runCPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  C.MiniCResult {..} <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ treeToMiniC _pipelineArgTable
  cFile <- inputCFile _pipelineArgFile
  writeFileEnsureLn cFile _resultCCode
  outfile <- Compile.outputFile _pipelineArgOptions
  Compile.runCommand
    _pipelineArgOptions
      { _compileInputFile = AppPath (preFileFromAbs cFile) False,
        _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
      }
  where
    inputCFile :: Path Abs File -> Sem r (Path Abs File)
    inputCFile inputFileCompile = do
      buildDir <- askBuildDir
      ensureDir buildDir
      return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))

runAsmPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runAsmPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  asmFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . treeToAsm
      $ _pipelineArgTable
  tab' <- getRight r
  let code = Asm.ppPrint tab' tab'
  writeFileEnsureLn asmFile code

runRegPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runRegPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  regFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . treeToReg
      $ _pipelineArgTable
  tab' <- getRight r
  let code = Reg.ppPrint tab' tab'
  writeFileEnsureLn regFile code

runAnomaPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runAnomaPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  nockmaFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . treeToAnoma
      $ _pipelineArgTable
  res <- getRight r
  outputAnomaResult nockmaFile res

outputAnomaResult :: (Members '[EmbedIO, App] r) => Path Abs File -> Nockma.AnomaResult -> Sem r ()
outputAnomaResult nockmaFile Nockma.AnomaResult {..} = do
  let code = Nockma.ppSerialize _anomaClosure
      prettyNockmaFile = replaceExtensions' [".pretty", ".nockma"] nockmaFile
  writeFileEnsureLn nockmaFile code
  writeFileEnsureLn prettyNockmaFile (Nockma.ppPrint _anomaClosure)

runCasmPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runCasmPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  casmFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . treeToCasm
      $ _pipelineArgTable
  Casm.Result {..} <- getRight r
  writeFileEnsureLn casmFile (toPlainText $ Casm.ppProgram _resultCode)

runCairoPipeline :: (Members '[EmbedIO, App, TaggedLock] r) => PipelineArg -> Sem r ()
runCairoPipeline pa@PipelineArg {..} = do
  entryPoint <- getEntry pa
  cairoFile <- Compile.outputFile _pipelineArgOptions
  r <-
    runReader entryPoint
      . runError @JuvixError
      . treeToCairo
      $ _pipelineArgTable
  res <- getRight r
  liftIO $ JSON.encodeFile (toFilePath cairoFile) res
