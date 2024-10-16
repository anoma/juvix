module Commands.Dev.Tree.Compile.Options
  ( module Commands.Dev.Tree.Compile.Options,
  )
where

import Commands.Compile.Anoma.Options
import Commands.Compile.Cairo.Options
import Commands.Compile.Native.Options
import Commands.Compile.RiscZeroRust.Options
import Commands.Compile.Wasi.Options
import Commands.Dev.DevCompile.Asm.Options
import Commands.Dev.DevCompile.Casm.Options
import Commands.Dev.DevCompile.Reg.Options
import Commands.Extra.Compile.Options
import Commands.Extra.NewCompile
import CommonOptions
import Juvix.Config qualified as Config

data CompileCommand
  = Native (NativeOptions ('InputExtension 'FileExtJuvixTree))
  | Wasi (WasiOptions ('InputExtension 'FileExtJuvixTree))
  | Asm (AsmOptions ('InputExtension 'FileExtJuvixTree))
  | Reg (RegOptions ('InputExtension 'FileExtJuvixTree))
  | Casm (CasmOptions ('InputExtension 'FileExtJuvixTree))
  | Anoma (AnomaOptions ('InputExtension 'FileExtJuvixTree))
  | Cairo (CairoOptions ('InputExtension 'FileExtJuvixTree))
  | RiscZeroRust (RiscZeroRustOptions ('InputExtension 'FileExtJuvixTree))
  deriving stock (Data)

supportedTargets :: [(CompileTarget, Parser CompileCommand)]
supportedTargets =
  [ (AppTargetNative64, Native <$> parseNative),
    (AppTargetAsm, Asm <$> parseAsm),
    (AppTargetReg, Reg <$> parseReg),
    (AppTargetCasm, Casm <$> parseCasm),
    (AppTargetAnoma, Anoma <$> parseAnoma),
    (AppTargetCairo, Cairo <$> parseCairo)
  ]
    <> [(AppTargetWasm32Wasi, Wasi <$> parseWasi) | Config.config ^. Config.configWasm]
    <> [(AppTargetRiscZeroRust, RiscZeroRust <$> parseRiscZeroRust) | Config.config ^. Config.configRust]

parseCompileCommand :: Parser CompileCommand
parseCompileCommand = commandTargetsHelper supportedTargets
