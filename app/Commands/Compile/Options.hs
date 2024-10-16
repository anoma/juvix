module Commands.Compile.Options
  ( module Commands.Compile.Options,
  )
where

import Commands.Compile.Anoma.Options
import Commands.Compile.Cairo.Options
import Commands.Compile.Native.Options
import Commands.Compile.RiscZeroRust.Options
import Commands.Compile.Wasi.Options
import Commands.Extra.NewCompile
import CommonOptions
import Juvix.Config qualified as Config

data CompileCommand
  = Native (NativeOptions 'InputMain)
  | Wasi (WasiOptions 'InputMain)
  | Anoma (AnomaOptions 'InputMain)
  | Cairo (CairoOptions 'InputMain)
  | RiscZeroRust (RiscZeroRustOptions 'InputMain)
  deriving stock (Data)

parseCompileCommand :: Parser CompileCommand
parseCompileCommand = commandTargetsHelper supportedTargets

supportedTargets :: [(CompileTarget, Parser CompileCommand)]
supportedTargets =
  [ (AppTargetAnoma, Anoma <$> parseAnoma),
    (AppTargetCairo, Cairo <$> parseCairo),
    (AppTargetNative64, Native <$> parseNative)
  ]
    <> [(AppTargetWasm32Wasi, Wasi <$> parseWasi) | Config.config ^. Config.configWasm]
    <> [(AppTargetRiscZeroRust, RiscZeroRust <$> parseRiscZeroRust) | Config.config ^. Config.configRust]
