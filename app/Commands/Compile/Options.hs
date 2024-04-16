module Commands.Compile.Options
  ( module Commands.Compile.Options,
  )
where

import Commands.Compile.Anoma.Options
import Commands.Compile.Cairo.Options
import Commands.Compile.Geb.Options
import Commands.Compile.Native.Options
import Commands.Compile.Vampir.Options
import Commands.Compile.Wasi.Options
import Commands.Extra.NewCompile
import CommonOptions

data CompileCommand
  = Native (NativeOptions 'InputMain)
  | Wasi (WasiOptions 'InputMain)
  | Geb (GebOptions 'InputMain)
  | Vampir (VampirOptions 'InputMain)
  | Anoma (AnomaOptions 'InputMain)
  | Cairo (CairoOptions 'InputMain)
  deriving stock (Data)

parseCompileCommand :: Parser CompileCommand
parseCompileCommand = commandTargetsHelper supportedTargets

supportedTargets :: [(CompileTarget, Parser CompileCommand)]
supportedTargets =
  [ (AppTargetVampIR, Vampir <$> parseVampir),
    (AppTargetAnoma, Anoma <$> parseAnoma),
    (AppTargetCairo, Cairo <$> parseCairo),
    (AppTargetGeb, Geb <$> parseGeb),
    (AppTargetWasm32Wasi, Wasi <$> parseWasi),
    (AppTargetNative64, Native <$> parseNative)
  ]
