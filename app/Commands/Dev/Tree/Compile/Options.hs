module Commands.Dev.Tree.Compile.Options
  ( module Commands.Dev.Tree.Compile.Options,
  )
where

import Commands.Compile.Anoma.Options
import Commands.Compile.Cairo.Options
import Commands.Compile.Native.Options
import Commands.Compile.Wasi.Options
import Commands.Dev.DevCompile.Asm.Options
import Commands.Dev.DevCompile.Casm.Options
import Commands.Dev.DevCompile.Reg.Options
import Commands.Extra.Compile.Options
import Commands.Extra.NewCompile
import CommonOptions

data CompileCommand
  = Native NativeOptions
  | Wasi WasiOptions
  | Asm AsmOptions -- TODO update options to parse .jvt
  | Reg RegOptions -- TODO update options to parse .jvt
  | Casm CasmOptions -- TODO update options to parse .jvt
  | Anoma AnomaOptions
  | Cairo CairoOptions
  deriving stock (Data)

treeSupportedTargets :: SupportedTargets
treeSupportedTargets =
  AppTargetNative64
    :| [ AppTargetWasm32Wasi,
         AppTargetAsm,
         AppTargetReg,
         AppTargetCasm,
         AppTargetCairo,
         AppTargetAnoma
       ]

supportedTargets :: [(CompileTarget, Parser CompileCommand)]
supportedTargets =
  [ (AppTargetNative64, Native <$> parseNative),
    (AppTargetWasm32Wasi, Wasi <$> parseWasi),
    (AppTargetAsm, Asm <$> parseAsm),
    (AppTargetReg, Reg <$> parseReg),
    (AppTargetCasm, Casm <$> parseCasm),
    (AppTargetAnoma, Anoma <$> parseAnoma),
    (AppTargetCairo, Cairo <$> parseCairo)
  ]

parseCompileCommand :: Parser CompileCommand
parseCompileCommand = commandTargetsHelper supportedTargets
