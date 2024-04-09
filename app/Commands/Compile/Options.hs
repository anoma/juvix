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
import CommonOptions

data CompileCommand
  = Native NativeOptions
  | Wasi WasiOptions
  | Geb GebOptions
  | Vampir VampirOptions
  | Anoma AnomaOptions
  | Cairo CairoOptions
  deriving stock (Data)

parseCompileCommand :: Parser CompileCommand
parseCompileCommand =
  hsubparser
    ( mconcat
        [ commandNative,
          commandWasi,
          commandGeb,
          commandVampir,
          commandAnoma,
          commandCairo
        ]
    )

commandNative :: Mod CommandFields CompileCommand
commandNative =
  command "native" $
    info
      (Native <$> parseNative)
      (progDesc "Compile to native code")

commandWasi :: Mod CommandFields CompileCommand
commandWasi =
  command "wasi" $
    info
      (Wasi <$> parseWasi)
      (progDesc "Compile to WASI (WebAssembly System Interface)")

commandGeb :: Mod CommandFields CompileCommand
commandGeb =
  command "geb" $
    info
      (Geb <$> parseGeb)
      (progDesc "Compile to Geb")

commandVampir :: Mod CommandFields CompileCommand
commandVampir =
  command "vampir" $
    info
      (Vampir <$> parseVampir)
      (progDesc "Compile to VampIR")

commandAnoma :: Mod CommandFields CompileCommand
commandAnoma =
  command "anoma" $
    info
      (Anoma <$> parseAnoma)
      (progDesc "Compile to Anoma")

commandCairo :: Mod CommandFields CompileCommand
commandCairo =
  command "cairo" $
    info
      (Cairo <$> parseCairo)
      (progDesc "Compile to Cairo")
