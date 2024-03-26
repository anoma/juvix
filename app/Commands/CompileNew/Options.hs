module Commands.CompileNew.Options
  ( module Commands.CompileNew.Options,
  )
where

import Commands.CompileNew.Anoma.Options
import Commands.CompileNew.Cairo.Options
import Commands.CompileNew.Geb.Options
import Commands.CompileNew.Native.Options
import Commands.CompileNew.Vampir.Options
import Commands.CompileNew.Wasi.Options
import CommonOptions

data CompileNewCommand
  = Native NativeOptions
  | Wasi WasiOptions
  | Geb GebOptions
  | Vampir VampirOptions
  | Anoma AnomaOptions
  | Cairo CairoOptions
  deriving stock (Data)

parseCompileNewCommand :: Parser CompileNewCommand
parseCompileNewCommand =
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

commandNative :: Mod CommandFields CompileNewCommand
commandNative =
  command "native" $
    info
      (Native <$> parseNative)
      (progDesc "Compile to native code")

commandWasi :: Mod CommandFields CompileNewCommand
commandWasi =
  command "wasi" $
    info
      (Wasi <$> parseWasi)
      (progDesc "Compile to WASI (WebAssembly System Interface)")

commandGeb :: Mod CommandFields CompileNewCommand
commandGeb =
  command "geb" $
    info
      (Geb <$> parseGeb)
      (progDesc "Compile to Geb")

commandVampir :: Mod CommandFields CompileNewCommand
commandVampir =
  command "vampir" $
    info
      (Vampir <$> parseVampir)
      (progDesc "Compile to VampIR")

commandAnoma :: Mod CommandFields CompileNewCommand
commandAnoma =
  command "anoma" $
    info
      (Anoma <$> parseAnoma)
      (progDesc "Compile to Anoma")

commandCairo :: Mod CommandFields CompileNewCommand
commandCairo =
  command "cairo" $
    info
      (Cairo <$> parseCairo)
      (progDesc "Compile to Cairo")
