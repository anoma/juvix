module Commands.Dev.InternalCompile.Options where

import Commands.Dev.InternalCompile.Asm.Options
import Commands.Dev.InternalCompile.Cairo.Options
import Commands.Dev.InternalCompile.Core.Options
import Commands.Dev.InternalCompile.Nockma.Options
import Commands.Dev.InternalCompile.Reg.Options
import Commands.Dev.InternalCompile.Tree.Options
import CommonOptions

data InternalCompileCommand
  = Core CoreOptions
  | Asm AsmOptions
  | Reg RegOptions
  | Tree TreeOptions
  | Nockma NockmaOptions
  | Cairo CairoOptions
  deriving stock (Data)

parseInternalCompileCommand :: Parser InternalCompileCommand
parseInternalCompileCommand =
  hsubparser
    ( mconcat
        [ commandCore,
          commandReg,
          commandTree,
          commandCairo,
          commandNockma,
          commandAsm
        ]
    )

commandCore :: Mod CommandFields InternalCompileCommand
commandCore =
  command "core" $
    info
      (Core <$> parseCore)
      (progDesc "Compile to Juvix Core")

commandReg :: Mod CommandFields InternalCompileCommand
commandReg =
  command "reg" $
    info
      (Reg <$> parseReg)
      (progDesc "Compile to Juvix Reg")

commandTree :: Mod CommandFields InternalCompileCommand
commandTree =
  command "tree" $
    info
      (Tree <$> parseTree)
      (progDesc "Compile to Juvix Tree")

commandCairo :: Mod CommandFields InternalCompileCommand
commandCairo =
  command "cairo" $
    info
      (Cairo <$> parseCairo)
      (progDesc "Compile to Juvix Cairo")

commandNockma :: Mod CommandFields InternalCompileCommand
commandNockma =
  command "nockma" $
    info
      (Nockma <$> parseNockma)
      (progDesc "Compile to Juvix Nockma")

commandAsm :: Mod CommandFields InternalCompileCommand
commandAsm =
  command "asm" $
    info
      (Asm <$> parseAsm)
      (progDesc "Compile to Juvix ASM")
