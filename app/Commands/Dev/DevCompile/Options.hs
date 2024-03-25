module Commands.Dev.DevCompile.Options where

import Commands.Dev.DevCompile.Asm.Options
import Commands.Dev.DevCompile.Cairo.Options
import Commands.Dev.DevCompile.Core.Options
import Commands.Dev.DevCompile.Nockma.Options
import Commands.Dev.DevCompile.Reg.Options
import Commands.Dev.DevCompile.Tree.Options
import CommonOptions

data DevCompileCommand
  = Core CoreOptions
  | Asm AsmOptions
  | Reg RegOptions
  | Tree TreeOptions
  | Nockma NockmaOptions
  | Cairo CairoOptions
  deriving stock (Data)

parseDevCompileCommand :: Parser DevCompileCommand
parseDevCompileCommand =
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

commandCore :: Mod CommandFields DevCompileCommand
commandCore =
  command "core" $
    info
      (Core <$> parseCore)
      (progDesc "Compile to Juvix Core")

commandReg :: Mod CommandFields DevCompileCommand
commandReg =
  command "reg" $
    info
      (Reg <$> parseReg)
      (progDesc "Compile to Juvix Reg")

commandTree :: Mod CommandFields DevCompileCommand
commandTree =
  command "tree" $
    info
      (Tree <$> parseTree)
      (progDesc "Compile to Juvix Tree")

commandCairo :: Mod CommandFields DevCompileCommand
commandCairo =
  command "cairo" $
    info
      (Cairo <$> parseCairo)
      (progDesc "Compile to Juvix Cairo")

commandNockma :: Mod CommandFields DevCompileCommand
commandNockma =
  command "nockma" $
    info
      (Nockma <$> parseNockma)
      (progDesc "Compile to Juvix Nockma")

commandAsm :: Mod CommandFields DevCompileCommand
commandAsm =
  command "asm" $
    info
      (Asm <$> parseAsm)
      (progDesc "Compile to Juvix ASM")
