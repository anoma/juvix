module Commands.Dev.DevCompile.Options where

import Commands.Dev.DevCompile.Asm.Options
import Commands.Dev.DevCompile.Casm.Options
import Commands.Dev.DevCompile.Core.Options
import Commands.Dev.DevCompile.Reg.Options
import Commands.Dev.DevCompile.Tree.Options
import CommonOptions

data DevCompileCommand
  = Core CoreOptions
  | Asm AsmOptions
  | Reg RegOptions
  | Tree TreeOptions
  | Casm CasmOptions
  deriving stock (Data)

parseDevCompileCommand :: Parser DevCompileCommand
parseDevCompileCommand =
  hsubparser
    ( mconcat
        [ commandCore,
          commandReg,
          commandTree,
          commandCasm,
          commandAsm
        ]
    )

commandCore :: Mod CommandFields DevCompileCommand
commandCore =
  command "core" $
    info
      (Core <$> parseCoreOptions)
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

commandCasm :: Mod CommandFields DevCompileCommand
commandCasm =
  command "casm" $
    info
      (Casm <$> parseCasm)
      (progDesc "Compile to Juvix Casm")

commandAsm :: Mod CommandFields DevCompileCommand
commandAsm =
  command "asm" $
    info
      (Asm <$> parseAsm)
      (progDesc "Compile to Juvix ASM")
