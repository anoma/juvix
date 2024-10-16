module Commands.Dev.DevCompile.Options where

import Commands.Dev.DevCompile.Asm.Options
import Commands.Dev.DevCompile.Casm.Options
import Commands.Dev.DevCompile.Core.Options
import Commands.Dev.DevCompile.NativeRust.Options
import Commands.Dev.DevCompile.Reg.Options
import Commands.Dev.DevCompile.Rust.Options
import Commands.Dev.DevCompile.Tree.Options
import Commands.Dev.DevCompile.Vampir.Options
import CommonOptions
import Juvix.Config qualified as Config

data DevCompileCommand
  = Core (CoreOptions 'InputMain)
  | Asm (AsmOptions 'InputMain)
  | Reg (RegOptions 'InputMain)
  | Tree (TreeOptions 'InputMain)
  | Casm (CasmOptions 'InputMain)
  | Rust (RustOptions 'InputMain)
  | NativeRust (NativeRustOptions 'InputMain)
  | Vampir (VampirOptions 'InputMain)
  deriving stock (Data)

parseDevCompileCommand :: Parser DevCompileCommand
parseDevCompileCommand =
  hsubparser
    ( mconcat $
        [ commandCore,
          commandReg,
          commandTree,
          commandCasm,
          commandAsm,
          commandVampir
        ]
          <> [commandRust | Config.config ^. Config.configRust]
          <> [commandNativeRust | Config.config ^. Config.configRust]
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

commandRust :: Mod CommandFields DevCompileCommand
commandRust =
  command "rust" $
    info
      (Rust <$> parseRust)
      (progDesc "Compile to Rust")

commandNativeRust :: Mod CommandFields DevCompileCommand
commandNativeRust =
  command "native-rust" $
    info
      (NativeRust <$> parseNativeRust)
      (progDesc "Compile to native executable through Rust")

commandVampir :: Mod CommandFields DevCompileCommand
commandVampir =
  command "vampir" $
    info
      (Vampir <$> parseVampir)
      (progDesc "Compile to VampIR")
