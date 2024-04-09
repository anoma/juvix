module Commands.Dev.Core.Options where

import Commands.Dev.Core.Asm.Options
import Commands.Dev.Core.Compile.Options
import Commands.Dev.Core.Eval.Options
import Commands.Dev.Core.FromConcrete.Options
import Commands.Dev.Core.Normalize.Options
import Commands.Dev.Core.Read.Options
import Commands.Dev.Core.Repl.Options
import Commands.Dev.Core.Strip.Options
import CommonOptions

data CoreCommand
  = Repl CoreReplOptions
  | Eval CoreEvalOptions
  | Normalize CoreNormalizeOptions
  | Read CoreReadOptions
  | FromConcrete CoreFromConcreteOptions
  | Strip CoreStripOptions
  | CoreAsm CoreAsmOptions
  | CoreCompile CompileOptions
  deriving stock (Data)

parseCoreCommand :: Parser CoreCommand
parseCoreCommand =
  hsubparser $
    mconcat
      [ commandRepl,
        commandEval,
        commandNormalize,
        commandRead,
        commandStrip,
        commandFromConcrete,
        commandAsm,
        commandCompile
      ]
  where
    commandRepl :: Mod CommandFields CoreCommand
    commandRepl = command "repl" replInfo

    commandEval :: Mod CommandFields CoreCommand
    commandEval = command "eval" evalInfo

    commandNormalize :: Mod CommandFields CoreCommand
    commandNormalize = command "normalize" normalizeInfo

    commandRead :: Mod CommandFields CoreCommand
    commandRead = command "read" readInfo

    commandStrip :: Mod CommandFields CoreCommand
    commandStrip = command "strip" stripInfo

    commandAsm :: Mod CommandFields CoreCommand
    commandAsm = command "asm" asmInfo

    commandFromConcrete :: Mod CommandFields CoreCommand
    commandFromConcrete = command "from-concrete" fromSourceInfo

    commandCompile :: Mod CommandFields CoreCommand
    commandCompile = command "compile" compileInfo

    replInfo :: ParserInfo CoreCommand
    replInfo =
      info
        (Repl <$> parseCoreReplOptions)
        (progDesc "Start an interactive session of the JuvixCore evaluator")

    fromSourceInfo :: ParserInfo CoreCommand
    fromSourceInfo =
      info
        (FromConcrete <$> parseCoreFromConcreteOptions)
        (progDesc "Parse a Juvix file and translate it to JuvixCore")

    evalInfo :: ParserInfo CoreCommand
    evalInfo =
      info
        (Eval <$> parseCoreEvalOptions)
        (progDesc "Evaluate a JuvixCore file and pretty print the result")

    normalizeInfo :: ParserInfo CoreCommand
    normalizeInfo =
      info
        (Normalize <$> parseCoreNormalizeOptions)
        (progDesc "Normalize the main definition from a JuvixCore file and pretty print the result")

    readInfo :: ParserInfo CoreCommand
    readInfo =
      info
        (Read <$> parseCoreReadOptions)
        (progDesc "Read a JuvixCore file, transform it, and pretty print it")

    stripInfo :: ParserInfo CoreCommand
    stripInfo =
      info
        (Strip <$> parseCoreStripOptions)
        (progDesc "Translate a JuvixCore file to Core.Stripped and pretty print the result")

    asmInfo :: ParserInfo CoreCommand
    asmInfo =
      info
        (CoreAsm <$> parseCoreAsmOptions)
        (progDesc "Translate a JuvixCore file to JuvixAsm and run the result")

    compileInfo :: ParserInfo CoreCommand
    compileInfo =
      info
        (CoreCompile <$> parseCoreCompileOptions)
        (progDesc "Compile a JuvixCore file to native code, WebAssembly, GEB or VampIR")
