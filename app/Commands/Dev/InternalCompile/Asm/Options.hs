module Commands.Dev.InternalCompile.Asm.Options where

import CommonOptions

data AsmOptions
  deriving stock (Data)

parseAsm :: Parser AsmOptions
parseAsm = undefined
