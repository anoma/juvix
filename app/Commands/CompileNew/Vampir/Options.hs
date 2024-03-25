module Commands.CompileNew.Vampir.Options where

import CommonOptions

data VampirOptions
  deriving stock (Data)

parseVampir :: Parser VampirOptions
parseVampir = undefined
