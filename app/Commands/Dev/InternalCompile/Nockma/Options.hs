module Commands.Dev.InternalCompile.Nockma.Options where

import CommonOptions

data NockmaOptions
  deriving stock (Data)

parseNockma :: Parser NockmaOptions
parseNockma = undefined
