module Commands.Dev.ImportTree.Print.Options where

import CommonOptions

data PrintOptions = PrintOptions
  deriving stock (Data)

parsePrint :: Parser PrintOptions
parsePrint = pure PrintOptions
