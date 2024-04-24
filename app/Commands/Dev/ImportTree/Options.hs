module Commands.Dev.ImportTree.Options where

import CommonOptions

data ImportTreeOptions = ImportTreeOptions
  deriving stock (Data)

parseImportTree :: Parser ImportTreeOptions
parseImportTree = pure ImportTreeOptions
