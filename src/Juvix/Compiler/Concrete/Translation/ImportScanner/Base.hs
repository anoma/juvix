module Juvix.Compiler.Concrete.Translation.ImportScanner.Base where

import Juvix.Prelude

data ParseError = ParseError

data Token
  = TokenString
  | TokenImport ImportScanParsed
  | TokenReserved
  | TokenCode

makePrisms ''Token
