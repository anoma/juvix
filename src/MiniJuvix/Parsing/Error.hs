-- | Adapted from https://github.com/heliaxdev/juvix/
module MiniJuvix.Parsing.Error 
  (ParsingError
  , Error)
  where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Prelude
--------------------------------------------------------------------------------

data ParsingError

data Error = NoHeaderErr FilePath | ParseError ParserError
  deriving (Show)