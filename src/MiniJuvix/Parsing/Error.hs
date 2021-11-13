-- | Adapted from https://github.com/heliaxdev/juvix/
module MiniJuvix.Parsing.Error
  ( Error (..),
  )
where

--------------------------------------------------------------------------------

import safe MiniJuvix.Utils.Prelude (FilePath)

--------------------------------------------------------------------------------

data ParsingError

data Error = NoHeaderErr FilePath | ParseError ParsingError
