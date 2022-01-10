{-# OPTIONS_GHC -Wno-deprecations #-}
module MiniJuvix.Parsing.Error
  ( Error (..),
    ParsingError (..),
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Prelude
import qualified Text.Show

--------------------------------------------------------------------------------

data ParsingError

instance Show ParsingError where
  show = undefined

data Error = NoHeaderErr FilePath | ParseError ParsingError

instance Show Error where
  show = undefined
