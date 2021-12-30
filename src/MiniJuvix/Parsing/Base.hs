module MiniJuvix.Parsing.Base (
  module Text.Megaparsec
  , module Data.List.NonEmpty
  , module Text.Megaparsec.Char
  , sepBy1 
  , some
  )
where

import qualified Control.Monad.Combinators as P
import Data.List.NonEmpty (NonEmpty)
import MiniJuvix.Utils.Prelude hiding (some)
import Text.Megaparsec hiding (sepBy1, some)
import Text.Megaparsec.Char

sepBy1 :: MonadPlus m ⇒ m a → m sep → m (NonEmpty a)
sepBy1 m s = fromList <$> P.sepBy1 m s

some :: MonadPlus m ⇒ m a → m (NonEmpty a)
some m = fromList <$> P.some m
