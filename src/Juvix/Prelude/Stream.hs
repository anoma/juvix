module Juvix.Prelude.Stream where

import Data.Stream qualified as Stream
import Juvix.Prelude.Base

allWords :: Stream Text
allWords = pack . toList <$> allFiniteSequences ('a' :| ['b' .. 'z'])

-- | Returns all non-empty finite sequences
allFiniteSequences :: forall a. NonEmpty a -> Stream (NonEmpty a)
allFiniteSequences elems = build 0 []
  where
    build :: Integer -> [NonEmpty a] -> Stream (NonEmpty a)
    build n = \case
      [] -> build (succ n) (toList (ofLength (succ n)))
      s : ss -> Stream.Cons s (build n ss)
    ofLength :: Integer -> NonEmpty (NonEmpty a)
    ofLength n
      | n < 1 = impossible
      | n == 1 = pure <$> elems
      | otherwise = do
          seq <- ofLength (n - 1)
          e <- elems
          return (pure e <> seq)
