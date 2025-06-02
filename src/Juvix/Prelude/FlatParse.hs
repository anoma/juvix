module Juvix.Prelude.FlatParse
  ( module FlatParse.Basic,
    module Control.Monad.Combinators,
    module Data.ByteString,
    module Juvix.Prelude.FlatParse,
  )
where

import Control.Monad.Combinators (manyTill_, sepBy1)
import Data.ByteString (ByteString)
import FlatParse.Basic hiding (some, (<|>))
import Juvix.Data.Loc qualified as Loc
import Juvix.Prelude.Base

-- | It assumes all given positions are valid in the given bytestring
fileLocs :: ByteString -> [Pos] -> [Loc.FileLoc]
fileLocs bs positions =
  let go :: Loc.Pos -> Loc.Pos -> Loc.Pos -> [(Int, Pos)] -> Parser Void [(Int, Loc.FileLoc)]
      go !line !col !offset = \case
        [] -> pure []
        allPos@((i, pos) : poss) -> do
          p <- getPos
          let fl =
                Loc.FileLoc
                  { _locLine = line,
                    _locCol = col,
                    _locOffset = offset
                  }
          if
            | pos == p -> ((i, fl) :) <$> go line col offset poss
            | otherwise ->
                do
                  c <- anyChar
                  if
                    | '\n' == c -> go (line + 1) 1 (succ offset) allPos
                    | otherwise -> go line (col + 1) (succ offset) allPos

      sorted :: [(Int, Pos)]
      sorted = sortBy (\(_, i) (_, j) -> compare i j) (zip [0 ..] positions)
   in case runParser (go 1 1 0 sorted) bs of
        OK res _ -> snd <$> sortOn fst res
        _ -> error "FlatParse.fileLocs: invalid position"
