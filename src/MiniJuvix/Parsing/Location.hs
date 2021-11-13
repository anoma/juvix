{-# OPTIONS_GHC -Wno-partial-fields #-}
-- | Adapted from https://github.com/heliaxdev/juvix/
module MiniJuvix.Parsing.Location 
  (noLoc,location,mkLocated)
  where

--------------------------------------------------------------------------------

import MiniJuvix.Utils.Parser (Parser)
import MiniJuvix.Utils.Prelude

import qualified Text.Megaparsec as P

--------------------------------------------------------------------------------

data Loc
  = NoLoc
  | Loc {line :: Int, col :: Int}
  deriving stock (Eq, Show, Ord)

data Located a = Located {located :: Loc, locVal :: a}
  deriving stock Show

instance Functor Located where
  fmap f (Located l v) = Located l (f v)

instance Eq a => Eq (Located a) where
  (==) l1 l2 = locVal l1 == locVal l2

instance Ord a => Ord (Located a) where
  compare l1 l2 = locVal l1 `compare` locVal l2

-- | Annotate something with no-location location information.
noLoc :: a -> Located a
noLoc = Located NoLoc

location :: Parser Loc
location = do
  srcPos <- P.getSourcePos
  let r = P.unPos $ P.sourceLine srcPos
      c = P.unPos $ P.sourceColumn srcPos
  pure $ Loc r c

mkLocated :: Parser a -> Parser (Located a)
mkLocated p = Located <$> location <*> p
