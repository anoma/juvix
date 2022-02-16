module MiniJuvix.Syntax.Concrete.Loc where

import MiniJuvix.Prelude
import Prettyprinter
import Language.Haskell.TH.Syntax (Lift)

newtype Pos = Pos Word64
  deriving stock (Show, Eq, Ord, Lift)

instance Semigroup Pos where
  Pos x <> Pos y = Pos (x + y)

instance Monoid Pos where
  mempty = Pos 0

data FileLoc = FileLoc {
  -- | Line number
    _locLine :: !Pos,
    -- | Column number
    _locCol :: !Pos
  }
  deriving stock (Show, Eq, Lift)

instance Ord FileLoc where
  compare (FileLoc l c) (FileLoc l' c') = compare (l, c) (l', c')

data Loc = Loc
  { -- | Name of source file
    _locFile :: FilePath,
    -- | Position within the file
    _locFileLoc :: !FileLoc
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | Inclusive interval
data Interval = Interval {
  _intFile :: FilePath,
  _intStart :: FileLoc,
  _intEnd :: FileLoc
  }
  deriving stock (Show, Ord, Eq, Lift)

class HasLoc t where
  getLoc :: t -> Interval

-- | Assumes the file is the same
instance Semigroup Interval where
  Interval f s e <> Interval _f s' e' = Interval f (min s s') (max e e')

mkInterval :: Loc -> Loc -> Interval
mkInterval start end =
  Interval (_locFile start) (_locFileLoc start) (_locFileLoc end)

instance Pretty Pos where
  pretty :: Pos -> Doc a
  pretty (Pos p) = pretty p

instance Pretty FileLoc where
  pretty :: FileLoc -> Doc a
  pretty FileLoc {..}  =
    pretty _locLine <> colon <> pretty _locCol

instance Pretty Loc where
  pretty :: Loc -> Doc a
  pretty Loc {..} =
    pretty _locFile <> colon <> pretty _locFileLoc

instance Pretty Interval where
  pretty :: Interval -> Doc a
  pretty Interval {..} =
    pretty _intFile <> colon
    <> ppPosRange (_locLine _intStart, _locLine _intEnd) <> colon
    <> ppPosRange (_locCol _intStart, _locCol _intEnd)
    where
    hyphen = pretty '-'
    ppPosRange :: (Pos, Pos) -> Doc a
    ppPosRange (s, e)
      | s == e = pretty s
      | otherwise = pretty s <> hyphen <> pretty e
