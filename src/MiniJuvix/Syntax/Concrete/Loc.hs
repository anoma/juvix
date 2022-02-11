module MiniJuvix.Syntax.Concrete.Loc where

import MiniJuvix.Utils.Prelude
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

-- | Assumes the file is the same
instance Semigroup Interval where
  Interval f s e <> Interval _f s' e' = Interval f (min s s') (max e e')

mkInterval :: Loc -> Loc -> Interval
mkInterval start end =
  Interval (_locFile start) (_locFileLoc start) (_locFileLoc end)

ppPos :: Pos -> Doc a
ppPos (Pos p) = pretty p

ppFileLoc :: FileLoc -> Doc a
ppFileLoc FileLoc {..}  =
  ppPos _locLine <> colon <> ppPos _locCol

ppLoc :: Loc -> Doc a
ppLoc Loc {..} =
  pretty _locFile <> colon <> ppFileLoc _locFileLoc

ppInterval :: Interval -> Doc a
ppInterval Interval {..} =
 pretty _intFile <> colon
 <> ppPosRange (_locLine _intStart, _locLine _intEnd) <> colon
 <> ppPosRange (_locCol _intStart, _locCol _intEnd)
 where
 hyphen = pretty '-'
 ppPosRange :: (Pos, Pos) -> Doc a
 ppPosRange (s, e)
   | s == e = ppPos s
   | otherwise = ppPos s <> hyphen <> ppPos e
