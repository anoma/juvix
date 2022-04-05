module MiniJuvix.Syntax.Concrete.Loc where

import MiniJuvix.Prelude
import Prettyprinter

newtype Pos = Pos {_unPos :: Word64 }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

instance Semigroup Pos where
  Pos x <> Pos y = Pos (x + y)

instance Monoid Pos where
  mempty = Pos 0

data FileLoc = FileLoc
  { -- | Line number
    _locLine :: !Pos,
    -- | Column number
    _locCol :: !Pos,
    -- | Offset wrt the start of the input. Used for syntax highlighting.
    _locOffset :: !Pos
  }
  deriving stock (Show, Eq, Generic)

instance Hashable FileLoc

instance Ord FileLoc where
  compare (FileLoc l c o) (FileLoc l' c' o') = compare (l, c, o) (l', c', o')

data Loc = Loc
  { -- | Name of source file
    _locFile :: FilePath,
    -- | Position within the file
    _locFileLoc :: !FileLoc
  }
  deriving stock (Show, Eq, Ord)

-- | Inclusive interval
data Interval = Interval
  { _intFile :: FilePath,
    _intStart :: FileLoc,
    _intEnd :: FileLoc
  }
  deriving stock (Show, Ord, Eq, Generic)

instance Hashable Interval

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
  pretty FileLoc {..} =
    pretty _locLine <> colon <> pretty _locCol

instance Pretty Loc where
  pretty :: Loc -> Doc a
  pretty Loc {..} =
    pretty _locFile <> colon <> pretty _locFileLoc

instance Pretty Interval where
  pretty :: Interval -> Doc a
  pretty Interval {..} =
    pretty _intFile <> colon
      <> ppPosRange (_locLine _intStart, _locLine _intEnd)
      <> colon
      <> ppPosRange (_locCol _intStart, _locCol _intEnd)
    where
      hyphen = pretty '-'
      ppPosRange :: (Pos, Pos) -> Doc a
      ppPosRange (s, e)
        | s == e = pretty s
        | otherwise = pretty s <> hyphen <> pretty e

makeLenses ''Interval
makeLenses ''FileLoc
makeLenses ''Loc
makeLenses ''Pos
