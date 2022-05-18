module MiniJuvix.Syntax.Concrete.Loc where

import MiniJuvix.Prelude.Base
import Prettyprinter
import Text.Megaparsec qualified as M

newtype Pos = Pos {_unPos :: Word64}
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

mkLoc :: FilePath -> Int -> M.SourcePos -> Loc
mkLoc root offset M.SourcePos {..} =
  let _locFile = normalise (root </> sourceName)
   in Loc {..}
  where
    _locOffset = Pos (fromIntegral offset)
    _locFileLoc = FileLoc {..}
      where
        _locLine = fromPos sourceLine
        _locCol = fromPos sourceColumn

fromPos :: M.Pos -> Pos
fromPos = Pos . fromIntegral . M.unPos

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

makeLenses ''Interval
makeLenses ''FileLoc
makeLenses ''Loc
makeLenses ''Pos

singletonInterval :: Loc -> Interval
singletonInterval l =
  Interval
    { _intFile = l ^. locFile,
      _intStart = l ^. locFileLoc,
      _intEnd = l ^. locFileLoc
    }

intervalStart :: Interval -> Loc
intervalStart i =
  Loc
    { _locFile = i ^. intFile,
      _locFileLoc = i ^. intStart
    }

mkInterval :: Loc -> Loc -> Interval
mkInterval start end =
  Interval (start ^. locFile) (start ^. locFileLoc) (end ^. locFileLoc)

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
  pretty i =
    pretty (i ^. intFile) <> colon
      <> ppPosRange (i ^. intStart . locLine, i ^. intEnd . locLine)
      <> colon
      <> ppPosRange (i ^. intStart . locCol, i ^. intEnd . locCol)
    where
      hyphen :: Doc a
      hyphen = pretty '-'
      ppPosRange :: (Pos, Pos) -> Doc a
      ppPosRange (s, e)
        | s == e = pretty s
        | otherwise = pretty s <> hyphen <> pretty e
