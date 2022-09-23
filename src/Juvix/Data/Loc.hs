module Juvix.Data.Loc where

import Juvix.Prelude.Base
import Prettyprinter
import Text.Megaparsec qualified as M

newtype Pos = Pos {_unPos :: Word64}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, Num, Enum, Real, Integral)

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
  { _intervalFile :: FilePath,
    _intervalStart :: FileLoc,
    _intervalEnd :: FileLoc
  }
  deriving stock (Show, Ord, Eq, Generic)

instance Hashable Interval

class HasLoc t where
  getLoc :: t -> Interval

-- | The items are assumed to be in order with respect to their location.
getLocSpan :: HasLoc t => NonEmpty t -> Interval
getLocSpan l = getLoc (head l) <> getLoc (last l)

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
    { _intervalFile = l ^. locFile,
      _intervalStart = l ^. locFileLoc,
      _intervalEnd = l ^. locFileLoc
    }

intervalLength :: Interval -> Int
intervalLength i = fromIntegral (i ^. intervalEnd . locOffset - i ^. intervalStart . locOffset) + 1

intervalStartLoc :: Interval -> Loc
intervalStartLoc i =
  Loc
    { _locFile = i ^. intervalFile,
      _locFileLoc = i ^. intervalStart
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
    pretty (i ^. intervalFile)
      <> colon
      <> ppPosRange (i ^. intervalStart . locLine, i ^. intervalEnd . locLine)
      <> colon
      <> ppPosRange (i ^. intervalStart . locCol, i ^. intervalEnd . locCol)
    where
      hyphen :: Doc a
      hyphen = pretty '-'
      ppPosRange :: (Pos, Pos) -> Doc a
      ppPosRange (s, e)
        | s == e = pretty s
        | otherwise = pretty s <> hyphen <> pretty e
