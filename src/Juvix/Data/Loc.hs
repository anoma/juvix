module Juvix.Data.Loc where

import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Prettyprinter
import Text.Megaparsec qualified as M

newtype Pos = Pos {_unPos :: Word64}
  deriving stock (Show, Eq, Ord, Data)
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
  deriving stock (Show, Eq, Generic, Data)

instance Hashable FileLoc

instance Ord FileLoc where
  compare (FileLoc l c o) (FileLoc l' c' o') = compare (l, c, o) (l', c', o')

data Loc = Loc
  { -- | Name of source file
    _locFile :: Path Abs File,
    -- | Position within the file
    _locFileLoc :: !FileLoc
  }
  deriving stock (Show, Eq, Ord)

mkLoc :: Int -> M.SourcePos -> Loc
mkLoc offset M.SourcePos {..} =
  let _locFile = absFile' (normalise sourceName)
   in Loc {..}
  where
    _locOffset = Pos (fromIntegral offset)
    _locFileLoc = FileLoc {..}
      where
        _locLine = fromPos sourceLine
        _locCol = fromPos sourceColumn
    absFile' :: FilePath -> Path Abs File
    absFile' fp = fromMaybe err (parseAbsFile fp)
      where
        err :: a
        err = error ("The path \"" <> pack fp <> "\" is not absolute. Remember to pass an absolute path to Megaparsec when running a parser")

-- | Make a `Loc` that points to the beginning of a file.
mkInitialLoc :: Path Abs File -> Loc
mkInitialLoc = mkLoc 0 . M.initialPos . fromAbsFile

fromPos :: M.Pos -> Pos
fromPos = Pos . fromIntegral . M.unPos

-- | Inclusive interval
data Interval = Interval
  { _intervalFile :: Path Abs File,
    _intervalStart :: FileLoc,
    _intervalEnd :: FileLoc
  }
  deriving stock (Show, Ord, Eq, Generic, Data)

instance Hashable Interval

class HasLoc t where
  getLoc :: t -> Interval

instance HasLoc Interval where
  getLoc = id

-- | The items are assumed to be in order with respect to their location.
getLocSpan :: (HasLoc t) => NonEmpty t -> Interval
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

intervalEndLine :: Interval -> Int
intervalEndLine a = a ^. intervalEnd . locLine . unPos . to fromIntegral

intervalStartLine :: Interval -> Int
intervalStartLine a = a ^. intervalStart . locLine . unPos . to fromIntegral

intervalStartLoc :: Interval -> Loc
intervalStartLoc i =
  Loc
    { _locFile = i ^. intervalFile,
      _locFileLoc = i ^. intervalStart
    }

mkInterval :: Loc -> Loc -> Interval
mkInterval start end =
  Interval (start ^. locFile) (start ^. locFileLoc) (end ^. locFileLoc)

filterByLoc :: HasLoc p => Path Abs File -> [p] -> [p]
filterByLoc p = filter ((== p) . (^. intervalFile) . getLoc)

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
