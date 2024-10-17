module Juvix.Compiler.Nockma.Language
  ( module Juvix.Compiler.Nockma.Language,
    module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Nockma.StdlibFunction.Base,
    module Juvix.Compiler.Nockma.Language.Path,
  )
where

import Data.HashMap.Strict qualified as HashMap
import GHC.Base (Type)
import Juvix.Compiler.Core.Language.Base (Symbol)
import Juvix.Compiler.Nockma.Language.Path
import Juvix.Compiler.Nockma.StdlibFunction.Base
import Juvix.Prelude hiding (Atom, Path)
import Juvix.Prelude.Pretty

data ReplStatement a
  = ReplStatementExpression (ReplExpression a)
  | ReplStatementAssignment (Assignment a)

data ReplExpression a
  = ReplExpressionTerm (ReplTerm a)
  | ReplExpressionWithStack (WithStack a)

data WithStack a = WithStack
  { _withStackStack :: ReplTerm a,
    _withStackTerm :: ReplTerm a
  }

data ReplTerm a
  = ReplName Text
  | ReplTerm (Term a)

newtype Program a = Program
  { _programStatements :: [Statement a]
  }

data Statement a
  = StatementAssignment (Assignment a)
  | StatementStandalone (Term a)

data Assignment a = Assignment
  { _assignmentName :: Text,
    _assignmentBody :: Term a
  }

data Term a
  = TermAtom (Atom a)
  | TermCell (Cell a)
  deriving stock (Show, Eq, Lift, Generic)

instance (Hashable a) => Hashable (Term a)

instance (NFData a) => NFData (Term a)

data StdlibCall a = StdlibCall
  { _stdlibCallFunction :: StdlibFunction,
    _stdlibCallArgs :: Term a
  }
  deriving stock (Show, Eq, Lift, Generic)

instance (Hashable a) => Hashable (StdlibCall a)

instance (NFData a) => NFData (StdlibCall a)

newtype Tag = Tag
  { _unTag :: Text
  }
  deriving stock (Show, Eq, Lift, Generic)

instance Hashable Tag

instance NFData Tag

data CellInfo a = CellInfo
  { _cellInfoLoc :: Irrelevant (Maybe Interval),
    _cellInfoTag :: Maybe Tag,
    _cellInfoCall :: Maybe (StdlibCall a)
  }
  deriving stock (Show, Eq, Lift, Generic)

instance (Hashable a) => Hashable (CellInfo a)

instance (NFData a) => NFData (CellInfo a)

data Cell a = Cell'
  { _cellLeft :: Term a,
    _cellRight :: Term a,
    _cellInfo :: CellInfo a
  }
  deriving stock (Show, Eq, Lift, Generic)

instance (Hashable a) => Hashable (Cell a)

instance (NFData a) => NFData (Cell a)

data AtomInfo = AtomInfo
  { _atomInfoHint :: Maybe AtomHint,
    _atomInfoTag :: Maybe Tag,
    _atomInfoLoc :: Irrelevant (Maybe Interval)
  }
  deriving stock (Show, Eq, Lift, Generic)

instance Hashable AtomInfo

instance NFData AtomInfo

data Atom a = Atom
  { _atom :: a,
    _atomInfo :: AtomInfo
  }
  deriving stock (Show, Eq, Lift, Generic)

instance (Hashable a) => Hashable (Atom a)

instance (NFData a) => NFData (Atom a)

data AtomHint
  = AtomHintOp
  | AtomHintPath
  | AtomHintBool
  | AtomHintNil
  | AtomHintVoid
  | AtomHintFunctionsPlaceholder
  | AtomHintStdlibPlaceholder
  | AtomHintString
  deriving stock (Show, Eq, Lift, Generic)

instance Hashable AtomHint

instance NFData AtomHint

data NockOp
  = OpAddress
  | OpQuote
  | OpApply
  | OpIsCell
  | OpInc
  | OpEq
  | OpIf
  | OpSequence
  | OpPush
  | OpCall
  | OpReplace
  | OpHint
  | OpScry
  deriving stock (Bounded, Enum, Eq, Generic)

instance Hashable NockOp

instance Pretty NockOp where
  pretty = \case
    OpAddress -> "@"
    OpQuote -> "quote"
    OpApply -> "apply"
    OpIsCell -> "isCell"
    OpInc -> "suc"
    OpEq -> "="
    OpIf -> "if"
    OpSequence -> "seq"
    OpPush -> "push"
    OpCall -> "call"
    OpReplace -> "replace"
    OpHint -> "hint"
    OpScry -> "scry"

data NockHint = NockHintPuts
  deriving stock (Show, Eq, Enum, Bounded)

textToStdlibFunctionMap :: HashMap Text StdlibFunction
textToStdlibFunctionMap =
  hashMap
    [ (prettyText f, f) | f <- allElements
    ]

parseStdlibFunction :: Text -> Maybe StdlibFunction
parseStdlibFunction t = textToStdlibFunctionMap ^. at t

atomOps :: HashMap Text NockOp
atomOps = HashMap.fromList [(prettyText op, op) | op <- allElements]

data StdlibCallCell a = StdlibCallCell
  { _stdlibCallCell :: StdlibCall a,
    _stdlibCallRaw :: OperatorCell a
  }

data OperatorCell a = OperatorCell
  { _operatorCellOp :: NockOp,
    _operatorCellTag :: Maybe Tag,
    _operatorCellTerm :: Term a
  }

data AutoConsCell a = AutoConsCell
  { _autoConsCellLeft :: Cell a,
    _autoConsCellRight :: Term a
  }

data ParsedCell a
  = ParsedOperatorCell (OperatorCell a)
  | ParsedAutoConsCell (AutoConsCell a)
  | ParsedStdlibCallCell (StdlibCallCell a)

-- | appends n R
encodedPathAppendRightN :: Natural -> EncodedPath -> EncodedPath
encodedPathAppendRightN n (EncodedPath p) = EncodedPath (f p)
  where
    -- equivalent to applying 2 * x + 1, n times
    f :: Natural -> Natural
    f x = (2 ^ n) * (x + 1) - 1

makeLenses ''Cell
makeLenses ''Tag
makeLenses ''StdlibCallCell
makeLenses ''StdlibCall
makeLenses ''Atom
makeLenses ''OperatorCell
makeLenses ''AutoConsCell
makeLenses ''Program
makeLenses ''Assignment
makeLenses ''WithStack
makeLenses ''AtomInfo
makeLenses ''CellInfo

isCell :: Term a -> Bool
isCell = \case
  TermCell {} -> True
  _ -> False

isAtom :: Term a -> Bool
isAtom = not . isCell

atomHint :: Lens' (Atom a) (Maybe AtomHint)
atomHint = atomInfo . atomInfoHint

termLoc :: Lens' (Term a) (Maybe Interval)
termLoc f = \case
  TermAtom a -> TermAtom <$> atomLoc f a
  TermCell a -> TermCell <$> cellLoc f a

cellLoc :: Lens' (Cell a) (Maybe Interval)
cellLoc = cellInfo . cellInfoLoc . unIrrelevant

cellTag :: Lens' (Cell a) (Maybe Tag)
cellTag = cellInfo . cellInfoTag

cellCall :: Lens' (Cell a) (Maybe (StdlibCall a))
cellCall = cellInfo . cellInfoCall

atomTag :: Lens' (Atom a) (Maybe Tag)
atomTag = atomInfo . atomInfoTag

atomLoc :: Lens' (Atom a) (Maybe Interval)
atomLoc = atomInfo . atomInfoLoc . unIrrelevant

naturalNockOps :: HashMap Natural NockOp
naturalNockOps = HashMap.fromList [(serializeOp op, op) | op <- allElements]

nockOpsNatural :: HashMap NockOp Natural
nockOpsNatural = HashMap.fromList (swap <$> HashMap.toList naturalNockOps)

parseOp :: (Member Fail r) => Natural -> Sem r NockOp
parseOp n = failMaybe (naturalNockOps ^. at n)

serializeOp :: NockOp -> Natural
serializeOp = \case
  OpAddress -> 0
  OpQuote -> 1
  OpApply -> 2
  OpIsCell -> 3
  OpInc -> 4
  OpEq -> 5
  OpIf -> 6
  OpSequence -> 7
  OpPush -> 8
  OpCall -> 9
  OpReplace -> 10
  OpHint -> 11
  OpScry -> 12

class (NockmaEq a) => NockNatural a where
  type ErrNockNatural a :: Type
  nockNatural :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Natural
  fromNatural :: (Member (Error (ErrNockNatural a)) r) => Natural -> Sem r a
  serializeNockOp :: NockOp -> a
  serializePath :: Path -> a

  errInvalidOp :: Atom a -> ErrNockNatural a

  errInvalidPath :: Atom a -> ErrNockNatural a
  errGetAtom :: ErrNockNatural a -> Atom a

  nockOp :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r NockOp
  nockOp atm = do
    case atm ^. atomHint of
      Just h
        | h /= AtomHintOp -> throw (errInvalidOp atm)
      _ -> return ()
    n <- nockNatural atm
    failWithError (errInvalidOp atm) (parseOp n)

  nockPath :: (Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Path
  nockPath atm = do
    n <- nockNatural atm
    failWithError (errInvalidPath atm) (decodePath (EncodedPath n))

  nockTrue :: Atom a
  nockFalse :: Atom a
  nockSucc :: Atom a -> Atom a
  nockNil :: Atom a
  nockVoid :: Atom a

nockBool :: (NockNatural a) => Bool -> Atom a
nockBool = \case
  True -> nockTrue
  False -> nockFalse

nockNilTagged :: Text -> Term Natural
nockNilTagged txt = TermAtom (set atomTag (Just (Tag txt)) nockNil)

data NockNaturalNaturalError
  = NaturalInvalidPath (Atom Natural)
  | NaturalInvalidOp (Atom Natural)
  deriving stock (Show)

nockTrueLiteral :: Term Natural
nockTrueLiteral = OpQuote # TermAtom (nockTrue @Natural)

nockFalseLiteral :: Term Natural
nockFalseLiteral = OpQuote # TermAtom (nockFalse @Natural)

nockBoolLiteral :: Bool -> Term Natural
nockBoolLiteral b
  | b = nockTrueLiteral
  | otherwise = nockFalseLiteral

nockHintName :: NockHint -> Text
nockHintName = \case
  NockHintPuts -> "puts"

nockHintValue :: NockHint -> Natural
nockHintValue = \case
  NockHintPuts -> 0x73747570

nockHintAtom :: NockHint -> Term Natural
nockHintAtom hint =
  TermAtom
    Atom
      { _atomInfo = emptyAtomInfo,
        _atom = nockHintValue hint
      }

instance NockNatural Natural where
  type ErrNockNatural Natural = NockNaturalNaturalError
  nockNatural a = return (a ^. atom)
  fromNatural = return
  nockTrue = Atom 0 (atomHintInfo AtomHintBool)
  nockFalse = Atom 1 (atomHintInfo AtomHintBool)
  nockNil = Atom 0 (atomHintInfo AtomHintNil)
  errGetAtom = \case
    NaturalInvalidPath a -> a
    NaturalInvalidOp a -> a
  nockSucc = over atom succ
  nockVoid = Atom 0 (atomHintInfo AtomHintVoid)
  errInvalidOp atm = NaturalInvalidOp atm
  errInvalidPath atm = NaturalInvalidPath atm
  serializeNockOp = serializeOp
  serializePath = (^. encodedPath) . encodePath

atomHintInfo :: AtomHint -> AtomInfo
atomHintInfo h =
  emptyAtomInfo
    { _atomInfoHint = Just h
    }

setAtomHint :: AtomHint -> Atom a -> Atom a
setAtomHint h = set (atomInfo . atomInfoHint) (Just h)

class IsNock nock where
  toNock :: nock -> Term Natural

instance IsNock (Term Natural) where
  toNock = id

instance IsNock (Atom Natural) where
  toNock = TermAtom

instance IsNock (Cell Natural) where
  toNock = TermCell

instance IsNock Natural where
  toNock = TAtom

instance IsNock NockOp where
  toNock op = toNock (Atom (serializeOp op) (atomHintInfo AtomHintOp))

instance IsNock Bool where
  toNock = \case
    False -> toNock (nockFalse @Natural)
    True -> toNock (nockTrue @Natural)

instance IsNock Path where
  toNock pos = TermAtom (Atom (encodePath pos ^. encodedPath) (atomHintInfo AtomHintPath))

instance IsNock EncodedPath where
  toNock = toNock . decodePath'

class HasTag a where
  atTag :: Lens' a (Maybe Tag)

instance (HasTag (Term a)) where
  atTag = lens getTag setTag
    where
      getTag :: Term x -> Maybe Tag
      getTag = \case
        TermAtom x -> x ^. atomTag
        TermCell x -> x ^. cellTag

      setTag :: Term a -> Maybe Tag -> Term a
      setTag t newTag = case t of
        TermAtom x -> TermAtom (set atomTag newTag x)
        TermCell x -> TermCell (set cellTag newTag x)

instance (HasTag (Cell a)) where
  atTag = cellTag

instance (HasTag (Atom a)) where
  atTag = atomTag

infixr 1 @.

(@.) :: Text -> Cell Natural -> Cell Natural
tag @. c = set cellTag (Just (Tag tag)) c

infixr 1 @

(@) :: (HasTag a) => Text -> a -> a
tagTxt @ c = set atTag (Just (Tag tagTxt)) c

infixr 5 #.

(#.) :: (IsNock x, IsNock y) => x -> y -> Cell Natural
a #. b = Cell (toNock a) (toNock b)

infixr 5 #

(#) :: (IsNock x, IsNock y) => x -> y -> Term Natural
a # b = TermCell (a #. b)

infixl 1 >>#.

(>>#.) :: (IsNock x, IsNock y) => x -> y -> Cell Natural
a >>#. b = OpSequence #. a # b

infixl 1 >>#

(>>#) :: (IsNock x, IsNock y) => x -> y -> Term Natural
a >># b = TermCell (a >>#. b)

opCall :: Text -> Path -> Term Natural -> Term Natural
opCall txt p t = TermCell (txt @ (OpCall #. (p # t)))

opReplace :: Text -> Path -> Term Natural -> Term Natural -> Term Natural
opReplace txt p t1 t2 = TermCell (txt @ OpReplace #. ((p #. t1) #. t2))

opAddress :: Text -> Path -> Term Natural
opAddress txt p = TermCell (txt @ OpAddress #. p)

opQuote :: (IsNock x) => Text -> x -> Term Natural
opQuote txt p = TermCell (txt @ OpQuote #. p)

opTrace :: Term Natural -> Term Natural
opTrace val = OpHint # (nockHintAtom NockHintPuts # val) # val

opTrace' :: Term Natural -> Term Natural -> Term Natural
opTrace' msg val = OpHint # (nockNilTagged "opTrace'" # msg) # val

{-# COMPLETE Cell #-}

pattern Cell :: Term a -> Term a -> Cell a
pattern Cell {_cellLeft', _cellRight'} <- Cell' _cellLeft' _cellRight' _
  where
    Cell a b = Cell' a b emptyCellInfo

{-# COMPLETE TCell, TAtom #-}

pattern TCell :: Term a -> Term a -> Term a
pattern TCell l r <- TermCell (Cell' l r _)
  where
    TCell a b = TermCell (Cell a b)

pattern TAtom :: a -> Term a
pattern TAtom a <- TermAtom (Atom a _)
  where
    TAtom a = TermAtom (Atom a emptyAtomInfo)

emptyCellInfo :: CellInfo a
emptyCellInfo =
  CellInfo
    { _cellInfoCall = Nothing,
      _cellInfoTag = Nothing,
      _cellInfoLoc = Irrelevant Nothing
    }

emptyAtomInfo :: AtomInfo
emptyAtomInfo =
  AtomInfo
    { _atomInfoHint = Nothing,
      _atomInfoTag = Nothing,
      _atomInfoLoc = Irrelevant Nothing
    }

class NockmaEq a where
  nockmaEq :: a -> a -> Bool

instance NockmaEq Natural where
  nockmaEq a b = a == b

instance (NockmaEq a) => NockmaEq [a] where
  nockmaEq a b =
    case zipExactMay a b of
      Nothing -> False
      Just z -> all (uncurry nockmaEq) z

instance (NockmaEq a) => NockmaEq (Atom a) where
  nockmaEq = nockmaEq `on` (^. atom)

instance (NockmaEq a) => NockmaEq (Term a) where
  nockmaEq = \cases
    (TermAtom a) (TermAtom b) -> nockmaEq a b
    (TermCell a) (TermCell b) -> nockmaEq a b
    TermCell {} TermAtom {} -> False
    TermAtom {} TermCell {} -> False

instance (NockmaEq a) => NockmaEq (Cell a) where
  nockmaEq (Cell l r) (Cell l' r') = nockmaEq l l' && nockmaEq r r'
