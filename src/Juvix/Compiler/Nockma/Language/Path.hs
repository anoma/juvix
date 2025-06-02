module Juvix.Compiler.Nockma.Language.Path where

import Juvix.Data.CodeAnn
import Juvix.Prelude hiding (Atom, Path)
import Prelude (show)

data Direction
  = L
  | R
  deriving stock (Eq, Bounded, Enum, Generic)

instance Show Direction where
  show = \case
    L -> "L"
    R -> "R"

instance Serialize Direction

instance NFData Direction

type Path = [Direction]

newtype EncodedPath = EncodedPath
  { _encodedPath :: Natural
  }

makeLenses ''EncodedPath

emptyPath :: Path
emptyPath = []

decodePath :: forall r. (Member Fail r) => EncodedPath -> Sem r Path
decodePath ep = execOutputList (go (ep ^. encodedPath))
  where
    go :: Natural -> Sem (Output Direction ': r) ()
    go = \case
      0 -> fail
      1 -> return ()
      x ->
        if
          | even x -> do
              go (x `div` 2)
              output L
          | otherwise -> do
              go ((x - 1) `div` 2)
              output R

decodePath' :: EncodedPath -> Path
decodePath' = run . runFailDefault (error "encoded path cannot be 0") . decodePath

encodePath :: Path -> EncodedPath
encodePath = EncodedPath . foldl' step 1
  where
    step :: Natural -> Direction -> Natural
    step n = \case
      R -> 2 * n + 1
      L -> 2 * n

instance Semigroup EncodedPath where
  a <> b = encodePath (decodePath' a <> decodePath' b)

instance Monoid EncodedPath where
  mempty = encodePath []

instance PrettyCodeAnn Path where
  ppCodeAnn = \case
    [] -> annotate (AnnKind KNameInductive) "S"
    ds -> mconcatMap ppCodeAnn ds

instance PrettyCodeAnn Direction where
  ppCodeAnn d = annotate (AnnKind KNameInductive) $ case d of
    L -> "L"
    R -> "R"
