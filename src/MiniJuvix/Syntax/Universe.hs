module MiniJuvix.Syntax.Universe where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Fixity

newtype Universe = Universe
  { _universeLevel :: Maybe Natural
  }
  deriving stock (Show, Ord)

instance Eq Universe where
  (Universe a) == (Universe b) = f a == f b
    where
      f :: Maybe Natural -> Natural
      f = fromMaybe defaultLevel

smallUniverse :: Universe
smallUniverse = Universe (Just 0)

defaultLevel :: Natural
defaultLevel = 0

makeLenses ''Universe

instance HasAtomicity Universe where
  atomicity u = case u ^. universeLevel of
    Nothing -> Atom
    Just {} -> Aggregate appFixity
