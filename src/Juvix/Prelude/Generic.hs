module Juvix.Prelude.Generic
  ( genericConstructorName,
    genericSameConstructor,
    GenericHasConstructor,
  )
where

import GHC.Generics qualified as G
import Juvix.Prelude.Base.Foundation

genericSameConstructor :: (Generic a, GenericHasConstructor (G.Rep a)) => a -> a -> Bool
genericSameConstructor x y = genericConstructorName @String x == genericConstructorName y

genericConstructorName ::
  forall str a.
  (GenericHasConstructor (G.Rep a), Generic a, IsString str) =>
  a ->
  str
genericConstructorName = fromString . genericConstrName . G.from

class GenericHasConstructor (f :: GHCType -> GHCType) where
  genericConstrName :: f x -> String

instance (GenericHasConstructor f) => GenericHasConstructor (G.D1 c f) where
  genericConstrName (G.M1 x) = genericConstrName x

instance (GenericHasConstructor x, GenericHasConstructor y) => GenericHasConstructor (x G.:+: y) where
  genericConstrName = \case
    G.L1 l -> genericConstrName l
    G.R1 r -> genericConstrName r

instance (G.Constructor c) => GenericHasConstructor (G.C1 c f) where
  genericConstrName x = G.conName x
