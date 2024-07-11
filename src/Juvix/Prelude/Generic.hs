module Juvix.Prelude.Generic
  ( genericConstructorName,
  )
where

import GHC.Generics qualified as G
import Juvix.Prelude.Base.Foundation

genericConstructorName ::
  (HasConstructor (G.Rep a), Generic a, IsString str) => a -> str
genericConstructorName = fromString . genericConstrName . G.from

class HasConstructor (f :: GHCType -> GHCType) where
  genericConstrName :: f x -> String

instance (HasConstructor f) => HasConstructor (G.D1 c f) where
  genericConstrName (G.M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x G.:+: y) where
  genericConstrName = \case
    G.L1 l -> genericConstrName l
    G.R1 r -> genericConstrName r

instance (G.Constructor c) => HasConstructor (G.C1 c f) where
  genericConstrName x = G.conName x
