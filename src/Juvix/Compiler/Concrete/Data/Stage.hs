module Juvix.Compiler.Concrete.Data.Stage where

import Data.Kind qualified as GHC
import Juvix.Prelude

data Stage
  = Parsed
  | Scoped
  deriving stock (Eq, Show)

type AnyStage (k :: Stage -> GHC.Type) =
  Σ Stage (TyCon1 k)

$(genSingletons [''Stage])

deriveStageShow ::
  forall (k :: Stage -> GHC.Type) (s :: Stage).
  (SingI s, Show (k 'Parsed), Show (k 'Scoped)) =>
  k s ->
  String
deriveStageShow = case sing :: SStage s of
  SParsed -> show
  SScoped -> show

deriveStageEq ::
  forall (k :: Stage -> GHC.Type) (s :: Stage).
  (SingI s, Eq (k 'Parsed), Eq (k 'Scoped)) =>
  k s ->
  k s ->
  Bool
deriveStageEq = case sing :: SStage s of
  SParsed -> (==)
  SScoped -> (==)

deriveStageOrd ::
  forall (k :: Stage -> GHC.Type) (s :: Stage).
  (SingI s, Ord (k 'Parsed), Ord (k 'Scoped)) =>
  k s ->
  k s ->
  Ordering
deriveStageOrd = case sing :: SStage s of
  SParsed -> compare
  SScoped -> compare
