{-# OPTIONS_GHC -Wno-operator-whitespace #-}
module Experiment.Stage where

import Data.List.Singletons
import Data.List.NonEmpty.Singletons
import Juvix.Prelude

data Stage
  = Concrete
  | Parsed
  | Computed
  deriving stock (Eq)

$(genSingletons [''Stage])

type Pipeline = NonEmpty Stage

type SPipeline (p :: Pipeline) = SNonEmpty p

infixr 5 >>>;
(>>>) :: forall {s :: Stage} {p :: Pipeline}. Sing s -> SNonEmpty p -> SPipeline (s <| p)
a >>> b = a %<| b

infixr 5 >>>|;
(>>>|) :: forall {s :: Stage} {last :: Stage}. SStage s -> SStage last -> SPipeline (s ':| '[last])
a >>>| b = a :%| SCons b SNil
