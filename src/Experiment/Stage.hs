module Experiment.Stage where

import Data.List.NonEmpty.Singletons
import Juvix.Prelude

data Stage
  = Concrete
  | Parsed
  | Computed
  deriving stock (Eq)

$(genSingletons [''Stage])

type Pipeline = NonEmpty Stage

type SPipeline p = SNonEmpty p
