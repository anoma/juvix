{-# OPTIONS_GHC -Wno-operator-whitespace #-}

module Experiment.Stage where

import Data.List.Singletons
import Juvix.Prelude

data Stage
  = Entry
  | RawText
  | RawString
  | Parsed
  | Computed
  deriving stock (Eq)

$(genSingletons [''Stage])

-- | using list of nonempty list makes for more convenient syntax
type Pipeline = [Stage]

type SPipeline (p :: Pipeline) = SList p

infixr 5 >>>

(>>>) :: forall {s :: Stage} {p :: Pipeline}. Sing s -> SPipeline p -> SPipeline (s ': p)
a >>> b = SCons a b

infixr 5 >>>|

(>>>|) :: forall {s :: Stage} {last :: Stage}. SStage s -> SStage last -> SPipeline '[s, last]
a >>>| b = SCons a (SCons b SNil)
