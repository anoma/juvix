module Juvix.Compiler.Core.Data.TransformationId where

import Juvix.Prelude

data TransformationId
  = LambdaLifting
  | TopEtaExpand
  | RemoveTypeArgs
  | MoveApps
  | NatToInt
  | Identity
  deriving stock (Data)
