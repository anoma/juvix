module Juvix.Compiler.Core.Data.TransformationId where

import Juvix.Prelude

data TransformationId
  = LambdaLetRecLifting
  | TopEtaExpand
  | RemoveTypeArgs
  | MoveApps
  | NatToInt
  | ConvertBuiltinTypes
  | Identity
  | UnrollRecursion
  | ComputeTypeInfo
  deriving stock (Data)
