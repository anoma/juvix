module Juvix.Compiler.Core.Transformation.LambdaLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Data.InfoTableBuilder

lambdaLiftNode :: Member InfoTableBuilder r => Node -> Sem r Node
lambdaLiftNode _ = do
  void freshSymbol
  error "not yet implemented"

lambdaLifting :: Transformation
lambdaLifting = run . mapT' lambdaLiftNode
