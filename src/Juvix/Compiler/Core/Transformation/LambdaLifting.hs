module Juvix.Compiler.Core.Transformation.LambdaLifting
  ( module Juvix.Compiler.Core.Transformation.LambdaLifting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Transformation.Base

lambdaLiftNode :: Node -> Sem r Node
lambdaLiftNode = return

lambdaLifting :: InfoTable -> InfoTable
lambdaLifting = run . mapT' lambdaLiftNode
