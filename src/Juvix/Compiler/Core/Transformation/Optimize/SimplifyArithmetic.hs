module Juvix.Compiler.Core.Transformation.Optimize.SimplifyArithmetic (simplifyArithmetic) where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Node -> Node
convertNode = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntAdd,
          [NBlt blt', n] <- _builtinAppArgs,
          blt' ^. builtinAppOp == OpIntSub,
          [x, m] <- blt' ^. builtinAppArgs,
          m == n ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntSub,
          [NBlt blt', n] <- _builtinAppArgs,
          blt' ^. builtinAppOp == OpIntAdd,
          [x, m] <- blt' ^. builtinAppArgs ->
            if
                | m == n ->
                    x
                | x == n ->
                    m
                | otherwise ->
                    node
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntAdd,
          [n, NBlt blt'] <- _builtinAppArgs,
          blt' ^. builtinAppOp == OpIntSub,
          [x, m] <- blt' ^. builtinAppArgs,
          m == n ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntAdd || _builtinAppOp == OpIntSub,
          [x, NCst (Constant _ (ConstInteger 0))] <- _builtinAppArgs ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntAdd,
          [NCst (Constant _ (ConstInteger 0)), x] <- _builtinAppArgs ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntMul,
          [_, c@(NCst (Constant _ (ConstInteger 0)))] <- _builtinAppArgs ->
            c
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntMul,
          [c@(NCst (Constant _ (ConstInteger 0))), _] <- _builtinAppArgs ->
            c
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntMul,
          [x, NCst (Constant _ (ConstInteger 1))] <- _builtinAppArgs ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpIntMul,
          [NCst (Constant _ (ConstInteger 1)), x] <- _builtinAppArgs ->
            x
      _ -> node

simplifyArithmetic :: InfoTable -> InfoTable
simplifyArithmetic = mapAllNodes convertNode
