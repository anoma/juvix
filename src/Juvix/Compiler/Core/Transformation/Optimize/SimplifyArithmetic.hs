module Juvix.Compiler.Core.Transformation.Optimize.SimplifyArithmetic (simplifyArithmetic) where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Data.Field

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
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldAdd,
          [NBlt blt', n] <- _builtinAppArgs,
          blt' ^. builtinAppOp == OpFieldSub,
          [x, m] <- blt' ^. builtinAppArgs,
          m == n ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldSub,
          [NBlt blt', n] <- _builtinAppArgs,
          blt' ^. builtinAppOp == OpFieldAdd,
          [x, m] <- blt' ^. builtinAppArgs ->
            if
              | m == n ->
                  x
              | x == n ->
                  m
              | otherwise ->
                  node
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldAdd,
          [n, NBlt blt'] <- _builtinAppArgs,
          blt' ^. builtinAppOp == OpFieldSub,
          [x, m] <- blt' ^. builtinAppArgs,
          m == n ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldAdd || _builtinAppOp == OpFieldSub,
          [x, NCst (Constant _ (ConstField f))] <- _builtinAppArgs,
          fieldToInteger f == 0 ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldAdd,
          [NCst (Constant _ (ConstField f)), x] <- _builtinAppArgs,
          fieldToInteger f == 0 ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldMul,
          [_, c@(NCst (Constant _ (ConstField f)))] <- _builtinAppArgs,
          fieldToInteger f == 0 ->
            c
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldMul,
          [c@(NCst (Constant _ (ConstField f))), _] <- _builtinAppArgs,
          fieldToInteger f == 0 ->
            c
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldMul,
          [x, NCst (Constant _ (ConstField f))] <- _builtinAppArgs,
          fieldToInteger f == 1 ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldMul,
          [NCst (Constant _ (ConstField f)), x] <- _builtinAppArgs,
          fieldToInteger f == 1 ->
            x
      NBlt BuiltinApp {..}
        | _builtinAppOp == OpFieldDiv,
          [x, NCst (Constant _ (ConstField f))] <- _builtinAppArgs,
          fieldToInteger f == 1 ->
            x
      _ -> node

simplifyArithmetic :: Module -> Module
simplifyArithmetic = mapAllNodes convertNode
