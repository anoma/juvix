module Juvix.Compiler.Tree.Transformation.TempHeight where

import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Transformation.Base

computeFunctionTempHeight :: Node -> Node
computeFunctionTempHeight = umapN go
  where
    go :: Int -> Node -> Node
    go k = \case
      MemRef (NodeMemRef i (DRef (TempRef r))) ->
        let r' = set refTempTempHeight (Just k) r
         in MemRef $ NodeMemRef i $ DRef (TempRef r')
      MemRef (NodeMemRef i (ConstrRef field@Field {_fieldRef = TempRef r})) ->
        let r' = set refTempTempHeight (Just k) r
         in MemRef $
              NodeMemRef
                i
                ( ConstrRef
                    field
                      { _fieldRef = TempRef r'
                      }
                )
      node -> node

computeTempHeight :: InfoTable -> InfoTable
computeTempHeight = mapT (const computeFunctionTempHeight)
