module Juvix.Compiler.Casm.Extra.LabelInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Casm.Data.LabelInfo
import Juvix.Compiler.Casm.Language

computeLabelInfo' :: (Instruction -> Int) -> [Instruction] -> LabelInfo
computeLabelInfo' sizeFun = go 0 mempty
  where
    go :: Address -> LabelInfo -> [Instruction] -> LabelInfo
    go addr acc = \case
      [] ->
        acc
      i@(Label (LabelRef sym _)) : instrs' ->
        go (addr + sizeFun i) (over labelInfoTable (HashMap.insert sym addr) acc) instrs'
      i : instrs' ->
        go (addr + sizeFun i) acc instrs'

computeLabelInfo :: [Instruction] -> LabelInfo
computeLabelInfo = computeLabelInfo' (const 1)
