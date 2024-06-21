module Juvix.Compiler.Reg.Transformation.Blocks.Cairo where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Reg.Data.Blocks.CallGraph
import Juvix.Compiler.Reg.Extra.Blocks
import Juvix.Compiler.Reg.Transformation.Blocks.Base

computeCairoBlts :: InfoTable -> InfoTable
computeCairoBlts tab = mapT (const (computeBlockCairoBlts bltFuns)) tab
  where
    bltFuns = computeCairoBltFuns tab

computeBlockCairoBlts :: HashSet Symbol -> Block -> Block
computeBlockCairoBlts bltFuns block =
  block'
    { _blockUsesCairoBuiltins =
        usesCairoBuiltins' (^. blockUsesCairoBuiltins) bltFuns block'
    }
  where
    block' =
      over blockNext (fmap (computeBlockCairoBlts bltFuns)) $
        overSubBlocks (computeBlockCairoBlts bltFuns) block

-- | Compute symbols of functions which _may_ depend (directly or indirectly) on
-- Cairo builtins (we include closure calls)
computeCairoBltFuns :: InfoTable -> HashSet Symbol
computeCairoBltFuns tab =
  computeAncestors callGraph startNodes
  where
    callGraph :: CallGraph
    callGraph = createCallGraph tab

    startNodes :: [Symbol]
    startNodes =
      map (^. functionSymbol)
        . filter (usesCairoBuiltins . (^. functionCode))
        . HashMap.elems
        $ (tab ^. infoFunctions)

usesCairoBuiltins :: Block -> Bool
usesCairoBuiltins = usesCairoBuiltins' usesCairoBuiltins mempty

usesCairoBuiltins' :: (Block -> Bool) -> HashSet Symbol -> Block -> Bool
usesCairoBuiltins' usesRec bltFuns block =
  maybe False usesRec (block ^. blockNext)
    || any isCairo (block ^. blockBody)
    || maybe False isClosureCall (block ^. blockFinal)
    || maybe False isCairoCall (block ^. blockFinal)
    || any usesRec (getSubBlocks block)
  where
    isCairo :: Instruction -> Bool
    isCairo = \case
      Cairo {} -> True
      _ -> False

    isClosureCall :: FinalInstruction -> Bool
    isClosureCall = \case
      Call InstrCall {..} | CallClosure {} <- _instrCallType -> True
      TailCall InstrTailCall {..} | CallClosure {} <- _instrTailCallType -> True
      _ -> False

    isCairoCall :: FinalInstruction -> Bool
    isCairoCall = \case
      Call InstrCall {..}
        | CallFun sym <- _instrCallType, HashSet.member sym bltFuns -> True
      TailCall InstrTailCall {..}
        | CallFun sym <- _instrTailCallType, HashSet.member sym bltFuns -> True
      _ -> False
