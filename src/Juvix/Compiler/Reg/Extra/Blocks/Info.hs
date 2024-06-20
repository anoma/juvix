module Juvix.Compiler.Reg.Extra.Blocks.Info
  ( module Juvix.Compiler.Tree.Extra.Info,
    module Juvix.Compiler.Reg.Extra.Blocks.Info,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Reg.Data.Blocks.CallGraph
import Juvix.Compiler.Reg.Data.Blocks.InfoTable
import Juvix.Compiler.Reg.Extra.Blocks
import Juvix.Compiler.Reg.Language.Blocks
import Juvix.Compiler.Tree.Extra.Info

data ExtraInfo = ExtraInfo
  { _extraInfoTable :: InfoTable,
    -- | Globally unique IDs for function symbols
    _extraInfoFUIDs :: HashMap Symbol Int,
    -- | IDs for constructor tags, consecutive starting from 0 for each
    -- inductive type separately
    _extraInfoCIDs :: HashMap Tag Int,
    -- | Symbols of functions which depend (directly or indirectly) on Cairo
    -- builtins
    _extraInfoCairoDepFuns :: HashSet Symbol
  }

makeLenses ''ExtraInfo

computeExtraInfo :: InfoTable -> ExtraInfo
computeExtraInfo tab =
  ExtraInfo
    { _extraInfoTable = tab,
      _extraInfoFUIDs = computeFUIDs tab,
      _extraInfoCIDs = computeCIDs tab,
      _extraInfoCairoDepFuns = computeCairoDepFuns tab
    }

computeCairoDepFuns :: InfoTable -> HashSet Symbol
computeCairoDepFuns tab =
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
    usesCairoBuiltins block =
      maybe False usesCairoBuiltins (block ^. blockNext)
        || any isCairo (block ^. blockBody)
        || any usesCairoBuiltins (getSubBlocks block)
      where
        isCairo :: Instruction -> Bool
        isCairo = \case
          Cairo {} -> True
          _ -> False
