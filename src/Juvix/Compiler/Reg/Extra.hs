module Juvix.Compiler.Reg.Extra where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Backend
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Language

userConstrs :: InfoTable -> [ConstructorInfo]
userConstrs tab =
  filter (\ci -> not (isBuiltinTag (ci ^. constructorTag))) $
    HashMap.elems (tab ^. infoConstrs)

-- | Compute the maximum runtime stack height
computeMaxStackHeight :: Limits -> Code -> Int
computeMaxStackHeight lims = maximum . map go
  where
    go :: Instruction -> Int
    go = \case
      Nop -> 0
      Binop {} -> 0
      Assign {} -> 0
      Trace {} -> 0
      Dump -> 0
      Failure {} -> 0
      Prealloc InstrPrealloc {..} ->
        length _instrPreallocLiveVars
      Alloc {} -> 0
      AllocClosure {} -> 0
      ExtendClosure {} -> 0
      Call InstrCall {..} ->
        length _instrCallLiveVars + 1
      CallClosures InstrCallClosures {..} ->
        length _instrCallClosuresLiveVars
          + length _instrCallClosuresArgs
          + lims
          ^. limitsDispatchStackSize
      Return {} -> 0
      Branch InstrBranch {..} ->
        max
          (computeMaxStackHeight lims _instrBranchTrue)
          (computeMaxStackHeight lims _instrBranchFalse)
      Case InstrCase {..} ->
        max
          ( maximum
              ( map
                  (computeMaxStackHeight lims . (^. caseBranchCode))
                  _instrCaseBranches
              )
          )
          (maybe 0 (computeMaxStackHeight lims) _instrCaseDefault)

data ExtraInfo = ExtraInfo
  { _extraInfoTable :: InfoTable,
    _extraInfoUIDs :: HashMap Tag Int,
    _extraInfoFUIDs :: HashMap Symbol Int,
    _extraInfoMaxStackHeight :: HashMap Symbol Int,
    _extraInfoMaxArgsNum :: Int,
    _extraInfoConstrsNum :: Int,
    _extraInfoFunctionsNum :: Int
  }

makeLenses ''ExtraInfo

computeUIDs :: Limits -> InfoTable -> HashMap Tag Int
computeUIDs lims tab =
  HashMap.fromList $
    zipWith
      (\ci uid -> (ci ^. constructorTag, uid))
      (userConstrs tab)
      [lims ^. limitsBuiltinUIDsNum ..]

computeFUIDs :: InfoTable -> HashMap Symbol Int
computeFUIDs tab =
  HashMap.fromList $
    zipWith
      (\fi fuid -> (fi ^. functionSymbol, fuid))
      (HashMap.elems (tab ^. infoFunctions))
      [0 ..]

computeExtraInfo :: Limits -> InfoTable -> ExtraInfo
computeExtraInfo lims tab =
  ExtraInfo
    { _extraInfoTable = tab,
      _extraInfoUIDs = computeUIDs lims tab,
      _extraInfoFUIDs = computeFUIDs tab,
      _extraInfoMaxStackHeight =
        HashMap.map
          (computeMaxStackHeight lims . (^. functionCode))
          (tab ^. infoFunctions),
      _extraInfoMaxArgsNum =
        maximum (map (^. functionArgsNum) (HashMap.elems (tab ^. infoFunctions))),
      _extraInfoConstrsNum =
        length (userConstrs tab) + lims ^. limitsBuiltinUIDsNum,
      _extraInfoFunctionsNum =
        HashMap.size (tab ^. infoFunctions)
    }
