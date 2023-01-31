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
      Show {} -> 0
      StrToInt {} -> 0
      Assign {} -> 0
      Trace {} -> 0
      Dump -> 0
      Failure {} -> 0
      Prealloc InstrPrealloc {..} ->
        length _instrPreallocLiveVars
      Alloc {} -> 0
      AllocClosure {} -> 0
      ExtendClosure {} -> 0
      Call InstrCall {..} | _instrCallIsTail -> 0
      Call InstrCall {..} ->
        length _instrCallLiveVars + 1
      CallClosures InstrCallClosures {..}
        | _instrCallClosuresIsTail ->
            length _instrCallClosuresArgs
              + 1
              + lims
                ^. limitsDispatchStackSize
      CallClosures InstrCallClosures {..} ->
        length _instrCallClosuresLiveVars
          + length _instrCallClosuresArgs
          + 1
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

computeStringMap :: HashMap Text Int -> Code -> HashMap Text Int
computeStringMap strs = snd . run . execState (HashMap.size strs, strs) . mapM go
  where
    go :: (Member (State (Int, HashMap Text Int)) r) => Instruction -> Sem r ()
    go = \case
      Nop -> return ()
      Binop BinaryOp {..} -> do
        goVal _binaryOpArg1
        goVal _binaryOpArg2
      Show InstrShow {..} -> do
        goVal _instrShowValue
      StrToInt InstrStrToInt {..} -> do
        goVal _instrStrToIntValue
      Assign InstrAssign {..} ->
        goVal _instrAssignValue
      Trace InstrTrace {..} ->
        goVal _instrTraceValue
      Dump -> return ()
      Failure InstrFailure {..} ->
        goVal _instrFailureValue
      Prealloc {} -> return ()
      Alloc InstrAlloc {..} ->
        mapM_ goVal _instrAllocArgs
      AllocClosure InstrAllocClosure {..} ->
        mapM_ goVal _instrAllocClosureArgs
      ExtendClosure InstrExtendClosure {..} ->
        mapM_ goVal _instrExtendClosureArgs
      Call InstrCall {..} ->
        mapM_ goVal _instrCallArgs
      CallClosures InstrCallClosures {..} ->
        mapM_ goVal _instrCallClosuresArgs
      Return InstrReturn {..} ->
        goVal _instrReturnValue
      Branch InstrBranch {..} -> do
        goVal _instrBranchValue
        mapM_ go _instrBranchTrue
        mapM_ go _instrBranchFalse
      Case InstrCase {..} -> do
        goVal _instrCaseValue
        mapM_ (mapM_ go . (^. caseBranchCode)) _instrCaseBranches
        maybe (return ()) (mapM_ go) _instrCaseDefault

    goVal :: (Member (State (Int, HashMap Text Int)) r) => Value -> Sem r ()
    goVal = \case
      ConstString str ->
        modify'
          ( \(sid :: Int, sstrs) ->
              if
                  | HashMap.member str sstrs -> (sid, sstrs)
                  | otherwise -> (sid + 1, HashMap.insert str sid sstrs)
          )
      _ -> return ()

data ExtraInfo = ExtraInfo
  { _extraInfoTable :: InfoTable,
    _extraInfoUIDs :: HashMap Tag Int,
    _extraInfoFUIDs :: HashMap Symbol Int,
    _extraInfoStringMap :: HashMap Text Int,
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
      _extraInfoStringMap =
        foldr
          (\fi mp -> computeStringMap mp (fi ^. functionCode))
          HashMap.empty
          (tab ^. infoFunctions),
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
