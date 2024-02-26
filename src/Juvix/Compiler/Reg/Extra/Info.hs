module Juvix.Compiler.Reg.Extra.Info where

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
      Unop {} -> 0
      Assign {} -> 0
      Trace {} -> 0
      Dump -> 0
      Failure {} -> 0
      Prealloc InstrPrealloc {..} ->
        length _instrPreallocLiveVars
      Alloc {} -> 0
      AllocClosure {} -> 0
      ExtendClosure {} -> 0
      TailCall {} -> 0
      Call InstrCall {..} ->
        length _instrCallLiveVars + 1
      TailCallClosures InstrTailCallClosures {..} ->
        length _instrTailCallClosuresArgs
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
        maximum1
          ( maybe 0 (computeMaxStackHeight lims) _instrCaseDefault
              :| ( map
                     (computeMaxStackHeight lims . (^. caseBranchCode))
                     _instrCaseBranches
                 )
          )
      Block InstrBlock {..} ->
        computeMaxStackHeight lims _instrBlockCode

computeMaxCallClosuresArgsNum :: Code -> Int
computeMaxCallClosuresArgsNum = maximum . map go
  where
    go :: Instruction -> Int
    go = \case
      Nop -> 0
      Binop {} -> 0
      Unop {} -> 0
      Assign {} -> 0
      Trace {} -> 0
      Dump -> 0
      Failure {} -> 0
      Prealloc InstrPrealloc {} -> 0
      Alloc {} -> 0
      AllocClosure {} -> 0
      ExtendClosure {} -> 0
      Call {} -> 0
      TailCall {} -> 0
      CallClosures InstrCallClosures {..} ->
        length _instrCallClosuresArgs
      TailCallClosures InstrTailCallClosures {..} ->
        length _instrTailCallClosuresArgs
      Return {} -> 0
      Branch InstrBranch {..} ->
        max
          (computeMaxCallClosuresArgsNum _instrBranchTrue)
          (computeMaxCallClosuresArgsNum _instrBranchFalse)
      Case InstrCase {..} ->
        maximum1
          ( maybe 0 computeMaxCallClosuresArgsNum _instrCaseDefault
              :| ( map
                     (computeMaxCallClosuresArgsNum . (^. caseBranchCode))
                     _instrCaseBranches
                 )
          )
      Block InstrBlock {..} ->
        computeMaxCallClosuresArgsNum _instrBlockCode

computeStringMap :: HashMap Text Int -> Code -> HashMap Text Int
computeStringMap strs = snd . run . execState (HashMap.size strs, strs) . mapM go
  where
    go :: (Member (State (Int, HashMap Text Int)) r) => Instruction -> Sem r ()
    go = \case
      Nop -> return ()
      Binop InstrBinop {..} -> do
        goVal _instrBinopArg1
        goVal _instrBinopArg2
      Unop InstrUnop {..} -> do
        goVal _instrUnopArg
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
      TailCall InstrTailCall {..} ->
        mapM_ goVal _instrTailCallArgs
      CallClosures InstrCallClosures {..} ->
        mapM_ goVal _instrCallClosuresArgs
      TailCallClosures InstrTailCallClosures {..} ->
        mapM_ goVal _instrTailCallClosuresArgs
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
      Block InstrBlock {..} ->
        mapM_ go _instrBlockCode

    goVal :: (Member (State (Int, HashMap Text Int)) r) => Value -> Sem r ()
    goVal = \case
      Const (ConstString str) ->
        modify'
          ( \(sid :: Int, sstrs) ->
              if
                  | HashMap.member str sstrs -> (sid, sstrs)
                  | otherwise -> (sid + 1, HashMap.insert str sid sstrs)
          )
      _ -> return ()

computeLocalVarsNum :: Code -> Int
computeLocalVarsNum = maximum . map go
  where
    go :: Instruction -> Int
    go = \case
      Nop -> 0
      Binop InstrBinop {..} -> goVarRef _instrBinopResult
      Unop InstrUnop {..} -> goVarRef _instrUnopResult
      Assign InstrAssign {..} -> goVarRef _instrAssignResult
      Trace {} -> 0
      Dump -> 0
      Failure {} -> 0
      Prealloc InstrPrealloc {} -> 0
      Alloc InstrAlloc {..} -> goVarRef _instrAllocResult
      AllocClosure InstrAllocClosure {..} -> goVarRef _instrAllocClosureResult
      ExtendClosure InstrExtendClosure {..} -> goVarRef _instrExtendClosureResult
      Call InstrCall {..} -> goVarRef _instrCallResult
      TailCall {} -> 0
      CallClosures InstrCallClosures {..} -> goVarRef _instrCallClosuresResult
      TailCallClosures {} -> 0
      Return {} -> 0
      Branch InstrBranch {..} ->
        max
          (computeLocalVarsNum _instrBranchTrue)
          (computeLocalVarsNum _instrBranchFalse)
      Case InstrCase {..} ->
        maximum1
          ( maybe 0 computeLocalVarsNum _instrCaseDefault
              :| ( map
                     (computeLocalVarsNum . (^. caseBranchCode))
                     _instrCaseBranches
                 )
          )
      Block InstrBlock {..} ->
        computeLocalVarsNum _instrBlockCode

    goVarRef :: VarRef -> Int
    goVarRef VarRef {..} = case _varRefGroup of
      VarGroupArgs -> 0
      VarGroupLocal -> _varRefIndex + 1

data ExtraInfo = ExtraInfo
  { _extraInfoTable :: InfoTable,
    _extraInfoUIDs :: HashMap Tag Int,
    _extraInfoFUIDs :: HashMap Symbol Int,
    _extraInfoStringMap :: HashMap Text Int,
    _extraInfoMaxStackHeight :: HashMap Symbol Int,
    _extraInfoLocalVarsNum :: HashMap Symbol Int,
    _extraInfoMaxArgsNum :: Int,
    _extraInfoMaxCallClosuresArgsNum :: Int,
    _extraInfoConstrsNum :: Int,
    _extraInfoFunctionsNum :: Int,
    _extraInfoSpecialisedApply :: Int
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
      _extraInfoLocalVarsNum =
        HashMap.map
          (computeLocalVarsNum . (^. functionCode))
          (tab ^. infoFunctions),
      _extraInfoMaxArgsNum =
        maximum (map (^. functionArgsNum) (HashMap.elems (tab ^. infoFunctions))),
      _extraInfoMaxCallClosuresArgsNum =
        maximum1
          ( lims ^. limitsSpecialisedApply
              :| map (computeMaxCallClosuresArgsNum . (^. functionCode)) (HashMap.elems (tab ^. infoFunctions))
          ),
      _extraInfoConstrsNum =
        length (userConstrs tab) + lims ^. limitsBuiltinUIDsNum,
      _extraInfoFunctionsNum =
        HashMap.size (tab ^. infoFunctions),
      _extraInfoSpecialisedApply = lims ^. limitsSpecialisedApply
    }
