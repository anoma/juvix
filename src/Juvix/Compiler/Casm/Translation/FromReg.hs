module Juvix.Compiler.Casm.Translation.FromReg where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Casm.Data.LabelInfoBuilder
import Juvix.Compiler.Casm.Data.Limits
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Extra.Base
import Juvix.Compiler.Casm.Extra.Stdlib
import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Translation.FromReg.CasmBuilder
import Juvix.Compiler.Reg.Data.Blocks.InfoTable qualified as Reg
import Juvix.Compiler.Reg.Extra.Blocks.Info qualified as Reg
import Juvix.Compiler.Reg.Language.Blocks qualified as Reg
import Juvix.Compiler.Tree.Evaluator.Builtins qualified as Reg
import Juvix.Data.Field

fromReg :: Reg.InfoTable -> Result
fromReg tab = uncurry Result $ run $ runLabelInfoBuilderWithNextId (Reg.getNextSymbolId tab) $ do
  let initialOffset :: Int = 2
  (blts, binstrs) <- addStdlibBuiltins initialOffset
  let cinstrs = concatMap (mkFunCall . fst) $ sortOn snd $ HashMap.toList (info ^. Reg.extraInfoFUIDs)
  endSym <- freshSymbol
  let endName :: Text = "__juvix_end"
      endLab = LabelRef endSym (Just endName)
  (addr, instrs) <- second (concat . reverse) <$> foldM (goFun blts endLab) (initialOffset + length binstrs + length cinstrs, []) (tab ^. Reg.infoFunctions)
  eassert (addr == length instrs + length cinstrs + length binstrs + initialOffset)
  registerLabelName endSym endName
  registerLabelAddress endSym addr
  let mainSym = fromJust $ tab ^. Reg.infoMainFunction
      mainName = fromJust (HashMap.lookup mainSym (tab ^. Reg.infoFunctions)) ^. Reg.functionName
      callInstr = mkCallRel (Lab $ LabelRef mainSym (Just mainName))
      jmpInstr = mkJumpRel (Val $ Lab endLab)
  return $ callInstr : jmpInstr : binstrs ++ cinstrs ++ instrs ++ [Label endLab]
  where
    info :: Reg.ExtraInfo
    info = Reg.computeExtraInfo tab

    mkFunCall :: Symbol -> [Instruction]
    mkFunCall sym =
      [ mkCallRel $ Lab $ LabelRef sym (Just $ Reg.lookupFunInfo tab sym ^. Reg.functionName),
        Return,
        Nop
      ]

    getTagId :: Tag -> Int
    getTagId tag =
      1 + 2 * fromJust (HashMap.lookup tag (info ^. Reg.extraInfoCIDs))

    unsupported :: Text -> a
    unsupported what = error ("Cairo backend: unsupported: " <> what)

    goFun :: forall r. (Member LabelInfoBuilder r) => StdlibBuiltins -> LabelRef -> (Address, [[Instruction]]) -> Reg.FunctionInfo -> Sem r (Address, [[Instruction]])
    goFun blts failLab (addr0, acc) funInfo = do
      let sym = funInfo ^. Reg.functionSymbol
          funName = funInfo ^. Reg.functionName
      registerLabelName sym funName
      registerLabelAddress sym addr0
      let lab = Label $ LabelRef sym (Just funName)
          block = funInfo ^. Reg.functionCode
          pre = [lab]
          addr1 = addr0 + length pre
          n = funInfo ^. Reg.functionArgsNum
      let vars =
            HashMap.fromList $
              map (\k -> (Reg.VarRef Reg.VarGroupArgs k Nothing, -3 - k)) [0 .. n - 1]
      instrs <-
        fmap fst
          . runCasmBuilder addr1 vars
          . runOutputList
          $ goBlock blts failLab mempty Nothing block
      return (addr1 + length instrs, (pre ++ instrs) : acc)

    goBlock :: forall r. (Members '[LabelInfoBuilder, CasmBuilder, Output Instruction] r) => StdlibBuiltins -> LabelRef -> HashSet Reg.VarRef -> Maybe Reg.VarRef -> Reg.Block -> Sem r ()
    goBlock blts failLab liveVars0 mout Reg.Block {..} = do
      mapM_ goInstr _blockBody
      case _blockNext of
        Just block' -> do
          eassert (isJust _blockFinal)
          goFinalInstr (block' ^. Reg.blockLiveVars) (fromJust _blockFinal)
          goBlock blts failLab liveVars0 mout block'
        Nothing -> case _blockFinal of
          Just instr ->
            goFinalInstr liveVars0 instr
          Nothing -> do
            eassert (isJust mout)
            eassert (HashSet.member (fromJust mout) liveVars0)
            goCallBlock Nothing liveVars0
      where
        output'' :: Instruction -> Sem r ()
        output'' i = do
          output i
          incPC 1

        output' :: Int -> Instruction -> Sem r ()
        output' apOff i = do
          output'' i
          incAP apOff

        goCallBlock :: Maybe Reg.VarRef -> HashSet Reg.VarRef -> Sem r ()
        goCallBlock outVar liveVars = do
          let liveVars' = toList (maybe liveVars (flip HashSet.delete liveVars) outVar)
              n = length liveVars'
              vars =
                HashMap.fromList $
                  maybe [] (\var -> [(var, -3 - n)]) outVar
                    ++ zipWithExact (\var k -> (var, -3 - k)) liveVars' [0 .. n - 1]
          mapM_ (mkMemRef >=> goAssignAp . Val . Ref) (reverse liveVars')
          output'' (mkCallRel $ Imm 3)
          output'' Return
          output'' Nop
          setAP 0
          setVars vars

        goLocalBlock :: Int -> HashMap Reg.VarRef Int -> HashSet Reg.VarRef -> Maybe Reg.VarRef -> Reg.Block -> Sem r ()
        goLocalBlock ap0 vars liveVars mout' block = do
          setAP ap0
          setVars vars
          goBlock blts failLab liveVars mout' block

        ----------------------------------------------------------------------
        -- The mk* functions don't change the builder state, may only read it
        ----------------------------------------------------------------------

        mkConst :: Reg.Constant -> Integer
        mkConst = \case
          Reg.ConstInt x -> x
          Reg.ConstBool True -> 0
          Reg.ConstBool False -> 1
          Reg.ConstField f -> fieldToInteger f
          Reg.ConstUnit -> 0
          Reg.ConstVoid -> 0
          Reg.ConstString {} -> unsupported "strings"

        mkLoad :: Reg.ConstrField -> Sem r RValue
        mkLoad Reg.ConstrField {..} = do
          v <- mkMemRef _constrFieldRef
          return $ Load $ LoadValue v (toOffset _constrFieldIndex + 1)

        mkMemRef :: Reg.VarRef -> Sem r MemRef
        mkMemRef vr = do
          v <- lookupVar' vr
          return $ MemRef Fp (toOffset v)

        mkRValue :: Reg.Value -> Sem r RValue
        mkRValue = \case
          Reg.ValConst c -> return $ Val $ Imm $ mkConst c
          Reg.CRef x -> mkLoad x
          Reg.VRef x -> Val . Ref <$> mkMemRef x

        ---------------------------------------------------------------------
        -- Instruction
        ---------------------------------------------------------------------

        goInstr :: Reg.Instruction -> Sem r ()
        goInstr = \case
          Reg.Binop x -> goBinop x
          Reg.Unop x -> goUnop x
          Reg.Assign x -> goAssign x
          Reg.Alloc x -> goAlloc x
          Reg.AllocClosure x -> goAllocClosure x
          Reg.Trace x -> goTrace x
          Reg.Dump -> unsupported "dump"
          Reg.Failure x -> goFail x

        goAssignVar :: Reg.VarRef -> RValue -> Sem r ()
        goAssignVar vr val = do
          off <- getAP
          insertVar vr off
          goAssignAp val

        goAssignAp :: RValue -> Sem r ()
        goAssignAp val = do
          output' 1 (mkAssignAp val)

        goAssignValue :: Reg.VarRef -> Reg.Value -> Sem r ()
        goAssignValue vr v = mkRValue v >>= goAssignVar vr

        goAssignApValue :: Reg.Value -> Sem r ()
        goAssignApValue v = mkRValue v >>= goAssignAp

        goValue :: Reg.Value -> Sem r Value
        goValue = \case
          Reg.ValConst c -> return $ Imm $ mkConst c
          Reg.CRef x -> do
            v <- mkLoad x
            goAssignAp v
            return $ Ref $ MemRef Ap (-1)
          Reg.VRef x -> do
            v <- Ref <$> mkMemRef x
            return v

        goExtraBinop :: ExtraOpcode -> Reg.VarRef -> MemRef -> Value -> Sem r ()
        goExtraBinop op res arg1 arg2 = do
          off <- getAP
          insertVar res off
          output' 1 $
            ExtraBinop
              InstrExtraBinop
                { _instrExtraBinopOpcode = op,
                  _instrExtraBinopResult = MemRef Ap 0,
                  _instrExtraBinopArg1 = arg1,
                  _instrExtraBinopArg2 = arg2,
                  _instrExtraBinopIncAp = True
                }

        goNativeBinop :: Opcode -> Reg.VarRef -> MemRef -> Value -> Sem r ()
        goNativeBinop op res arg1 arg2 = goAssignVar res binop
          where
            binop =
              Binop
                BinopValue
                  { _binopValueOpcode = op,
                    _binopValueArg1 = arg1,
                    _binopValueArg2 = arg2
                  }

        goEq :: Reg.VarRef -> MemRef -> Value -> Sem r ()
        goEq res arg1 arg2 = goExtraBinop FieldSub res arg1 arg2

        goIntLe :: Reg.VarRef -> MemRef -> Value -> Sem r ()
        goIntLe res arg1 arg2 = case arg2 of
          Imm v ->
            goExtraBinop IntLt res arg1 (Imm (v + 1))
          Ref mref -> do
            output' 1 inc
            goExtraBinop IntLt res (adjustAp 1 arg1) (Ref $ MemRef Ap (-1))
            where
              inc =
                Assign
                  InstrAssign
                    { _instrAssignResult = MemRef Ap 0,
                      _instrAssignValue =
                        Binop
                          BinopValue
                            { _binopValueArg1 = mref,
                              _binopValueArg2 = Imm 1,
                              _binopValueOpcode = FieldAdd
                            },
                      _instrAssignIncAp = True
                    }
          Lab {} -> impossible

        goOpArgsNum :: Reg.VarRef -> MemRef -> Sem r ()
        goOpArgsNum res v = do
          goAssignAp (Val $ Imm $ fromIntegral casmMaxFunctionArgs + 1)
          goAssignAp (Load $ LoadValue (adjustAp 1 v) casmClosureArgsNumOffset)
          goExtraBinop FieldSub res (MemRef Ap (-2)) (Ref $ MemRef Ap (-1))

        goBinop' :: Reg.BinaryOp -> Reg.VarRef -> MemRef -> Value -> Sem r ()
        goBinop' op res arg1 arg2 = case op of
          Reg.OpIntAdd ->
            goExtraBinop IntAdd res arg1 arg2
          Reg.OpIntSub ->
            goExtraBinop IntSub res arg1 arg2
          Reg.OpIntMul ->
            goExtraBinop IntMul res arg1 arg2
          Reg.OpIntDiv ->
            goExtraBinop IntDiv res arg1 arg2
          Reg.OpIntMod ->
            goExtraBinop IntMod res arg1 arg2
          Reg.OpIntLt ->
            goExtraBinop IntLt res arg1 arg2
          Reg.OpIntLe ->
            goIntLe res arg1 arg2
          Reg.OpFieldAdd ->
            goNativeBinop FieldAdd res arg1 arg2
          Reg.OpFieldSub ->
            goExtraBinop FieldSub res arg1 arg2
          Reg.OpFieldMul ->
            goNativeBinop FieldMul res arg1 arg2
          Reg.OpFieldDiv ->
            goExtraBinop FieldDiv res arg1 arg2
          Reg.OpEq ->
            goEq res arg1 arg2
          Reg.OpStrConcat ->
            unsupported "strings"

        goBinop :: Reg.InstrBinop -> Sem r ()
        goBinop x@Reg.InstrBinop {..} = case _instrBinopArg1 of
          Reg.ValConst c1 -> case _instrBinopArg2 of
            Reg.ValConst c2 -> case Reg.evalBinop' _instrBinopOpcode c1 c2 of
              Left err -> error err
              Right c ->
                goAssignVar _instrBinopResult (Val $ Imm $ mkConst c)
            _ ->
              goBinop
                x
                  { Reg._instrBinopArg1 = _instrBinopArg2,
                    Reg._instrBinopArg2 = _instrBinopArg1
                  }
          Reg.CRef ctr1 -> do
            v1 <- mkLoad ctr1
            goAssignAp v1
            v2 <- goValue _instrBinopArg2
            case _instrBinopArg2 of
              Reg.CRef {} -> do
                goBinop' _instrBinopOpcode _instrBinopResult (MemRef Ap (-2)) v2
              _ -> do
                goBinop' _instrBinopOpcode _instrBinopResult (MemRef Ap (-1)) v2
          Reg.VRef var1 -> do
            ref <- mkMemRef var1
            v2 <- goValue _instrBinopArg2
            goBinop' _instrBinopOpcode _instrBinopResult ref v2

        goUnop :: Reg.InstrUnop -> Sem r ()
        goUnop Reg.InstrUnop {..} = case _instrUnopOpcode of
          Reg.OpShow -> unsupported "strings"
          Reg.OpStrToInt -> unsupported "strings"
          Reg.OpFieldToInt -> goAssignValue _instrUnopResult _instrUnopArg
          Reg.OpIntToField -> goAssignValue _instrUnopResult _instrUnopArg
          Reg.OpArgsNum -> do
            v <- goValue _instrUnopArg
            case v of
              Ref mr -> do
                goOpArgsNum _instrUnopResult mr
              Imm {} -> impossible
              Lab {} -> impossible

        goAssign :: Reg.InstrAssign -> Sem r ()
        goAssign Reg.InstrAssign {..} =
          goAssignValue _instrAssignResult _instrAssignValue

        goAllocCall :: Reg.VarRef -> Sem r ()
        goAllocCall res = do
          output' 4 $ mkCallRel $ Lab $ LabelRef (blts ^. stdlibGetRegs) (Just (blts ^. stdlibGetRegsName))
          goNativeBinop FieldAdd res (MemRef Ap (-2)) (Imm 3)

        goAlloc :: Reg.InstrAlloc -> Sem r ()
        goAlloc Reg.InstrAlloc {..} = do
          goAllocCall _instrAllocResult
          goAssignAp (Val $ Imm $ fromIntegral tagId)
          mapM_ goAssignApValue _instrAllocArgs
          where
            tagId = getTagId _instrAllocTag

        goAllocClosure :: Reg.InstrAllocClosure -> Sem r ()
        goAllocClosure Reg.InstrAllocClosure {..} = do
          goAllocCall _instrAllocClosureResult
          goAssignAp (Val $ Imm $ fromIntegral $ 1 + 3 * fuid)
          goAssignAp (Val $ Imm $ fromIntegral $ casmMaxFunctionArgs + 1 - storedArgsNum)
          goAssignAp (Val $ Imm $ fromIntegral $ casmMaxFunctionArgs + 1 - leftArgsNum)
          mapM_ goAssignApValue _instrAllocClosureArgs
          where
            fuid = fromJust $ HashMap.lookup _instrAllocClosureSymbol (info ^. Reg.extraInfoFUIDs)
            storedArgsNum = length _instrAllocClosureArgs
            leftArgsNum = _instrAllocClosureExpectedArgsNum - storedArgsNum

        goTrace :: Reg.InstrTrace -> Sem r ()
        goTrace Reg.InstrTrace {..} = do
          v <- mkRValue _instrTraceValue
          output' 0 $ Trace (InstrTrace v)

        goFail :: Reg.InstrFailure -> Sem r ()
        goFail Reg.InstrFailure {..} = do
          v <- mkRValue _instrFailureValue
          output' 0 $ Trace (InstrTrace v)
          output' 0 $ mkJumpRel (Val $ Lab failLab)

        ---------------------------------------------------------------------
        -- FinalInstruction
        ---------------------------------------------------------------------

        goFinalInstr :: HashSet Reg.VarRef -> Reg.FinalInstruction -> Sem r ()
        goFinalInstr liveVars = \case
          Reg.ExtendClosure x -> goExtendClosure liveVars x
          Reg.Call x -> goCall liveVars x
          Reg.TailCall x -> goTailCall x
          Reg.Return x -> goReturn x
          Reg.Branch x -> goBranch liveVars x
          Reg.Case x -> goCase liveVars x

        goExtendClosure :: HashSet Reg.VarRef -> Reg.InstrExtendClosure -> Sem r ()
        goExtendClosure liveVars Reg.InstrExtendClosure {..} = do
          mapM_ goAssignApValue _instrExtendClosureArgs
          goAssignAp (Val $ Imm $ fromIntegral $ length _instrExtendClosureArgs)
          val <- mkMemRef _instrExtendClosureValue
          goAssignAp (Val $ Ref val)
          output'' $ mkCallRel $ Lab $ LabelRef (blts ^. stdlibExtendClosure) (Just (blts ^. stdlibExtendClosureName))
          goCallBlock (Just _instrExtendClosureResult) liveVars

        goCall' :: Reg.CallType -> [Reg.Value] -> Sem r ()
        goCall' ct args = case ct of
          Reg.CallFun sym -> do
            mapM_ goAssignApValue (reverse args)
            output'' $ mkCallRel $ Lab $ LabelRef sym (Just funName)
            where
              funName = Reg.lookupFunInfo tab sym ^. Reg.functionName
          Reg.CallClosure cl -> do
            mapM_ goAssignApValue (reverse args)
            r <- mkMemRef cl
            goAssignAp (Val $ Ref r)
            output'' $ mkCallRel $ Lab $ LabelRef (blts ^. stdlibCallClosure) (Just (blts ^. stdlibCallClosureName))

        goCall :: HashSet Reg.VarRef -> Reg.InstrCall -> Sem r ()
        goCall liveVars Reg.InstrCall {..} = do
          goCall' _instrCallType _instrCallArgs
          goCallBlock (Just _instrCallResult) liveVars

        -- There is no way to make "proper" tail calls in Cairo, because
        -- the only way to set the `fp` register is via the `call` instruction.
        -- So we just translate tail calls into `call` followed by `ret`.
        goTailCall :: Reg.InstrTailCall -> Sem r ()
        goTailCall Reg.InstrTailCall {..} = do
          goCall' _instrTailCallType _instrTailCallArgs
          output'' Return

        goReturn :: Reg.InstrReturn -> Sem r ()
        goReturn Reg.InstrReturn {..} = do
          goAssignApValue _instrReturnValue
          output'' Return

        goBranch :: HashSet Reg.VarRef -> Reg.InstrBranch -> Sem r ()
        goBranch liveVars Reg.InstrBranch {..} = do
          v <- goValue _instrBranchValue
          case v of
            Imm c
              | c == 0 -> goBlock blts failLab liveVars _instrBranchOutVar _instrBranchTrue
              | otherwise -> goBlock blts failLab liveVars _instrBranchOutVar _instrBranchFalse
            Ref r -> do
              symFalse <- freshSymbol
              symEnd <- freshSymbol
              let labFalse = LabelRef symFalse Nothing
                  labEnd = LabelRef symEnd Nothing
              output'' $ mkJumpIf (Lab labFalse) r
              ap0 <- getAP
              vars <- getVars
              goLocalBlock ap0 vars liveVars _instrBranchOutVar _instrBranchTrue
              -- _instrBranchOutVar is Nothing iff the branch returns
              when (isJust _instrBranchOutVar) $
                output'' (mkJumpRel (Val $ Lab labEnd))
              addrFalse <- getPC
              registerLabelAddress symFalse addrFalse
              output'' $ Label labFalse
              goLocalBlock ap0 vars liveVars _instrBranchOutVar _instrBranchFalse
              addrEnd <- getPC
              registerLabelAddress symEnd addrEnd
              output'' $ Label labEnd
            Lab {} -> impossible

        goLoad :: Reg.Value -> Offset -> Sem r RValue
        goLoad val off = do
          v <- mkRValue val
          case v of
            Val (Ref r) -> return $ Load $ LoadValue r off
            _ -> do
              goAssignAp v
              return $ Load $ LoadValue (MemRef Ap (-1)) off

        goCase :: HashSet Reg.VarRef -> Reg.InstrCase -> Sem r ()
        goCase liveVars Reg.InstrCase {..} = do
          syms <- replicateM (length tags) freshSymbol
          symEnd <- freshSymbol
          let symMap = HashMap.fromList $ zip tags syms
              labs = map (flip LabelRef Nothing) syms
              labEnd = LabelRef symEnd Nothing
              jmps = map (mkJumpRel . Val . Lab) labs
              -- we need the Nop instructions to ensure that the relative jump
              -- offsets in our CASM interpreter correspond to the relative jump
              -- offsets in the Cairo binary representation
              jmps' = concatMap (\i -> [i, Nop]) jmps
          v <- goLoad _instrCaseValue 0
          output'' (mkJumpRel v)
          mapM_ output'' jmps'
          ap0 <- getAP
          vars <- getVars
          mapM_ (goCaseBranch ap0 vars symMap labEnd) _instrCaseBranches
          mapM_ (goDefaultLabel symMap) defaultTags
          whenJust _instrCaseDefault $
            goLocalBlock ap0 vars liveVars _instrCaseOutVar
          addrEnd <- getPC
          registerLabelAddress symEnd addrEnd
          output'' $ Label labEnd
          where
            tags = Reg.lookupInductiveInfo tab _instrCaseInductive ^. Reg.inductiveConstructors
            ctrTags = HashSet.fromList $ map (^. Reg.caseBranchTag) _instrCaseBranches
            defaultTags = filter (not . flip HashSet.member ctrTags) tags

            goCaseBranch :: Int -> HashMap Reg.VarRef Int -> HashMap Tag Symbol -> LabelRef -> Reg.CaseBranch -> Sem r ()
            goCaseBranch ap0 vars symMap labEnd Reg.CaseBranch {..} = do
              let sym = fromJust $ HashMap.lookup _caseBranchTag symMap
                  lab = LabelRef sym Nothing
              addr <- getPC
              registerLabelAddress sym addr
              output'' $ Label lab
              goLocalBlock ap0 vars liveVars _instrCaseOutVar _caseBranchCode
              -- _instrCaseOutVar is Nothing iff the branch returns
              when (isJust _instrCaseOutVar) $
                output'' (mkJumpRel (Val $ Lab labEnd))

            goDefaultLabel :: HashMap Tag Symbol -> Reg.Tag -> Sem r ()
            goDefaultLabel symMap tag = do
              let sym = fromJust $ HashMap.lookup tag symMap
                  lab = LabelRef sym Nothing
              addr <- getPC
              registerLabelAddress sym addr
              output'' $ Label lab
