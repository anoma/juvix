module Juvix.Compiler.Reg.Pretty.Base
  ( module Juvix.Compiler.Reg.Pretty.Base,
    module Juvix.Compiler.Reg.Pretty.Options,
  )
where

import Data.Foldable
import Juvix.Compiler.Reg.Data.InfoTable
import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Reg.Pretty.Options
import Juvix.Compiler.Tree.Pretty.Base qualified as Tree
import Juvix.Compiler.Tree.Pretty.Extra
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

instance PrettyCode VarRef where
  ppCode VarRef {..} = case _varRefName of
    Just n -> return $ variable (quoteName n)
    Nothing -> case _varRefGroup of
      VarGroupArgs -> return $ ppRef Str.arg _varRefIndex
      VarGroupLocal -> return $ ppRef Str.tmp _varRefIndex
    where
      ppRef :: Text -> Index -> Doc Ann
      ppRef str off = variable str <> brackets (integer off)

instance PrettyCode ConstrField where
  ppCode ConstrField {..} = do
    dr <- ppCode _constrFieldRef
    ctr <- Tree.ppConstrName _constrFieldTag
    return $ dr <> dot <> ctr <> brackets (integer _constrFieldIndex)

instance PrettyCode Value where
  ppCode = \case
    Const x -> Tree.ppCode x
    CRef x -> ppCode x
    VRef x -> ppCode x

instance PrettyCode Opcode where
  ppCode = \case
    OpIntAdd -> return $ primitive Str.add_
    OpIntSub -> return $ primitive Str.sub_
    OpIntMul -> return $ primitive Str.mul_
    OpIntDiv -> return $ primitive Str.div_
    OpIntMod -> return $ primitive Str.mod_
    OpIntLt -> return $ primitive Str.lt_
    OpIntLe -> return $ primitive Str.le_
    OpEq -> return $ primitive Str.eq
    OpStrConcat -> return $ primitive Str.instrStrConcat

instance PrettyCode BinaryOp where
  ppCode BinaryOp {..} = do
    res <- ppCode _binaryOpResult
    arg1 <- ppCode _binaryOpArg1
    arg2 <- ppCode _binaryOpArg2
    op <- ppCode _binaryOpCode
    return $ res <+> primitive Str.equal <+> op <+> arg1 <+> arg2

instance PrettyCode InstrShow where
  ppCode InstrShow {..} = do
    res <- ppCode _instrShowResult
    val <- ppCode _instrShowValue
    return $ res <+> primitive Str.equal <+> primitive Str.show_ <+> val

instance PrettyCode InstrStrToInt where
  ppCode InstrStrToInt {..} = do
    res <- ppCode _instrStrToIntResult
    val <- ppCode _instrStrToIntValue
    return $ res <+> primitive Str.equal <+> primitive Str.instrStrToInt <+> val

instance PrettyCode InstrAssign where
  ppCode InstrAssign {..} = do
    res <- ppCode _instrAssignResult
    val <- ppCode _instrAssignValue
    return $ res <+> primitive Str.equal <+> val

instance PrettyCode InstrTrace where
  ppCode InstrTrace {..} = do
    val <- ppCode _instrTraceValue
    return $ primitive Str.trace_ <+> val

instance PrettyCode InstrFailure where
  ppCode InstrFailure {..} = do
    val <- ppCode _instrFailureValue
    return $ primitive Str.fail_ <+> val

instance PrettyCode InstrArgsNum where
  ppCode InstrArgsNum {..} = do
    res <- ppCode _instrArgsNumResult
    val <- ppCode _instrArgsNumValue
    return $ res <+> primitive Str.equal <+> primitive Str.argsnum <+> val

ppLiveVars :: (Member (Reader Options) r) => [VarRef] -> Sem r (Doc Ann)
ppLiveVars vars
  | null vars = return mempty
  | otherwise = do
      vars' <- mapM ppCode vars
      return $ primitive "live:" <+> brackets (hsep vars')

instance PrettyCode InstrPrealloc where
  ppCode InstrPrealloc {..} = do
    vars <- ppLiveVars _instrPreallocLiveVars
    return $ primitive Str.prealloc <+> integer _instrPreallocWordsNum <> vars

instance PrettyCode InstrAlloc where
  ppCode InstrAlloc {..} = do
    res <- ppCode _instrAllocResult
    tag <- Tree.ppConstrName _instrAllocTag
    args <- mapM ppCode _instrAllocArgs
    return $
      res
        <+> primitive Str.equal
        <+> primitive Str.alloc
        <+> tag
        <+> brackets (hsep args)

instance PrettyCode InstrAllocClosure where
  ppCode InstrAllocClosure {..} = do
    res <- ppCode _instrAllocClosureResult
    fn <- Tree.ppFunName _instrAllocClosureSymbol
    args <- mapM ppCode _instrAllocClosureArgs
    return $
      res
        <+> primitive Str.equal
        <+> primitive Str.calloc
        <+> fn
        <+> brackets (hsep args)

instance PrettyCode InstrExtendClosure where
  ppCode InstrExtendClosure {..} = do
    res <- ppCode _instrExtendClosureResult
    fn <- ppCode _instrExtendClosureValue
    args <- mapM ppCode _instrExtendClosureArgs
    return $
      res
        <+> primitive Str.equal
        <+> primitive Str.cextend
        <+> fn
        <+> brackets (hsep args)

instance PrettyCode CallType where
  ppCode = \case
    CallFun sym -> Tree.ppFunName sym
    CallClosure cl -> ppCode cl

instance PrettyCode InstrCall where
  ppCode InstrCall {..} = do
    res <- ppCode _instrCallResult
    fn <- ppCode _instrCallType
    args <- mapM ppCode _instrCallArgs
    let cl = if _instrCallIsTail then Str.tcall else Str.call
    return $
      res
        <+> primitive Str.equal
        <+> primitive cl
        <+> fn
        <+> brackets (hsep args)

instance PrettyCode InstrCallClosures where
  ppCode InstrCallClosures {..} = do
    res <- ppCode _instrCallClosuresResult
    fn <- ppCode _instrCallClosuresValue
    args <- mapM ppCode _instrCallClosuresArgs
    let cl = if _instrCallClosuresIsTail then Str.instrTccall else Str.ccall
    return $
      res
        <+> primitive Str.equal
        <+> primitive cl
        <+> fn
        <+> brackets (hsep args)

instance PrettyCode InstrReturn where
  ppCode InstrReturn {..} = do
    val <- ppCode _instrReturnValue
    return $ primitive Str.ret <+> val

instance PrettyCode InstrBranch where
  ppCode InstrBranch {..} = do
    val <- ppCode _instrBranchValue
    br1 <- ppCodeCode _instrBranchTrue
    br2 <- ppCodeCode _instrBranchFalse
    return $
      primitive Str.br
        <+> val
        <+> braces'
          ( constr Str.true_ <> colon
              <+> braces' br1
                <> line
                <> constr Str.false_
                <> colon
              <+> braces' br2
          )

instance PrettyCode CaseBranch where
  ppCode CaseBranch {..} = do
    tag <- Tree.ppConstrName _caseBranchTag
    body <- ppCodeCode _caseBranchCode
    return $ tag <> colon <+> braces' body

instance PrettyCode InstrCase where
  ppCode InstrCase {..} = do
    ind <- Tree.ppIndName _instrCaseInductive
    val <- ppCode _instrCaseValue
    brs <- mapM ppCode _instrCaseBranches
    def <- maybe (return Nothing) (fmap Just . ppCodeCode) _instrCaseDefault
    let brs' = brs ++ catMaybes [def]
    return $ primitive Str.case_ <> brackets ind <+> val <+> braces' (vsep brs')

instance PrettyCode InstrBlock where
  ppCode InstrBlock {..} = braces' <$> ppCodeCode _instrBlockCode

instance PrettyCode Instruction where
  ppCode = \case
    Nop -> return $ primitive Str.nop
    Binop x -> ppCode x
    Show x -> ppCode x
    StrToInt x -> ppCode x
    Assign x -> ppCode x
    Trace x -> ppCode x
    Dump -> return $ primitive Str.dump
    Failure x -> ppCode x
    ArgsNum x -> ppCode x
    Prealloc x -> ppCode x
    Alloc x -> ppCode x
    AllocClosure x -> ppCode x
    ExtendClosure x -> ppCode x
    Call x -> ppCode x
    CallClosures x -> ppCode x
    Return x -> ppCode x
    Branch x -> ppCode x
    Case x -> ppCode x
    Block x -> ppCode x

ppCodeCode :: (Member (Reader Options) r) => Code -> Sem r (Doc Ann)
ppCodeCode x = do
  cs <- mapM ppCode x
  return $ vcat $ map (<> semi) cs

instance PrettyCode FunctionInfo where
  ppCode = Tree.ppFunInfo ppCodeCode

instance PrettyCode ConstructorInfo where
  ppCode = Tree.ppCode

instance PrettyCode InfoTable where
  ppCode = Tree.ppInfoTable ppCodeCode