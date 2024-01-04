module Juvix.Compiler.Casm.Pretty.Base where

import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Casm.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Data.NameKind
import Juvix.Extra.Strings qualified as Str

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

ppOffset :: Offset -> Sem r (Doc Ann)
ppOffset off = return $ annotate AnnLiteralInteger $ pretty off

ppIncAp :: Bool -> Sem r (Doc Ann)
ppIncAp = \case
  True -> return $ Str.semicolon <+> Str.apPlusPlus
  False -> mempty

instance PrettyCode Reg where
  ppCode = \case
    Ap -> return Str.ap
    Fp -> return Str.fp

ppWithOffset :: Offset -> Doc Ann -> Sem r (Doc Ann)
ppWithOffset off r
  | off == 0 =
      return r
  | off > 0 = do
      off' <- ppOffset off
      return $ r <+> Str.plus <+> off'
  | otherwise = do
      off' <- ppOffset (-off)
      return $ r <+> Str.minus <+> off'

instance PrettyCode MemRef where
  ppCode MemRef {..} = do
    r <- ppCode _memRefReg
    r' <- ppWithOffset _memRefOff r
    return $ brackets r'

instance PrettyCode LabelRef where
  ppCode LabelRef {..} = case _labelRefName of
    Just n -> return $ annotate (AnnKind KNameFunction) $ pretty n
    Nothing -> return $ "label_" <> pretty (_labelRefSymbol ^. symbolId)

instance PrettyCode Value where
  ppCode = \case
    Imm i -> return $ annotate AnnLiteralInteger $ pretty i
    Ref r -> ppCode r
    Lab l -> ppCode l

instance PrettyCode InstrAssign where
  ppCode InstrAssign {..} = do
    v <- ppCode _instrAssignValue
    r <- ppCode _instrAssignResult
    incAp <- ppIncAp _instrAssignIncAp
    return $ r <+> Str.equal <+> v <> incAp

instance PrettyCode Opcode where
  ppCode = \case
    FieldAdd -> return Str.plus
    FieldSub -> return Str.minus
    FieldMul -> return Str.mul

instance PrettyCode InstrBinop where
  ppCode InstrBinop {..} = do
    v1 <- ppCode _instrBinopArg1
    v2 <- ppCode _instrBinopArg2
    r <- ppCode _instrBinopResult
    op <- ppCode _instrBinopOpcode
    incAp <- ppIncAp _instrBinopIncAp
    return $ r <+> Str.equal <+> v1 <+> op <+> v2 <> incAp

instance PrettyCode InstrLoad where
  ppCode InstrLoad {..} = do
    r <- ppCode _instrLoadResult
    src <- ppCode _instrLoadSrc
    src' <- ppWithOffset _instrLoadOff src
    incAp <- ppIncAp _instrLoadIncAp
    return $ r <+> Str.equal <+> brackets src' <> incAp

instance PrettyCode InstrJump where
  ppCode InstrJump {..} = do
    tgt <- ppCode _instrJumpTarget
    incAp <- ppIncAp _instrJumpIncAp
    return $ Str.jmp <+> tgt <> incAp

instance PrettyCode InstrJumpIf where
  ppCode InstrJumpIf {..} = do
    tgt <- ppCode _instrJumpIfTarget
    v <- ppCode _instrJumpIfValue
    incAp <- ppIncAp _instrJumpIfIncAp
    return $ Str.jmp <+> tgt <+> Str.if_ <+> v <+> Str.notequal <+> annotate AnnLiteralInteger "0" <> incAp

instance PrettyCode InstrCall where
  ppCode InstrCall {..} = do
    tgt <- ppCode _instrCallTarget
    return $ Str.call <+> tgt

instance PrettyCode InstrAlloc where
  ppCode InstrAlloc {..} = do
    let s = annotate AnnLiteralInteger $ pretty _instrAllocSize
    return $ Str.ap <+> Str.plusequal <+> s

instance PrettyCode Instruction where
  ppCode = \case
    Assign x -> ppCode x
    Binop x -> ppCode x
    Load x -> ppCode x
    Jump x -> ppCode x
    JumpIf x -> ppCode x
    Call x -> ppCode x
    Return -> return Str.ret
    Alloc x -> ppCode x
    Label x -> (<> colon) <$> ppCode x
