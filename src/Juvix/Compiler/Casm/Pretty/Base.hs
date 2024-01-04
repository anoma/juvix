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

ppRel :: Bool -> Sem r (Doc Ann)
ppRel = \case
  True -> return Str.rel
  False -> return Str.abs

instance PrettyCode Reg where
  ppCode = \case
    Ap -> return Str.ap
    Fp -> return Str.fp

instance PrettyCode MemRef where
  ppCode MemRef {..} = do
    r <- ppCode _memRefReg
    off <- ppOffset _memRefOff
    return $ brackets (r <+> Str.plus <+> off)

instance PrettyCode LabelRef where
  ppCode LabelRef {..} = case _labelRefName of
    Just n -> return $ annotate (AnnKind KNameFunction) $ pretty n
    Nothing -> return $ "label_" <> pretty (_labelRefSymbol ^. symbolId)

instance PrettyCode Value where
  ppCode = \case
    Imm i -> return $ annotate AnnLiteralInteger $ pretty i
    Ref r -> ppCode r
    Label l -> ppCode l

instance PrettyCode InstrAssign where
  ppCode InstrAssign {..} = do
    v <- ppCode _instrAssignValue
    r <- ppCode _instrAssignResult
    return $ r <+> Str.equal <+> v

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
    return $ r <+> Str.equal <+> v1 <+> op <+> v2

instance PrettyCode InstrLoad where
  ppCode InstrLoad {..} = do
    r <- ppCode _instrLoadResult
    src <- ppCode _instrLoadSrc
    off <- ppOffset _instrLoadOff
    return $ r <+> Str.equal <+> brackets (src <+> Str.plus <+> off)

instance PrettyCode InstrJump where
  ppCode InstrJump {..} = do
    tgt <- ppCode _instrJumpTarget
    return $ Str.jmp <+> tgt

instance PrettyCode InstrJumpIf where
  ppCode InstrJumpIf {..} = do
    tgt <- ppCode _instrJumpIfTarget
    v <- ppCode _instrJumpIfValue
    return $ Str.jmp <+> tgt <+> Str.if_ <+> v <+> Str.notequal <+> annotate AnnLiteralInteger "0"

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
