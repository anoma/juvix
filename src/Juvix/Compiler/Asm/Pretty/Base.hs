module Juvix.Compiler.Asm.Pretty.Base
  ( module Juvix.Compiler.Asm.Pretty.Base,
    module Juvix.Compiler.Asm.Pretty.Options,
  )
where

import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Interpreter.RuntimeState
import Juvix.Compiler.Asm.Pretty.Options
import Juvix.Compiler.Internal.Data.Name
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

instance PrettyCode Constr where
  ppCode = Tree.ppCode

instance PrettyCode Closure where
  ppCode = Tree.ppCode

instance PrettyCode Val where
  ppCode = Tree.ppCode

instance PrettyCode ArgumentArea where
  ppCode ArgumentArea {..} =
    ppCode $ map snd $ sortBy (\x y -> compare (fst x) (fst y)) $ HashMap.toList _argumentArea

instance PrettyCode TemporaryStack where
  ppCode TemporaryStack {..} =
    ppCode $ reverse $ toList _temporaryStack

instance PrettyCode ValueStack where
  ppCode ValueStack {..} =
    ppCode $ reverse _valueStack

instance PrettyCode Frame where
  ppCode :: (Member (Reader Options) r) => Frame -> Sem r (Doc Ann)
  ppCode Frame {..} = do
    n <- maybe (return $ annotate (AnnKind KNameFunction) $ pretty (Str.main :: String)) Tree.ppFunName _frameFunction
    let header =
          pretty (Str.function :: String)
            <+> n
              <> maybe mempty (\loc -> pretty (" called at" :: String) <+> pretty loc) _frameCallLocation
    args <- ppCode _frameArgs
    temp <- ppCode _frameTemp
    stack <- ppCode _frameStack
    return $
      header
        <> line
        <> indent' (pretty ("arguments = " :: String) <> args)
        <> line
        <> indent' (pretty ("temporaries = " :: String) <> temp)
        <> line
        <> indent' (pretty ("value stack = " :: String) <> stack)
        <> line

instance PrettyCode RuntimeState where
  ppCode :: (Member (Reader Options) r) => RuntimeState -> Sem r (Doc Ann)
  ppCode RuntimeState {..} = do
    frm <- ppCode _runtimeFrame
    calls <- mapM (ppCode . (^. contFrame)) (_runtimeCallStack ^. callStack)
    return $ frm <> fold calls

instance PrettyCode Value where
  ppCode :: (Member (Reader Options) r) => Value -> Sem r (Doc Ann)
  ppCode = \case
    Constant c -> Tree.ppCode c
    Ref mval -> Tree.ppCode mval

ppCall :: (Member (Reader Options) r) => Doc Ann -> InstrCall -> Sem r (Doc Ann)
ppCall call InstrCall {..} = case _callType of
  CallFun sym -> do
    fn <- Tree.ppFunName sym
    return $ call <+> fn
  CallClosure ->
    return $ call <+> variable Str.dollar <+> integer _callArgsNum

instance PrettyCode Type where
  ppCode = Tree.ppCode

instance PrettyCode Instruction where
  ppCode :: (Member (Reader Options) r) => Instruction -> Sem r (Doc Ann)
  ppCode = \case
    Binop IntAdd -> return $ primitive Str.instrAdd
    Binop IntSub -> return $ primitive Str.instrSub
    Binop IntMul -> return $ primitive Str.instrMul
    Binop IntDiv -> return $ primitive Str.instrDiv
    Binop IntMod -> return $ primitive Str.instrMod
    Binop IntLt -> return $ primitive Str.instrLt
    Binop IntLe -> return $ primitive Str.instrLe
    Binop ValEq -> return $ primitive Str.instrEq
    Binop StrConcat -> return $ primitive Str.instrStrConcat
    ValShow -> return $ primitive Str.instrShow
    StrToInt -> return $ primitive Str.instrStrToInt
    Push val -> (primitive Str.instrPush <+>) <$> ppCode val
    Pop -> return $ primitive Str.instrPop
    Trace -> return $ primitive Str.instrTrace
    Dump -> return $ primitive Str.instrDump
    Failure -> return $ primitive Str.instrFailure
    ArgsNum -> return $ primitive Str.instrArgsNum
    Prealloc InstrPrealloc {..} ->
      return $ primitive Str.instrPrealloc <+> integer _preallocWordsNum
    AllocConstr tag -> (primitive Str.instrAlloc <+>) <$> Tree.ppConstrName tag
    AllocClosure InstrAllocClosure {..} -> do
      fn <- Tree.ppFunName _allocClosureFunSymbol
      return $ primitive Str.instrCalloc <+> fn <+> integer _allocClosureArgsNum
    ExtendClosure InstrExtendClosure {..} ->
      return $ primitive Str.instrCextend <+> integer _extendClosureArgsNum
    Call c -> ppCall (primitive Str.instrCall) c
    TailCall c -> ppCall (primitive Str.instrTcall) c
    CallClosures InstrCallClosures {..} ->
      return $ primitive Str.instrCcall <+> integer _callClosuresArgsNum
    TailCallClosures InstrCallClosures {..} ->
      return $ primitive Str.instrTccall <+> integer _callClosuresArgsNum
    Return -> return $ primitive Str.instrReturn

ppCodeCode :: (Member (Reader Options) r) => Code -> Sem r (Doc Ann)
ppCodeCode x = do
  cs <- mapM ppCode x
  return $ vcat $ map (<> semi) cs

instance PrettyCode CaseBranch where
  ppCode :: (Member (Reader Options) r) => CaseBranch -> Sem r (Doc Ann)
  ppCode CaseBranch {..} = do
    name <- Tree.ppConstrName _caseBranchTag
    br <- ppCodeCode _caseBranchCode
    return $ name <> colon <+> braces' br

instance PrettyCode Command where
  ppCode :: (Member (Reader Options) r) => Command -> Sem r (Doc Ann)
  ppCode = \case
    Instr CmdInstr {..} -> ppCode _cmdInstrInstruction
    Branch CmdBranch {..} -> do
      br1 <- ppCodeCode _cmdBranchTrue
      br2 <- ppCodeCode _cmdBranchFalse
      return $
        primitive Str.instrBr
          <+> braces'
            ( constr Str.true_ <> colon
                <+> braces' br1
                  <> line
                  <> constr Str.false_
                  <> colon
                <+> braces' br2
            )
    Case CmdCase {..} -> do
      name <- Tree.ppIndName _cmdCaseInductive
      brs <- mapM ppCode _cmdCaseBranches
      brs' <- case _cmdCaseDefault of
        Just def -> do
          d <-
            ( ppCodeCode
                >=> (\x -> return $ primitive Str.default_ <> colon <+> braces' x)
              )
              def
          return $ brs ++ [d]
        Nothing -> return brs
      return $ primitive Str.case_ <+> name <+> braces' (vsep brs')
    Save CmdSave {..} -> do
      c <- ppCodeCode _cmdSaveCode
      let s = if _cmdSaveIsTail then Str.tsave else Str.save
      return $ primitive s <+> (maybe mempty ((<> space) . variable) _cmdSaveName) <> braces' c

instance (PrettyCode a) => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode x
    return $ encloseSep "[" "]" ", " cs

instance PrettyCode FunctionInfo where
  ppCode = Tree.ppFunInfo ppCodeCode

instance PrettyCode ConstructorInfo where
  ppCode = Tree.ppCode

instance PrettyCode InfoTable where
  ppCode = Tree.ppInfoTable ppCodeCode

{--------------------------------------------------------------------------------}
{- helper functions -}

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensIf (atomParens associates (atomicity e) fixlr)
    <$> ppCode e
