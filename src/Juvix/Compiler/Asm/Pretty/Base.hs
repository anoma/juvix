module Juvix.Compiler.Asm.Pretty.Base
  ( module Juvix.Compiler.Asm.Pretty.Base,
    module Juvix.Compiler.Asm.Pretty.Options,
  )
where

import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Interpreter.Base
import Juvix.Compiler.Asm.Interpreter.RuntimeState
import Juvix.Compiler.Asm.Pretty.Options
import Juvix.Compiler.Core.Pretty.Base qualified as Core
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

wrapCore ::
  forall r' c.
  (Member (Reader Options) r') =>
  (forall r. (Member (Reader Core.Options) r) => c -> Sem r (Doc Ann)) ->
  c ->
  Sem r' (Doc Ann)
wrapCore f c = do
  opts <- ask
  return $ run $ runReader (toCoreOptions opts) $ f c

quoteAsmName :: Text -> Text
quoteAsmName txt =
  foldr
    (uncurry Text.replace)
    txt
    [ ("$", "__dollar__"),
      (":", "__colon__")
    ]

quoteAsmFunName :: Text -> Text
quoteAsmFunName txt =
  foldr
    (uncurry Text.replace)
    txt
    [ ("readLn", "__readLn__")
    ]

ppConstrName :: (Member (Reader Options) r) => Tag -> Sem r (Doc Ann)
ppConstrName tag = do
  opts <- ask
  let tab = opts ^. optInfoTable
  maybe
    (wrapCore Core.ppCode tag)
    (\ci -> return $ annotate (AnnKind KNameConstructor) (pretty (quoteAsmName (ci ^. constructorName))))
    (HashMap.lookup tag (tab ^. infoConstrs))

ppIndName :: (Member (Reader Options) r) => Symbol -> Sem r (Doc Ann)
ppIndName sym = do
  opts <- ask
  let ci = fromMaybe impossible $ HashMap.lookup sym (opts ^. optInfoTable . infoInductives)
  return $ annotate (AnnKind KNameInductive) (pretty (quoteAsmName (ci ^. inductiveName)))

ppFunName :: (Member (Reader Options) r) => Symbol -> Sem r (Doc Ann)
ppFunName sym = do
  opts <- ask
  let tab = opts ^. optInfoTable
  maybe
    ( return $
        annotate (AnnKind KNameFunction) $
          pretty ("unnamed_function_" ++ show sym :: String)
    )
    (\fi -> return $ annotate (AnnKind KNameFunction) (pretty (quoteAsmFunName $ quoteAsmName (fi ^. functionName))))
    (HashMap.lookup sym (tab ^. infoFunctions))

instance PrettyCode BuiltinDataTag where
  ppCode = wrapCore Core.ppCode

instance PrettyCode Tag where
  ppCode = ppConstrName

instance PrettyCode Constr where
  ppCode (Constr tag args) = do
    n' <- ppConstrName tag
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

instance PrettyCode Closure where
  ppCode (Closure sym args) = do
    n' <- ppFunName sym
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

instance PrettyCode Val where
  ppCode = \case
    ValInteger i ->
      return $ integer i
    ValBool True ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.true_ :: String))
    ValBool False ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.false_ :: String))
    ValString txt ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    ValUnit ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.unit :: String))
    ValVoid ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.void :: String))
    ValConstr c ->
      ppCode c
    ValClosure cl ->
      ppCode cl

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
    n <- maybe (return $ annotate (AnnKind KNameFunction) $ pretty (Str.main :: String)) ppFunName _frameFunction
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

instance PrettyCode TypeInductive where
  ppCode :: (Member (Reader Options) r) => TypeInductive -> Sem r (Doc Ann)
  ppCode TypeInductive {..} = do
    opts <- ask
    let ii = lookupInductiveInfo (opts ^. optInfoTable) _typeInductiveSymbol
    return $ annotate (AnnKind KNameInductive) (pretty (ii ^. inductiveName))

instance PrettyCode TypeConstr where
  ppCode :: (Member (Reader Options) r) => TypeConstr -> Sem r (Doc Ann)
  ppCode TypeConstr {..} = do
    opts <- ask
    let tab = opts ^. optInfoTable
    let ii = lookupInductiveInfo tab _typeConstrInductive
    let iname = annotate (AnnKind KNameInductive) (pretty (ii ^. inductiveName))
    let ci = lookupConstrInfo tab _typeConstrTag
    let cname = annotate (AnnKind KNameConstructor) (pretty (ci ^. constructorName))
    args <- mapM ppCode _typeConstrFields
    return $ iname <> kwColon <> cname <> encloseSep "(" ")" ", " args

instance PrettyCode TypeFun where
  ppCode :: (Member (Reader Options) r) => TypeFun -> Sem r (Doc Ann)
  ppCode TypeFun {..} = do
    l <-
      if
          | null (NonEmpty.tail _typeFunArgs) ->
              ppLeftExpression funFixity (head _typeFunArgs)
          | otherwise -> do
              args <- mapM ppCode _typeFunArgs
              return $ encloseSep "(" ")" ", " (toList args)
    r <- ppRightExpression funFixity _typeFunTarget
    return $ l <+> kwArrow <+> r

instance PrettyCode Type where
  ppCode :: (Member (Reader Options) r) => Type -> Sem r (Doc Ann)
  ppCode = \case
    TyDynamic ->
      return $ annotate (AnnKind KNameInductive) (pretty (Str.mul :: String))
    TyInteger {} ->
      return $ annotate (AnnKind KNameInductive) (pretty (Str.integer :: String))
    TyBool {} ->
      return $ annotate (AnnKind KNameInductive) (pretty (Str.bool :: String))
    TyString ->
      return $ annotate (AnnKind KNameInductive) (pretty (Str.string :: String))
    TyUnit ->
      return $ annotate (AnnKind KNameInductive) (pretty (Str.unit :: String))
    TyVoid ->
      return $ annotate (AnnKind KNameInductive) (pretty (Str.void :: String))
    TyInductive x ->
      ppCode x
    TyConstr x ->
      ppCode x
    TyFun x ->
      ppCode x

instance PrettyCode DirectRef where
  ppCode :: DirectRef -> Sem r (Doc Ann)
  ppCode = \case
    StackRef -> return $ variable Str.dollar
    ArgRef off -> return $ variable Str.arg <> lbracket <> integer off <> rbracket
    TempRef off -> return $ variable Str.tmp <> lbracket <> integer off <> rbracket

instance PrettyCode Field where
  ppCode :: (Member (Reader Options) r) => Field -> Sem r (Doc Ann)
  ppCode Field {..} = do
    dr <- ppCode _fieldRef
    ctr <- ppConstrName _fieldTag
    return $ dr <> dot <> ctr <> lbracket <> integer _fieldOffset <> rbracket

instance PrettyCode MemValue where
  ppCode :: (Member (Reader Options) r) => MemValue -> Sem r (Doc Ann)
  ppCode = \case
    DRef dr -> ppCode dr
    ConstrRef fld -> ppCode fld

instance PrettyCode Value where
  ppCode :: (Member (Reader Options) r) => Value -> Sem r (Doc Ann)
  ppCode = \case
    ConstInt v ->
      return $ annotate AnnLiteralInteger (pretty v)
    ConstBool True ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.true_ :: String))
    ConstBool False ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.false_ :: String))
    ConstString txt ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    ConstUnit {} ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.unit :: String))
    ConstVoid {} ->
      return $ annotate (AnnKind KNameConstructor) (pretty (Str.void :: String))
    Ref mval ->
      ppCode mval

ppCall :: (Member (Reader Options) r) => Doc Ann -> InstrCall -> Sem r (Doc Ann)
ppCall call InstrCall {..} = case _callType of
  CallFun sym -> do
    fn <- ppFunName sym
    return $ call <+> fn
  CallClosure ->
    return $ call <+> variable Str.dollar <+> integer _callArgsNum

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
    PushTemp -> return $ primitive Str.instrPusht
    PopTemp -> return $ primitive Str.instrPopt
    Trace -> return $ primitive Str.instrTrace
    Dump -> return $ primitive Str.instrDump
    Failure -> return $ primitive Str.instrFailure
    Prealloc InstrPrealloc {..} ->
      return $ primitive Str.instrPrealloc <+> integer _preallocWordsNum
    AllocConstr tag -> (primitive Str.instrAlloc <+>) <$> ppConstrName tag
    AllocClosure InstrAllocClosure {..} -> do
      fn <- ppFunName _allocClosureFunSymbol
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
    name <- ppConstrName _caseBranchTag
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
      name <- ppIndName _cmdCaseInductive
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

instance (PrettyCode a) => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode x
    return $ encloseSep "[" "]" ", " cs

instance PrettyCode FunctionInfo where
  ppCode FunctionInfo {..} = do
    argtys <- mapM ppCode (typeArgs _functionType)
    targetty <- ppCode (typeTarget _functionType)
    c <- ppCodeCode _functionCode
    return $
      keyword Str.function
        <+> annotate (AnnKind KNameFunction) (pretty (quoteAsmFunName $ quoteAsmName _functionName))
          <> encloseSep lparen rparen ", " argtys
        <+> colon
        <+> targetty
        <+> braces' c

ppFunSig :: (Member (Reader Options) r) => FunctionInfo -> Sem r (Doc Ann)
ppFunSig FunctionInfo {..} = do
  argtys <- mapM ppCode (typeArgs _functionType)
  targetty <- ppCode (typeTarget _functionType)
  return $
    keyword Str.function
      <+> annotate (AnnKind KNameFunction) (pretty (quoteAsmFunName $ quoteAsmName _functionName))
        <> encloseSep lparen rparen ", " argtys
      <+> colon
      <+> targetty
        <> semi

instance PrettyCode ConstructorInfo where
  ppCode ConstructorInfo {..} = do
    ty <- ppCode _constructorType
    return $ annotate (AnnKind KNameConstructor) (pretty (quoteAsmName _constructorName)) <+> colon <+> ty

ppInductive :: Member (Reader Options) r => InfoTable -> InductiveInfo -> Sem r (Doc Ann)
ppInductive tab InductiveInfo {..} = do
  ctrs <- mapM (ppCode . lookupConstrInfo tab) _inductiveConstructors
  return $ kwInductive <+> annotate (AnnKind KNameInductive) (pretty (quoteAsmName _inductiveName)) <+> braces' (vcat (map (<> semi) ctrs))

instance PrettyCode InfoTable where
  ppCode tab@InfoTable {..} = do
    inds <- mapM (ppInductive tab) (HashMap.elems (filterOutBuiltins _infoInductives))
    funsigs <- mapM ppFunSig (HashMap.elems _infoFunctions)
    funs <- mapM ppCode (HashMap.elems _infoFunctions)
    return $ vcat (map (<> line) inds) <> line <> vcat funsigs <> line <> line <> vcat (map (<> line) funs)
    where
      filterOutBuiltins :: HashMap Symbol InductiveInfo -> HashMap Symbol InductiveInfo
      filterOutBuiltins =
        HashMap.filter
          ( \ii -> case ii ^. inductiveConstructors of
              BuiltinTag _ : _ -> False
              UserTag _ : _ -> True
              [] -> True
          )

{--------------------------------------------------------------------------------}
{- helper functions -}

braces' :: Doc Ann -> Doc Ann
braces' d = braces (line <> indent' d <> line)

integer :: (Pretty a) => a -> Doc Ann
integer i = annotate AnnLiteralInteger (pretty i)

constr :: Text -> Doc Ann
constr a = annotate (AnnKind KNameConstructor) (pretty a)

variable :: Text -> Doc Ann
variable a = annotate (AnnKind KNameLocal) (pretty a)

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
