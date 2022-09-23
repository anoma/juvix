module Juvix.Compiler.Asm.Pretty.Base
  ( module Juvix.Compiler.Asm.Pretty.Base,
    module Juvix.Compiler.Asm.Pretty.Options,
  )
where

import Data.Foldable
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Interpreter.Base
import Juvix.Compiler.Asm.Interpreter.RuntimeState
import Juvix.Compiler.Asm.Language.Type
import Juvix.Compiler.Asm.Pretty.Options
import Juvix.Compiler.Core.Pretty.Base qualified as Core
import Juvix.Data.CodeAnn

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

wrapCore ::
  forall r' c.
  Member (Reader Options) r' =>
  (forall r. Member (Reader Core.Options) r => c -> Sem r (Doc Ann)) ->
  c ->
  Sem r' (Doc Ann)
wrapCore f c = do
  opts <- ask
  return $ run $ runReader (toCoreOptions opts) $ f c

instance PrettyCode BuiltinDataTag where
  ppCode = wrapCore Core.ppCode

instance PrettyCode Tag where
  ppCode = wrapCore Core.ppCode

instance PrettyCode Constr where
  ppCode (Constr tag args) = do
    opts <- ask
    let tab = opts ^. optInfoTable
    n' <-
      maybe
        (ppCode tag)
        (\ci -> return $ annotate (AnnKind KNameConstructor) (pretty (ci ^. constructorName)))
        (HashMap.lookup tag (tab ^. infoConstrs))
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

ppFunName :: Member (Reader Options) r => Symbol -> Sem r (Doc Ann)
ppFunName sym = do
  opts <- ask
  let tab = opts ^. optInfoTable
  maybe
    ( return $
        annotate (AnnKind KNameFunction) $
          pretty ("unnamed_function_" ++ show sym :: String)
    )
    (\fi -> return $ annotate (AnnKind KNameFunction) (pretty (fi ^. functionName)))
    (HashMap.lookup sym (tab ^. infoFunctions))

instance PrettyCode Closure where
  ppCode (Closure sym args) = do
    n' <- ppFunName sym
    args' <- mapM (ppRightExpression appFixity) args
    return $ foldl' (<+>) n' args'

instance PrettyCode Val where
  ppCode = \case
    ValInteger i ->
      return $ annotate AnnLiteralInteger (pretty i)
    ValBool True ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("true" :: String))
    ValBool False ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("false" :: String))
    ValString txt ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    ValUnit {} ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("unit" :: String))
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
  ppCode Frame {..} = do
    n <- maybe (return $ annotate (AnnKind KNameFunction) $ pretty ("main" :: String)) ppFunName _frameFunction
    let header =
          pretty ("function" :: String)
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
  ppCode RuntimeState {..} = do
    frm <- ppCode _runtimeFrame
    calls <- mapM (ppCode . (^. contFrame)) (_runtimeCallStack ^. callStack)
    return $ frm <> fold calls

instance PrettyCode TypeInductive where
  ppCode TypeInductive {..} = do
    opts <- ask
    let ii = getInductiveInfo (opts ^. optInfoTable) _typeInductiveSymbol
    return $ annotate (AnnKind KNameInductive) (pretty (ii ^. inductiveName))

instance PrettyCode TypeConstr where
  ppCode TypeConstr {..} = do
    opts <- ask
    let tab = opts ^. optInfoTable
    let ii = getInductiveInfo tab _typeConstrInductive
    let iname = annotate (AnnKind KNameInductive) (pretty (ii ^. inductiveName))
    let ci = getConstrInfo tab _typeConstrTag
    let cname = annotate (AnnKind KNameConstructor) (pretty (ci ^. constructorName))
    args <- mapM ppCode _typeConstrFields
    return $ iname <> kwColon <> cname <> encloseSep "(" ")" ", " args

instance PrettyCode TypeFun where
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
  ppCode = \case
    TyDynamic ->
      return $ annotate (AnnKind KNameInductive) (pretty ("*" :: String))
    TyInteger {} ->
      return $ annotate (AnnKind KNameInductive) (pretty ("integer" :: String))
    TyBool {} ->
      return $ annotate (AnnKind KNameInductive) (pretty ("bool" :: String))
    TyString ->
      return $ annotate (AnnKind KNameInductive) (pretty ("string" :: String))
    TyUnit ->
      return $ annotate (AnnKind KNameInductive) (pretty ("unit" :: String))
    TyInductive x ->
      ppCode x
    TyConstr x ->
      ppCode x
    TyFun x ->
      ppCode x

instance PrettyCode Instruction where
  -- TODO: properly handle the arguments
  ppCode = \case
    IntAdd -> return $ pretty ("add" :: String)
    IntSub -> return $ pretty ("sub" :: String)
    IntMul -> return $ pretty ("mul" :: String)
    IntDiv -> return $ pretty ("div" :: String)
    IntMod -> return $ pretty ("mod" :: String)
    IntLt -> return $ pretty ("lt" :: String)
    IntLe -> return $ pretty ("le" :: String)
    ValEq -> return $ pretty ("eq" :: String)
    Push {} -> return $ pretty ("push" :: String)
    Pop -> return $ pretty ("pop" :: String)
    PushTemp -> return $ pretty ("pusht" :: String)
    PopTemp -> return $ pretty ("popt" :: String)
    Trace -> return $ pretty ("trace" :: String)
    Dump -> return $ pretty ("dump" :: String)
    Failure -> return $ pretty ("fail" :: String)
    AllocConstr {} -> return $ pretty ("alloc" :: String)
    AllocClosure {} -> return $ pretty ("calloc" :: String)
    ExtendClosure {} -> return $ pretty ("cextend" :: String)
    Call {} -> return $ pretty ("call" :: String)
    TailCall {} -> return $ pretty ("tcall" :: String)
    CallClosures {} -> return $ pretty ("ccall" :: String)
    TailCallClosures {} -> return $ pretty ("tccall" :: String)
    Return -> return $ pretty ("ret" :: String)

instance PrettyCode a => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode x
    return $ encloseSep "[" "]" ", " cs

{--------------------------------------------------------------------------------}
{- helper functions -}

parensIf :: Bool -> Doc Ann -> Doc Ann
parensIf t = if t then parens else id

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
