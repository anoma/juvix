module Juvix.Compiler.Core.Pretty.Base
  ( module Juvix.Compiler.Core.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Core.Pretty.Options,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Info qualified as Info
import Juvix.Compiler.Core.Language.Info.BinderInfo as BinderInfo
import Juvix.Compiler.Core.Language.Info.BranchInfo as BranchInfo
import Juvix.Compiler.Core.Language.Info.NameInfo as NameInfo
import Juvix.Compiler.Core.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

runPrettyCode :: PrettyCode c => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

instance PrettyCode NameId where
  ppCode (NameId k) = return (pretty k)

instance PrettyCode Name where
  ppCode n = do
    showNameId <- asks (^. optShowNameIds)
    return (prettyName showNameId n)

instance PrettyCode BuiltinOp where
  ppCode = \case
    OpIntAdd -> return kwPlus
    OpIntSub -> return kwMinus
    OpIntMul -> return kwMul
    OpIntDiv -> return kwDiv
    OpIntMod -> return kwMod
    OpIntLt -> return kwLess
    OpIntLe -> return kwLessEquals
    OpEq -> return kwEquals

instance PrettyCode BuiltinDataTag where
  ppCode = \case
    TagNil -> return $ annotate (AnnKind KNameConstructor) (pretty ("nil" :: String))
    TagCons -> return $ annotate (AnnKind KNameConstructor) (pretty ("cons" :: String))
    TagReturn -> return $ annotate (AnnKind KNameConstructor) (pretty ("return" :: String))
    TagBind -> return $ annotate (AnnKind KNameConstructor) (pretty ("bind" :: String))
    TagWrite -> return $ annotate (AnnKind KNameConstructor) (pretty ("write" :: String))
    TagReadLn -> return $ annotate (AnnKind KNameConstructor) (pretty ("readLn" :: String))

instance PrettyCode Tag where
  ppCode = \case
    BuiltinTag tag -> ppCode tag
    UserTag tag -> return $ kwUnnamedConstr <> pretty tag

instance PrettyCode Node where
  ppCode node = case node of
    Var {..} ->
      case Info.lookup kNameInfo varInfo of
        Just ni -> ppCode (ni ^. NameInfo.infoName)
        Nothing -> return $ kwDeBruijnVar <> pretty varIndex
    Ident {..} ->
      case Info.lookup kNameInfo identInfo of
        Just ni -> ppCode (ni ^. NameInfo.infoName)
        Nothing -> return $ kwUnnamedIdent <> pretty identSymbol
    Constant _ (ConstInteger int) ->
      return $ annotate AnnLiteralInteger (pretty int)
    Constant _ (ConstBool True) ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("true" :: String))
    Constant _ (ConstBool False) ->
      return $ annotate (AnnKind KNameConstructor) (pretty ("false" :: String))
    Constant _ (ConstString txt) ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    App {..} -> do
      l' <- ppLeftExpression appFixity appLeft
      r' <- ppRightExpression appFixity appRight
      return $ l' <+> r'
    BuiltinApp {..} -> do
      args' <- mapM (ppRightExpression appFixity) builtinArgs
      op' <- ppCode builtinOp
      return $ foldl (<+>) op' args'
    ConstrApp {..} -> do
      args' <- mapM (ppRightExpression appFixity) constrArgs
      n' <-
        case Info.lookup kNameInfo constrInfo of
          Just ni -> ppCode (ni ^. NameInfo.infoName)
          Nothing -> ppCode constrTag
      return $ foldl (<+>) n' args'
    Lambda {} -> do
      let (infos, body) = unfoldLambdas' node
      pplams <- mapM ppLam infos
      b <- ppCode body
      return $ foldl (flip (<+>)) b pplams
      where
        ppLam :: Member (Reader Options) r => Info -> Sem r (Doc Ann)
        ppLam i =
          case Info.lookup kBinderInfo i of
            Just bi -> do
              n <- ppCode (bi ^. BinderInfo.infoName)
              return $ kwLambda <> n
            Nothing -> return $ kwLambda <> kwQuestion
    Let {..} -> do
      n' <-
        case Info.lookup kBinderInfo letInfo of
          Just bi -> ppCode (bi ^. BinderInfo.infoName)
          Nothing -> return kwQuestion
      v' <- ppCode letValue
      b' <- ppCode letBody
      return $ kwLet <+> n' <+> kwAssign <+> v' <+> kwIn <+> b'
    Case {..} -> do
      bns <-
        case Info.lookup kCaseBinderInfo caseInfo of
          Just ci -> mapM (mapM (ppCode . (^. BinderInfo.infoName))) (ci ^. infoBranchBinders)
          Nothing -> mapM (\(CaseBranch _ n _) -> replicateM n (return kwQuestion)) caseBranches
      cns <-
        case Info.lookup kCaseBranchInfo caseInfo of
          Just ci -> mapM (ppCode . (^. BranchInfo.infoTagName)) (ci ^. infoBranches)
          Nothing -> mapM (\(CaseBranch tag _ _) -> ppCode tag) caseBranches
      let bs = map (\(CaseBranch _ _ br) -> br) caseBranches
      v <- ppCode caseValue
      bs' <- sequence $ zipWith3Exact (\cn bn br -> ppCode br >>= \br' -> return $ foldl (<+>) cn bn <+> kwMapsto <+> br') cns bns bs
      bs'' <-
        case caseDefault of
          Just def -> do
            d' <- ppCode def
            return $ bs' ++ [kwDefault <+> kwMapsto <+> d']
          Nothing -> return bs'
      let bss = bracesIndent $ align $ concatWith (\a b -> a <> kwSemicolon <> line <> b) bs''
      return $ kwCase <+> v <+> kwOf <+> bss
    If {..} -> do
      v <- ppCode ifValue
      b1 <- ppCode ifTrueBranch
      b2 <- ppCode ifFalseBranch
      return $ kwIf <+> v <+> kwThen <+> b1 <+> kwElse <+> b2
    Data {..} -> ppCode (ConstrApp dataInfo dataTag dataArgs)
    Closure {..} -> ppCode (substEnv closureEnv closureBody)

instance PrettyCode a => PrettyCode (NonEmpty a) where
  ppCode x = do
    cs <- mapM ppCode (toList x)
    return $ encloseSep "(" ")" ", " cs

{--------------------------------------------------------------------------------}
{- helper functions -}

parensIf :: Bool -> Doc Ann -> Doc Ann
parensIf t = if t then parens else id

ppPostExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppPostExpression = ppLRExpression isPostfixAssoc

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

{--------------------------------------------------------------------------------}
{- keywords -}

kwDeBruijnVar :: Doc Ann
kwDeBruijnVar = keyword Str.deBruijnVar

kwUnnamedIdent :: Doc Ann
kwUnnamedIdent = keyword Str.exclamation

kwUnnamedConstr :: Doc Ann
kwUnnamedConstr = keyword Str.exclamation

kwQuestion :: Doc Ann
kwQuestion = keyword Str.questionMark

kwLess :: Doc Ann
kwLess = keyword Str.less

kwLessEquals :: Doc Ann
kwLessEquals = keyword Str.lessEqual

kwPlus :: Doc Ann
kwPlus = keyword Str.plus

kwMinus :: Doc Ann
kwMinus = keyword Str.minus

kwMul :: Doc Ann
kwMul = keyword Str.mul

kwDiv :: Doc Ann
kwDiv = keyword Str.div

kwMod :: Doc Ann
kwMod = keyword Str.mod

kwCase :: Doc Ann
kwCase = keyword Str.case_

kwOf :: Doc Ann
kwOf = keyword Str.of_

kwDefault :: Doc Ann
kwDefault = keyword Str.underscore

kwIf :: Doc Ann
kwIf = keyword Str.if_

kwThen :: Doc Ann
kwThen = keyword Str.then_

kwElse :: Doc Ann
kwElse = keyword Str.else_
