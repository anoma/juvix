module Juvix.Compiler.Core.Pretty.Base
  ( module Juvix.Compiler.Core.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Core.Pretty.Options,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.BinderInfo as BinderInfo
import Juvix.Compiler.Core.Info.BranchInfo as BranchInfo
import Juvix.Compiler.Core.Info.NameInfo as NameInfo
import Juvix.Compiler.Core.Language
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
    OpTrace -> return kwTrace
    OpFail -> return kwFail

instance PrettyCode BuiltinDataTag where
  ppCode = \case
    TagTrue -> return $ annotate (AnnKind KNameConstructor) (pretty ("true" :: String))
    TagFalse -> return $ annotate (AnnKind KNameConstructor) (pretty ("false" :: String))
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
      case Info.lookup kNameInfo _varInfo of
        Just ni -> do
          showDeBruijn <- asks (^. optShowDeBruijnIndices)
          n <- ppCode (ni ^. NameInfo.infoName)
          if showDeBruijn
            then return $ n <> kwDeBruijnVar <> pretty _varIndex
            else return n
        Nothing -> return $ kwDeBruijnVar <> pretty _varIndex
    Ident {..} ->
      case Info.lookup kNameInfo _identInfo of
        Just ni -> ppCode (ni ^. NameInfo.infoName)
        Nothing -> return $ kwUnnamedIdent <> pretty _identSymbol
    Constant _ (ConstInteger int) ->
      return $ annotate AnnLiteralInteger (pretty int)
    Constant _ (ConstString txt) ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))
    App {..} -> do
      l' <- ppLeftExpression appFixity _appLeft
      r' <- ppRightExpression appFixity _appRight
      return $ l' <+> r'
    BuiltinApp {..} -> do
      args' <- mapM (ppRightExpression appFixity) _builtinArgs
      op' <- ppCode _builtinOp
      return $ foldl' (<+>) op' args'
    Constr {..} -> do
      args' <- mapM (ppRightExpression appFixity) _constrArgs
      n' <-
        case Info.lookup kNameInfo _constrInfo of
          Just ni -> ppCode (ni ^. NameInfo.infoName)
          Nothing -> ppCode _constrTag
      return $ foldl' (<+>) n' args'
    Lambda {} -> do
      let (infos, body) = unfoldLambdas' node
      pplams <- mapM ppLam infos
      b <- ppCode body
      return $ foldl' (flip (<+>)) b pplams
      where
        ppLam :: Member (Reader Options) r => Info -> Sem r (Doc Ann)
        ppLam i =
          case getInfoName (getInfoBinder i) of
            Just name -> do
              n <- ppCode name
              return $ kwLambda <> n
            Nothing -> return $ kwLambda <> kwQuestion
    Let {..} -> do
      n' <-
        case getInfoName (getInfoBinder _letInfo) of
          Just name -> ppCode name
          Nothing -> return kwQuestion
      v' <- ppCode _letValue
      b' <- ppCode _letBody
      return $ kwLet <+> n' <+> kwAssign <+> v' <+> kwIn <+> b'
    Case {..} -> do
      bns <-
        case Info.lookup kCaseBinderInfo _caseInfo of
          Just ci -> mapM (mapM (maybe (return kwQuestion) ppCode . getInfoName)) (ci ^. infoBranchBinders)
          Nothing -> mapM (\(CaseBranch _ n _) -> replicateM n (return kwQuestion)) _caseBranches
      cns <-
        case Info.lookup kCaseBranchInfo _caseInfo of
          Just ci -> mapM (ppCode . (^. BranchInfo.infoTagName)) (ci ^. infoBranches)
          Nothing -> mapM (\(CaseBranch tag _ _) -> ppCode tag) _caseBranches
      let bs = map (\(CaseBranch _ _ br) -> br) _caseBranches
      v <- ppCode _caseValue
      bs' <- sequence $ zipWith3Exact (\cn bn br -> ppCode br >>= \br' -> return $ foldl' (<+>) cn bn <+> kwMapsto <+> br') cns bns bs
      bs'' <-
        case _caseDefault of
          Just def -> do
            d' <- ppCode def
            return $ bs' ++ [kwDefault <+> kwMapsto <+> d']
          Nothing -> return bs'
      let bss = bracesIndent $ align $ concatWith (\a b -> a <> kwSemicolon <> line <> b) bs''
      return $ kwCase <+> v <+> kwOf <+> bss
    Pi {..} ->
      case getInfoName $ getInfoBinder _piInfo of
        Just name -> do
          n <- ppCode name
          b <- ppCode _piBody
          return $ kwLambda <> n <+> b
        Nothing -> do
          b <- ppCode _piBody
          return $ kwLambda <> kwQuestion <+> b
    Univ {..} ->
      return $ kwType <+> pretty _univLevel
    TypeConstr {..} -> do
      args' <- mapM (ppRightExpression appFixity) _typeConstrArgs
      n' <-
        case Info.lookup kNameInfo _typeConstrInfo of
          Just ni -> ppCode (ni ^. NameInfo.infoName)
          Nothing -> return $ kwUnnamedIdent <> pretty _typeConstrSymbol
      return $ foldl' (<+>) n' args'
    Closure {..} ->
      ppCode (substEnv _closureEnv (Lambda _closureInfo _closureBody))

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

kwPi :: Doc Ann
kwPi = keyword Str.pi_

kwTrace :: Doc Ann
kwTrace = keyword Str.trace_

kwFail :: Doc Ann
kwFail = keyword Str.fail_
