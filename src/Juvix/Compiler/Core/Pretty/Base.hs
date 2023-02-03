module Juvix.Compiler.Core.Pretty.Base
  ( module Juvix.Compiler.Core.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Core.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Core.Data.BinderList as BL
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Stripped
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Stripped qualified as Stripped
import Juvix.Compiler.Core.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

instance PrettyCode BuiltinOp where
  ppCode = \case
    OpIntAdd -> return primPlus
    OpIntSub -> return primMinus
    OpIntMul -> return primMul
    OpIntDiv -> return primDiv
    OpIntMod -> return primMod
    OpIntLt -> return primLess
    OpIntLe -> return primLessEquals
    OpEq -> return primEquals
    OpShow -> return primShow
    OpStrConcat -> return primStrConcat
    OpStrToInt -> return primStrToInt
    OpTrace -> return primTrace
    OpFail -> return primFail

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

instance PrettyCode Primitive where
  ppCode = \case
    PrimInteger _ -> return $ annotate (AnnKind KNameInductive) (pretty ("int" :: String))
    PrimBool _ -> return $ annotate (AnnKind KNameInductive) (pretty ("bool" :: String))
    PrimString -> return $ annotate (AnnKind KNameInductive) (pretty ("string" :: String))

ppName :: NameKind -> Text -> Sem r (Doc Ann)
ppName kind name = return $ annotate (AnnKind kind) (pretty name)

ppCodeVar' :: (Member (Reader Options) r) => Text -> Var' i -> Sem r (Doc Ann)
ppCodeVar' name v = do
  let name' = annotate (AnnKind KNameLocal) (pretty name)
  showDeBruijn <- asks (^. optShowDeBruijnIndices)
  if showDeBruijn || name == ""
    then return $ name' <> kwDeBruijnVar <> pretty (v ^. varIndex)
    else return name'

instance PrettyCode (Constant' i) where
  ppCode = \case
    Constant _ (ConstInteger int) ->
      return $ annotate AnnLiteralInteger (pretty int)
    Constant _ (ConstString txt) ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))

instance (PrettyCode a, HasAtomicity a) => PrettyCode (App' i a) where
  ppCode App {..} = do
    l' <- ppLeftExpression appFixity _appLeft
    r' <- ppRightExpression appFixity _appRight
    return $ l' <+> r'

instance PrettyCode Stripped.Fun where
  ppCode = \case
    Stripped.FunVar x -> ppCodeVar' (x ^. (varInfo . Stripped.varInfoName)) x
    Stripped.FunIdent x -> ppName KNameLocal (x ^. (identInfo . Stripped.identInfoName))

instance (PrettyCode f, PrettyCode a, HasAtomicity a) => PrettyCode (Apps' i f a) where
  ppCode Apps {..} = do
    args' <- mapM (ppRightExpression appFixity) _appsArgs
    n' <- ppCode _appsFun
    return $ foldl' (<+>) n' args'

instance (PrettyCode a, HasAtomicity a) => PrettyCode (BuiltinApp' i a) where
  ppCode BuiltinApp {..} = do
    args' <- mapM (ppRightExpression appFixity) _builtinAppArgs
    op' <- ppCode _builtinAppOp
    return $ foldl' (<+>) op' args'

ppCodeConstr' :: (PrettyCode a, HasAtomicity a, Member (Reader Options) r) => Text -> Constr' i a -> Sem r (Doc Ann)
ppCodeConstr' name c = do
  args' <- mapM (ppRightExpression appFixity) (c ^. constrArgs)
  n' <- case c ^. constrTag of
    BuiltinTag tag -> ppCode tag
    _ -> ppName KNameConstructor name
  return $ foldl' (<+>) n' args'

instance (Pretty k, PrettyCode a) => PrettyCode (Map k a) where
  ppCode m = do
    m' <-
      sep . punctuate ","
        <$> sequence
          [ do
              a' <- ppCode a
              let k' = pretty k
              return $ k' <+> kwMapsto <+> a'
            | (k, a) <- Map.toList m
          ]
    return $ braces m'

instance (PrettyCode a) => PrettyCode (BinderList a) where
  ppCode bl = do
    m <-
      sequence
        [ do
            v' <- ppCode v
            return (pretty k <+> kwMapsto <+> v')
          | (k, v) <- BL.toIndexedList bl
        ]
    return $ brackets (hsep $ punctuate "," m)

instance (PrettyCode a) => PrettyCode (Binder' a) where
  ppCode (Binder mname _ ty) = do
    let name' = case mname of
          "" -> "_"
          _ -> mname
    ty' <- ppCode ty
    return (parens (pretty name' <+> kwColon <+> ty'))

ppCodeLet' :: (PrettyCode a, Member (Reader Options) r) => Text -> Maybe (Doc Ann) -> Let' i a ty -> Sem r (Doc Ann)
ppCodeLet' name mty lt = do
  n' <- ppName KNameConstructor name
  v' <- ppCode (lt ^. letItem . letItemValue)
  b' <- ppCode (lt ^. letBody)
  let tty = case mty of
        Just ty ->
          mempty <+> kwColon <+> ty
        Nothing ->
          mempty
  return $ kwLet <+> n' <> tty <+> kwAssign <+> v' <+> kwIn <+> b'

ppCodeCase' :: (PrettyCode a, Member (Reader Options) r) => [[Text]] -> [Text] -> Case' i bi a ty -> Sem r (Doc Ann)
ppCodeCase' branchBinderNames branchTagNames Case {..} =
  case _caseBranches of
    [CaseBranch _ (BuiltinTag TagTrue) _ _ br1, CaseBranch _ (BuiltinTag TagTrue) _ _ br2] -> do
      br1' <- ppCode br1
      br2' <- ppCode br2
      v <- ppCode _caseValue
      return $ kwIf <+> v <+> kwThen <+> br1' <+> kwElse <+> br2'
    [CaseBranch _ (BuiltinTag TagTrue) _ _ br1] | isJust _caseDefault -> do
      br1' <- ppCode br1
      br2' <- ppCode (fromJust _caseDefault)
      v <- ppCode _caseValue
      return $ kwIf <+> v <+> kwThen <+> br1' <+> kwElse <+> br2'
    _ -> do
      let branchBodies = map (^. caseBranchBody) _caseBranches
      bns <- mapM (mapM (ppName KNameLocal)) branchBinderNames
      cns <- mapM (ppName KNameConstructor) branchTagNames
      v <- ppCode _caseValue
      bs' <- sequence $ zipWith3Exact (\cn bn br -> ppCode br >>= \br' -> return $ foldl' (<+>) cn bn <+> kwAssign <+> br') cns bns branchBodies
      bs'' <-
        case _caseDefault of
          Just def -> do
            d' <- ppCode def
            return $ bs' ++ [kwDefault <+> kwAssign <+> d']
          Nothing -> return bs'
      let bss = bracesIndent $ align $ concatWith (\a b -> a <> kwSemicolon <> line <> b) bs''
      return $ kwCase <+> v <+> kwOf <+> bss

instance PrettyCode PatternWildcard where
  ppCode _ = return kwWildcard

instance PrettyCode PatternBinder where
  ppCode PatternBinder {..} = do
    n <- ppName KNameLocal (_patternBinder ^. binderName)
    case _patternBinderPattern of
      PatWildcard {} -> return n
      _ -> do
        pat <- ppRightExpression appFixity _patternBinderPattern
        return $ n <> kwAt <> pat

instance PrettyCode PatternConstr where
  ppCode PatternConstr {..} = do
    n <- ppName KNameConstructor (getInfoName _patternConstrInfo)
    args <- mapM (ppRightExpression appFixity) _patternConstrArgs
    return $ foldl' (<+>) n args

instance PrettyCode Pattern where
  ppCode = \case
    PatWildcard x -> ppCode x
    PatBinder x -> ppCode x
    PatConstr x -> ppCode x

ppPatterns :: (Member (Reader Options) r) => NonEmpty Pattern -> Sem r (Doc Ann)
ppPatterns pats = do
  ps' <- mapM ppCode pats
  return $ hsep (punctuate comma (toList ps'))

instance PrettyCode Let where
  ppCode :: forall r. (Member (Reader Options) r) => Let -> Sem r (Doc Ann)
  ppCode x = do
    let binder = x ^. letItem . letItemBinder
        name = binder ^. binderName
        ty = binder ^. binderType
     in do
          mty <- case ty of
            NDyn {} -> return Nothing
            _ -> Just <$> ppCode ty
          ppCodeLet' name mty x

instance PrettyCode LetRec where
  ppCode :: forall r. (Member (Reader Options) r) => LetRec -> Sem r (Doc Ann)
  ppCode LetRec {..} = do
    names <- mapM (getName . (^. letItemBinder)) _letRecValues
    vs <- mapM (ppCode . (^. letItemValue)) _letRecValues
    b' <- ppCode _letRecBody
    return $ case names of
      hns :| [] -> kwLetRec <+> hns <+> kwAssign <+> head vs <+> kwIn <+> b'
      _ ->
        let bss =
              indent' $
                align $
                  concatWith (\a b -> a <> kwSemicolon <> line <> b) $
                    zipWithExact (\name val -> name <+> kwAssign <+> val) (toList names) (toList vs)
            nss = enclose kwSquareL kwSquareR (concatWith (<+>) names)
         in kwLetRec <> nss <> line <> bss <> line <> kwIn <> line <> b'
    where
      getName :: Binder -> Sem r (Doc Ann)
      getName i = ppName KNameLocal (i ^. binderName)

instance PrettyCode Node where
  ppCode :: forall r. (Member (Reader Options) r) => Node -> Sem r (Doc Ann)
  ppCode node = case node of
    NVar x ->
      let name = getInfoName (x ^. varInfo)
       in ppCodeVar' name x
    NIdt x ->
      let name = getInfoName (x ^. identInfo)
       in ppName KNameLocal name
    NCst x -> ppCode x
    NApp x -> ppCode x
    NBlt x -> ppCode x
    NCtr x ->
      let name = getInfoName (x ^. constrInfo)
       in ppCodeConstr' name x
    NLam (Lambda _ bi body) -> do
      b <- ppCode body
      lam <- do
        n <- ppName KNameLocal (bi ^. binderName)
        case bi ^. binderType of
          NDyn {} -> return $ kwLambda <> n
          ty -> do
            tty <- ppCode ty
            return $ kwLambda <> parens (n <+> kwColon <+> tty)
      return (lam <+> b)
    NLet x -> ppCode x
    NRec l -> ppCode l
    NCase x@Case {..} ->
      let branchBinderNames = map (\CaseBranch {..} -> map (^. binderName) _caseBranchBinders) _caseBranches
          branchTagNames = map (\CaseBranch {..} -> getInfoName _caseBranchInfo) _caseBranches
       in ppCodeCase' branchBinderNames branchTagNames x
    NMatch Match {..} -> do
      let branchPatterns = map (^. matchBranchPatterns) _matchBranches
          branchBodies = map (^. matchBranchBody) _matchBranches
      pats <- mapM ppPatterns branchPatterns
      vs <- mapM ppCode _matchValues
      bs <- sequence $ zipWithExact (\ps br -> ppCode br >>= \br' -> return $ ps <+> kwMapsto <+> br') pats branchBodies
      let bss = bracesIndent $ align $ concatWith (\a b -> a <> kwSemicolon <> line <> b) bs
      return $ kwMatch <+> hsep (punctuate comma (toList vs)) <+> kwWith <+> bss
    NPi Pi {..} ->
      let piType = _piBinder ^. binderType
       in case _piBinder ^. binderName of
            "?" -> do
              ty <- ppLeftExpression funFixity piType
              b <- ppRightExpression funFixity _piBody
              return $ ty <+> kwArrow <+> b
            name -> do
              n <- ppName KNameLocal name
              ty <- ppCode piType
              b <- ppCode _piBody
              return $ kwPi <+> n <+> kwColon <+> ty <> comma <+> b
    NUniv Univ {..} ->
      return $ kwType <+> pretty _univLevel
    NPrim TypePrim {..} -> ppCode _typePrimPrimitive
    NTyp TypeConstr {..} -> do
      args' <- mapM (ppRightExpression appFixity) _typeConstrArgs
      n' <- ppName KNameConstructor (getInfoName _typeConstrInfo)
      return $ foldl' (<+>) n' args'
    NDyn {} -> return kwDynamic
    Closure env l@Lambda {} ->
      ppCode (substEnv env (NLam l))

instance PrettyCode Stripped.TypeApp where
  ppCode Stripped.TypeApp {..} = do
    args' <- mapM (ppRightExpression appFixity) _typeAppArgs
    n' <- ppName KNameLocal _typeAppName
    return $ foldl' (<+>) n' args'

instance PrettyCode Stripped.TypeFun where
  ppCode Stripped.TypeFun {..} = do
    l' <- ppLeftExpression funFixity _typeFunLeft
    r' <- ppRightExpression funFixity _typeFunRight
    return $ l' <+> kwArrow <+> r'

instance PrettyCode Stripped.Type where
  ppCode = \case
    Stripped.TyDynamic -> return kwAny
    Stripped.TyPrim x -> ppCode x
    Stripped.TyApp x -> ppCode x
    Stripped.TyFun x -> ppCode x

instance PrettyCode Stripped.Node where
  ppCode = \case
    Stripped.NVar x ->
      let name = x ^. (varInfo . Stripped.varInfoName)
       in ppCodeVar' name x
    Stripped.NIdt x ->
      let name = x ^. (identInfo . Stripped.identInfoName)
       in ppName KNameLocal name
    Stripped.NCst x -> ppCode x
    Stripped.NApp x -> ppCode x
    Stripped.NBlt x -> ppCode x
    Stripped.NCtr x ->
      let name = x ^. (constrInfo . Stripped.constrInfoName)
       in ppCodeConstr' name x
    Stripped.NLet x ->
      let name = x ^. (letItem . letItemBinder . binderName)
          ty = x ^. (letItem . letItemBinder . binderType)
       in ppCode ty >>= \tty -> ppCodeLet' name (Just tty) x
    Stripped.NCase x@Stripped.Case {..} ->
      let branchBinderNames = map (map (^. binderName) . (^. caseBranchBinders)) _caseBranches
          branchTagNames = map (^. (caseBranchInfo . Stripped.caseBranchInfoConstrName)) _caseBranches
       in ppCodeCase' branchBinderNames branchTagNames x

instance PrettyCode ConstructorInfo where
  ppCode :: (Member (Reader Options) r) => ConstructorInfo -> Sem r (Doc Ann)
  ppCode ci = do
    name <- ppName KNameConstructor (ci ^. constructorName)
    ty <- ppCode (ci ^. constructorType)
    return $ name <+> colon <+> ty

instance PrettyCode InfoTable where
  ppCode :: forall r. (Member (Reader Options) r) => InfoTable -> Sem r (Doc Ann)
  ppCode tbl = do
    tys <- ppInductives (toList (tbl ^. infoInductives))
    ctx' <- ppContext (tbl ^. identContext)
    return ("-- Types" <> line <> tys <> line <> "-- Identifiers" <> line <> ctx' <> line)
    where
      ppContext :: IdentContext -> Sem r (Doc Ann)
      ppContext ctx = do
        defs <- mapM (uncurry ppDef) (HashMap.toList ctx)
        return (vsep defs)
        where
          ppDef :: Symbol -> Node -> Sem r (Doc Ann)
          ppDef s n = do
            let mname :: Text
                mname = tbl ^. infoIdentifiers . at s . _Just . identifierName
                mname' = (\nm -> nm <> "!" <> prettyText s) mname
            sym' <- ppName KNameLocal mname'
            body' <- ppCode n
            let ii = fromJust $ HashMap.lookup s (tbl ^. infoIdentifiers)
                ty = ii ^. identifierType
            ty' <- ppCode ty
            let tydoc = if isDynamic ty then mempty else space <> colon <+> ty'
            return (kwDef <+> sym' <> tydoc <+> kwAssign <+> nest 2 body')
      ppInductives :: [InductiveInfo] -> Sem r (Doc Ann)
      ppInductives inds = do
        inds' <- mapM ppInductive inds
        return (vsep inds')
        where
          ppInductive :: InductiveInfo -> Sem r (Doc Ann)
          ppInductive ii = do
            name <- ppName KNameInductive (ii ^. inductiveName)
            ctrs <- mapM (fmap (<> semi) . ppCode) (ii ^. inductiveConstructors)
            return (kwInductive <+> name <+> braces (line <> indent' (vsep ctrs) <> line))

instance PrettyCode Stripped.ArgumentInfo where
  ppCode :: (Member (Reader Options) r) => Stripped.ArgumentInfo -> Sem r (Doc Ann)
  ppCode Stripped.ArgumentInfo {..} = do
    name <- ppName KNameLocal _argumentName
    ty <- ppCode _argumentType
    return $ name <+> colon <+> ty

instance PrettyCode Stripped.InfoTable where
  ppCode :: forall r. (Member (Reader Options) r) => Stripped.InfoTable -> Sem r (Doc Ann)
  ppCode tbl = do
    ctx' <- ppFunctions (tbl ^. Stripped.infoFunctions)
    return ("-- Functions" <> line <> ctx' <> line)
    where
      ppFunctions :: HashMap Symbol Stripped.FunctionInfo -> Sem r (Doc Ann)
      ppFunctions ctx = do
        defs <- mapM (uncurry ppDef) (HashMap.toList ctx)
        return (vsep defs)
        where
          ppDef :: Symbol -> Stripped.FunctionInfo -> Sem r (Doc Ann)
          ppDef _ fi = do
            sym' <- ppName KNameFunction (fi ^. Stripped.functionName)
            args <- mapM ppCode (fi ^. Stripped.functionArgsInfo)
            body' <- ppCode (fi ^. Stripped.functionBody)
            return (kwDef <+> sym' <> encloseSep lparen rparen ", " args <+> kwAssign <+> body')

instance (PrettyCode a) => PrettyCode (NonEmpty a) where
  ppCode x = ppCode (toList x)

instance (PrettyCode a) => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode x
    return $ encloseSep "(" ")" ", " cs

{--------------------------------------------------------------------------------}
{- helper functions -}

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

kwSquareL :: Doc Ann
kwSquareL = delimiter "["

kwSquareR :: Doc Ann
kwSquareR = delimiter "]"

kwAny :: Doc Ann
kwAny = keyword Str.any

kwDeBruijnVar :: Doc Ann
kwDeBruijnVar = keyword Str.deBruijnVar

kwUnnamedIdent :: Doc Ann
kwUnnamedIdent = keyword Str.exclamation

kwUnnamedConstr :: Doc Ann
kwUnnamedConstr = keyword Str.exclamation

kwQuestion :: Doc Ann
kwQuestion = keyword Str.questionMark

primLess :: Doc Ann
primLess = primitive Str.less

primLessEquals :: Doc Ann
primLessEquals = primitive Str.lessEqual

primPlus :: Doc Ann
primPlus = primitive Str.plus

primMinus :: Doc Ann
primMinus = primitive Str.minus

primMul :: Doc Ann
primMul = primitive Str.mul

primDiv :: Doc Ann
primDiv = primitive Str.div

primMod :: Doc Ann
primMod = primitive Str.mod

primEquals :: Doc Ann
primEquals = primitive Str.equal

primShow :: Doc Ann
primShow = primitive Str.show_

primStrConcat :: Doc Ann
primStrConcat = primitive Str.strConcat

primStrToInt :: Doc Ann
primStrToInt = primitive Str.strToInt

kwLetRec :: Doc Ann
kwLetRec = keyword Str.letrec_

kwOf :: Doc Ann
kwOf = keyword Str.of_

kwMatch :: Doc Ann
kwMatch = keyword Str.match_

kwWith :: Doc Ann
kwWith = keyword Str.with_

kwIf :: Doc Ann
kwIf = keyword Str.if_

kwThen :: Doc Ann
kwThen = keyword Str.then_

kwElse :: Doc Ann
kwElse = keyword Str.else_

kwDefault :: Doc Ann
kwDefault = keyword Str.underscore

kwPi :: Doc Ann
kwPi = keyword Str.piUnicode

kwDef :: Doc Ann
kwDef = keyword Str.def

primFail :: Doc Ann
primFail = primitive Str.fail_

primTrace :: Doc Ann
primTrace = primitive Str.trace_

kwFail :: Doc Ann
kwFail = keyword Str.fail_

kwDynamic :: Doc Ann
kwDynamic = keyword Str.any
