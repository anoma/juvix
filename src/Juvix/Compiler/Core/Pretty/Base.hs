module Juvix.Compiler.Core.Pretty.Base
  ( module Juvix.Compiler.Core.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Core.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Stripped
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info qualified as Info
import Juvix.Compiler.Core.Info.BinderInfo
import Juvix.Compiler.Core.Info.NameInfo as NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Stripped qualified as Stripped
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

instance PrettyCode Primitive where
  ppCode = \case
    PrimInteger _ -> return $ annotate (AnnKind KNameInductive) (pretty ("integer" :: String))
    PrimBool _ -> return $ annotate (AnnKind KNameInductive) (pretty ("bool" :: String))
    PrimString -> return $ annotate (AnnKind KNameInductive) (pretty ("string" :: String))

ppCodeVar' :: Member (Reader Options) r => Maybe Name -> Var' i -> Sem r (Doc Ann)
ppCodeVar' name v =
  case name of
    Just nm -> do
      showDeBruijn <- asks (^. optShowDeBruijnIndices)
      n <- ppCode nm
      if showDeBruijn
        then return $ n <> kwDeBruijnVar <> pretty (v ^. varIndex)
        else return n
    Nothing -> return $ kwDeBruijnVar <> pretty (v ^. varIndex)

ppCodeIdent' :: Member (Reader Options) r => Maybe Name -> Ident' i -> Sem r (Doc Ann)
ppCodeIdent' name idt =
  case name of
    Just nm -> ppCode nm
    Nothing -> return $ kwUnnamedIdent <> pretty (idt ^. identSymbol)

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
    Stripped.FunIdent x -> ppCodeIdent' (x ^. (identInfo . Stripped.identInfoName)) x

instance (PrettyCode f, PrettyCode a, HasAtomicity a) => PrettyCode (Apps' f i a) where
  ppCode Apps {..} = do
    args' <- mapM (ppRightExpression appFixity) _appsArgs
    n' <- ppCode _appsFun
    return $ foldl' (<+>) n' args'

instance (PrettyCode a, HasAtomicity a) => PrettyCode (BuiltinApp' i a) where
  ppCode BuiltinApp {..} = do
    args' <- mapM (ppRightExpression appFixity) _builtinAppArgs
    op' <- ppCode _builtinAppOp
    return $ foldl' (<+>) op' args'

ppCodeConstr' :: (PrettyCode a, HasAtomicity a, Member (Reader Options) r) => Maybe Name -> Constr' i a -> Sem r (Doc Ann)
ppCodeConstr' name c = do
  args' <- mapM (ppRightExpression appFixity) (c ^. constrArgs)
  n' <- case name of
    Just nm -> ppCode nm
    Nothing -> ppCode (c ^. constrTag)
  return $ foldl' (<+>) n' args'

ppCodeLet' :: (PrettyCode a, Member (Reader Options) r) => Maybe Name -> Let' i a -> Sem r (Doc Ann)
ppCodeLet' name lt = do
  n' <- case name of
    Just nm -> ppCode nm
    Nothing -> return kwQuestion
  v' <- ppCode (lt ^. letValue)
  b' <- ppCode (lt ^. letBody)
  return $ kwLet <+> n' <+> kwAssign <+> v' <+> kwIn <+> b'

ppCodeCase' :: (PrettyCode a, Member (Reader Options) r) => [[Maybe Name]] -> [Maybe Name] -> Case' i bi a -> Sem r (Doc Ann)
ppCodeCase' branchBinderNames branchTagNames Case {..} = do
  let branchTags = map (^. caseBranchTag) _caseBranches
  let branchBodies = map (^. caseBranchBody) _caseBranches
  bns <- mapM (mapM (maybe (return kwQuestion) ppCode)) branchBinderNames
  cns <- zipWithM (\tag -> maybe (ppCode tag) ppCode) branchTags branchTagNames
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
    n <- case getInfoName _patternBinderInfo of
      Just name -> ppCode name
      Nothing -> return kwQuestion
    case _patternBinderPattern of
      PatWildcard {} -> return n
      _ -> do
        pat <- ppRightExpression appFixity _patternBinderPattern
        return $ n <> kwAt <> pat

instance PrettyCode PatternConstr where
  ppCode PatternConstr {..} = do
    n <- maybe (ppCode _patternConstrTag) ppCode (getInfoName _patternConstrInfo)
    args <- mapM (ppRightExpression appFixity) _patternConstrArgs
    return $ foldl' (<+>) n args

instance PrettyCode Pattern where
  ppCode = \case
    PatWildcard x -> ppCode x
    PatBinder x -> ppCode x
    PatConstr x -> ppCode x

ppPatterns :: Member (Reader Options) r => NonEmpty Pattern -> Sem r (Doc Ann)
ppPatterns pats = do
  ps' <- mapM ppCode pats
  return $ hsep (punctuate comma (toList ps'))

instance PrettyCode Node where
  ppCode :: forall r. Member (Reader Options) r => Node -> Sem r (Doc Ann)
  ppCode node = case node of
    NVar x ->
      let name = getInfoName (x ^. varInfo)
       in ppCodeVar' name x
    NIdt x ->
      let name = getInfoName (x ^. identInfo)
       in ppCodeIdent' name x
    NCst x -> ppCode x
    NApp x -> ppCode x
    NBlt x -> ppCode x
    NCtr x ->
      let name = getInfoName (x ^. constrInfo)
       in ppCodeConstr' name x
    NLam Lambda {} -> do
      let (infos, body) = unfoldLambdas node
      pplams <- mapM ppLam infos
      b <- ppCode body
      return $ foldl' (flip (<+>)) b pplams
      where
        ppLam :: Info -> Sem r (Doc Ann)
        ppLam i =
          case getInfoName (getInfoBinder i) of
            Just name -> do
              n <- ppCode name
              return $ kwLambda <> n
            Nothing -> return $ kwLambda <> kwQuestion
    NLet x ->
      let name = getInfoName (getInfoBinder (x ^. letInfo))
       in ppCodeLet' name x
    NRec LetRec {..} -> do
      let n = length _letRecValues
      ns <- mapM getName (getInfoBinders n _letRecInfo)
      vs <- mapM ppCode _letRecValues
      b' <- ppCode _letRecBody
      case listToMaybe ns of
        Just hns -> return $ kwLetRec <+> hns <+> kwAssign <+> head vs <+> kwIn <+> b'
        Nothing ->
          let bss =
                indent' $
                  align $
                    concatWith (\a b -> a <> kwSemicolon <> line <> b) $
                      zipWithExact (\name val -> name <+> kwAssign <+> val) ns (toList vs)
              nss = enclose kwSquareL kwSquareR (concatWith (<+>) ns)
           in return $ kwLetRec <> nss <> line <> bss <> line <> kwIn <> line <> b'
      where
        getName :: Info -> Sem r (Doc Ann)
        getName i =
          case getInfoName i of
            Just name -> ppCode name
            Nothing -> return kwQuestion
    NCase x@Case {..} ->
      let branchBinderNames = map (\(CaseBranch {..}) -> map getInfoName (getInfoBinders _caseBranchBindersNum _caseBranchInfo)) _caseBranches
          branchTagNames = map (\(CaseBranch {..}) -> getInfoName _caseBranchInfo) _caseBranches
       in ppCodeCase' branchBinderNames branchTagNames x
    NMatch Match {..} -> do
      let branchPatterns = map (^. matchBranchPatterns) _matchBranches
      let branchBodies = map (^. matchBranchBody) _matchBranches
      pats <- mapM ppPatterns branchPatterns
      vs <- mapM ppCode _matchValues
      bs <- sequence $ zipWithExact (\ps br -> ppCode br >>= \br' -> return $ ps <+> kwMapsto <+> br') pats branchBodies
      let bss = bracesIndent $ align $ concatWith (\a b -> a <> kwSemicolon <> line <> b) bs
      return $ kwMatch <+> hsep (punctuate comma (toList vs)) <+> kwWith <+> bss
    NPi Pi {..} ->
      case getInfoName $ getInfoBinder _piInfo of
        Just name -> do
          n <- ppCode name
          b <- ppCode _piBody
          return $ kwLambda <> n <+> b
        Nothing -> do
          b <- ppCode _piBody
          return $ kwLambda <> kwQuestion <+> b
    NUniv Univ {..} ->
      return $ kwType <+> pretty _univLevel
    NPrim TypePrim {..} -> ppCode _typePrimPrimitive
    NTyp TypeConstr {..} -> do
      args' <- mapM (ppRightExpression appFixity) _typeConstrArgs
      n' <-
        case Info.lookup kNameInfo _typeConstrInfo of
          Just ni -> ppCode (ni ^. NameInfo.infoName)
          Nothing -> return $ kwUnnamedIdent <> pretty _typeConstrSymbol
      return $ foldl' (<+>) n' args'
    NDyn {} -> return kwDynamic
    Closure env l@Lambda {} ->
      ppCode (substEnv env (NLam l))

instance PrettyCode Stripped.Node where
  ppCode = \case
    Stripped.NVar x ->
      let name = x ^. (varInfo . Stripped.varInfoName)
       in ppCodeVar' name x
    Stripped.NIdt x ->
      let name = x ^. (identInfo . Stripped.identInfoName)
       in ppCodeIdent' name x
    Stripped.NCst x -> ppCode x
    Stripped.NApp x -> ppCode x
    Stripped.NBlt x -> ppCode x
    Stripped.NCtr x ->
      let name = x ^. (constrInfo . Stripped.constrInfoName)
       in ppCodeConstr' name x
    Stripped.NLet x ->
      let name = x ^. (letInfo . Stripped.letInfoBinderName)
       in ppCodeLet' name x
    Stripped.NCase x@Stripped.Case {..} ->
      let branchBinderNames = map (^. (caseBranchInfo . Stripped.caseBranchInfoBinderNames)) _caseBranches
          branchTagNames = map (^. (caseBranchInfo . Stripped.caseBranchInfoConstrName)) _caseBranches
       in ppCodeCase' branchBinderNames branchTagNames x

instance PrettyCode InfoTable where
  ppCode :: forall r. Member (Reader Options) r => InfoTable -> Sem r (Doc Ann)
  ppCode tbl = do
    ctx' <- ppContext (tbl ^. identContext)
    return ("-- IdentContext" <> line <> ctx' <> line)
    where
      ppContext :: IdentContext -> Sem r (Doc Ann)
      ppContext ctx = do
        defs <- mapM (uncurry ppDef) (HashMap.toList ctx)
        return (vsep defs)
        where
          ppDef :: Symbol -> Node -> Sem r (Doc Ann)
          ppDef s n = do
            sym' <- maybe (return (pretty s)) ppCode (tbl ^? infoIdentifiers . at s . _Just . identifierName . _Just)
            body' <- ppCode n
            return (kwDef <+> sym' <+> kwAssign <+> body')

instance PrettyCode Stripped.InfoTable where
  ppCode :: forall r. Member (Reader Options) r => Stripped.InfoTable -> Sem r (Doc Ann)
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
          ppDef s fi = do
            sym' <- maybe (return (pretty s)) ppCode (fi ^. Stripped.functionName)
            body' <- ppCode (fi ^. Stripped.functionBody)
            return (kwDef <+> sym' <+> kwAssign <+> body')

instance PrettyCode a => PrettyCode (NonEmpty a) where
  ppCode x = ppCode (toList x)

instance PrettyCode a => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode x
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

kwAt :: Doc Ann
kwAt = delimiter "@"

kwSquareL :: Doc Ann
kwSquareL = delimiter "["

kwSquareR :: Doc Ann
kwSquareR = delimiter "]"

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

kwLetRec :: Doc Ann
kwLetRec = keyword Str.letrec_

kwCase :: Doc Ann
kwCase = keyword Str.case_

kwOf :: Doc Ann
kwOf = keyword Str.of_

kwMatch :: Doc Ann
kwMatch = keyword Str.match_

kwWith :: Doc Ann
kwWith = keyword Str.with_

kwDefault :: Doc Ann
kwDefault = keyword Str.underscore

kwPi :: Doc Ann
kwPi = keyword Str.pi_

kwDef :: Doc Ann
kwDef = keyword Str.def

kwTrace :: Doc Ann
kwTrace = keyword Str.trace_

kwFail :: Doc Ann
kwFail = keyword Str.fail_

kwDynamic :: Doc Ann
kwDynamic = keyword Str.mul
