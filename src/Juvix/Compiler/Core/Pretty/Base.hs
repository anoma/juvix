module Juvix.Compiler.Core.Pretty.Base
  ( module Juvix.Compiler.Core.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Core.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Stripped
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Extra.Utils.Base
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Stripped qualified as Stripped
import Juvix.Compiler.Core.Language.Value
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
    OpSeq -> return primSeq
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
    PrimInteger _ -> return $ annotate (AnnKind KNameInductive) (pretty ("Int" :: String))
    PrimBool _ -> return $ annotate (AnnKind KNameInductive) (pretty ("Bool" :: String))
    PrimString -> return $ annotate (AnnKind KNameInductive) (pretty ("String" :: String))

ppName :: NameKind -> Text -> Sem r (Doc Ann)
ppName kind name = return $ annotate (AnnKind kind) (pretty name)

ppIdentName :: Member (Reader Options) r => Text -> Symbol -> Sem r (Doc Ann)
ppIdentName name sym = do
  showIds <- asks (^. optShowIdentIds)
  let name' = if showIds then name <> "!" <> prettyText sym else name
  ppName KNameFunction name'

ppCodeVar' :: (Member (Reader Options) r) => Text -> Var' i -> Sem r (Doc Ann)
ppCodeVar' name v = do
  name' <- ppName KNameLocal name
  showDeBruijn <- asks (^. optShowDeBruijnIndices)
  if showDeBruijn || name == ""
    then return $ name' <> kwDeBruijnVar <> pretty (v ^. varIndex)
    else return name'

instance PrettyCode ConstantValue where
  ppCode = \case
    ConstInteger int ->
      return $ annotate AnnLiteralInteger (pretty int)
    ConstString txt ->
      return $ annotate AnnLiteralString (pretty (show txt :: String))

instance PrettyCode (Constant' i) where
  ppCode Constant {..} = ppCode _constantValue

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
  n' <- case c ^. constrTag of
    BuiltinTag tag -> ppCode tag
    _ -> ppName KNameConstructor name
  args' <- mapM (ppRightExpression appFixity) (c ^. constrArgs)
  return $ hsep (n' : args')

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

ppWithType :: Member (Reader Options) r => Doc Ann -> Type -> Sem r (Doc Ann)
ppWithType d = \case
  NDyn {} ->
    return d
  ty -> do
    ty' <- ppCode ty
    return $ parens (d <+> kwColon <+> ty')

ppNameTyped :: NameKind -> Text -> Maybe (Doc Ann) -> Sem r (Doc Ann)
ppNameTyped kn name mty = do
  n <- ppName kn name
  case mty of
    Nothing -> return n
    Just ty -> return $ parens (n <+> kwColon <+> ty)

ppType :: Member (Reader Options) r => Type -> Sem r (Maybe (Doc Ann))
ppType = \case
  NDyn {} -> return Nothing
  ty -> Just <$> ppCode ty

ppTypeAnnot :: Member (Reader Options) r => Type -> Sem r (Doc Ann)
ppTypeAnnot = \case
  NDyn {} ->
    return mempty
  ty -> do
    ty' <- ppCode ty
    return $ mempty <+> kwColon <+> parens ty'

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
  return $ kwLet <+> n' <> tty <+> kwAssign <> oneLineOrNext v' <+> kwIn <> line <> b'

ppCodeCase' :: (PrettyCode a, Member (Reader Options) r) => [[Text]] -> [[Maybe (Doc Ann)]] -> [Text] -> Case' i bi a ty -> Sem r (Doc Ann)
ppCodeCase' branchBinderNames branchBinderTypes branchTagNames Case {..} =
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
      bns <- zipWithM (zipWithM (ppNameTyped KNameLocal)) branchBinderNames branchBinderTypes
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

instance PrettyCode a => PrettyCode (If' i a) where
  ppCode If {..} = do
    v <- ppCode _ifValue
    l <- ppCode _ifTrue
    r <- ppCode _ifFalse
    return $ kwIf <+> v <+> kwThen <+> l <+> kwElse <+> r

instance PrettyCode PatternWildcard where
  ppCode PatternWildcard {..} = do
    n <- ppName KNameLocal (_patternWildcardBinder ^. binderName)
    ppWithType n (_patternWildcardBinder ^. binderType)

instance PrettyCode PatternConstr where
  ppCode PatternConstr {..} = do
    n <- ppName KNameConstructor (getInfoName _patternConstrInfo)
    bn <- ppName KNameLocal (_patternConstrBinder ^. binderName)
    let mkpat :: Doc Ann -> Doc Ann
        mkpat pat = if _patternConstrBinder ^. binderName == "?" || _patternConstrBinder ^. binderName == "" then pat else bn <> kwAt <> parens pat
    args <- mapM (ppRightExpression appFixity) _patternConstrArgs
    let pat = mkpat (hsep (n : args))
    ppWithType pat (_patternConstrBinder ^. binderType)

instance PrettyCode Pattern where
  ppCode = \case
    PatWildcard x -> ppCode x
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
    let tys = fmap (^. letItemBinder . binderType) _letRecValues
    names <- mapM (getName . (^. letItemBinder)) _letRecValues
    types <- mapM ppCode tys
    vs <- mapM (ppCode . (^. letItemValue)) _letRecValues
    b' <- ppCode _letRecBody
    let bs =
          zipWith3Exact
            (\n ty ty' -> if isDynamic ty' then n else n <+> colon <+> ty)
            (toList names)
            (toList types)
            (toList tys)
    return $ case bs of
      [hbs] -> kwLetRec <+> hbs <+> kwAssign <+> head vs <+> kwIn <+> b'
      _ ->
        let bss =
              indent' $
                align $
                  concatWith (\a b -> a <> kwSemicolon <> line <> b) $
                    zipWithExact (\b val -> b <+> kwAssign <+> val) (toList bs) (toList vs)
            nss = enclose kwSquareL kwSquareR (concatWith (<+>) names)
         in kwLetRec <> nss <> line <> bss <> kwSemicolon <> line <> kwIn <> line <> b'
    where
      getName :: Binder -> Sem r (Doc Ann)
      getName i = ppName KNameLocal (i ^. binderName)

instance PrettyCode Lambda where
  ppCode (Lambda _ bi body) = do
    b <- ppCode body
    lam <- do
      n <- ppName KNameLocal (bi ^. binderName)
      case bi ^. binderType of
        NDyn {} -> return $ kwLambda <> n
        ty -> do
          tty <- ppCode ty
          return $ kwLambda <> parens (n <+> kwColon <+> tty)
    return (lam <> oneLineOrNext b)

instance PrettyCode Bottom where
  ppCode :: Member (Reader Options) r => Bottom -> Sem r (Doc Ann)
  ppCode Bottom {..} = do
    ty' <- ppCode _bottomType
    return (parens (kwBottom <+> kwColon <+> ty'))

instance PrettyCode Node where
  ppCode :: forall r. (Member (Reader Options) r) => Node -> Sem r (Doc Ann)
  ppCode node = case node of
    NVar x ->
      let name = getInfoName (x ^. varInfo)
       in ppCodeVar' name x
    NIdt x -> do
      let name = getInfoName (x ^. identInfo)
       in ppIdentName name (x ^. identSymbol)
    NCst x -> ppCode x
    NApp x -> ppCode x
    NBlt x -> ppCode x
    NCtr x ->
      let name = getInfoName (x ^. constrInfo)
       in ppCodeConstr' name x
    NLam l -> ppCode l
    NLet x -> ppCode x
    NRec l -> ppCode l
    NCase x@Case {..} -> do
      let branchBinderNames = map (\CaseBranch {..} -> map (^. binderName) _caseBranchBinders) _caseBranches
          branchTagNames = map (\CaseBranch {..} -> getInfoName _caseBranchInfo) _caseBranches
      branchBinderTypes <- mapM (\CaseBranch {..} -> mapM (ppType . (^. binderType)) _caseBranchBinders) _caseBranches
      ppCodeCase' branchBinderNames branchBinderTypes branchTagNames x
    NMatch Match {..} -> do
      let branchPatterns = map (^. matchBranchPatterns) _matchBranches
          branchBodies = map (^. matchBranchBody) _matchBranches
      pats <- mapM ppPatterns branchPatterns
      vs <- mapM ppCode _matchValues
      vs' <- zipWithM ppWithType (toList vs) (toList _matchValueTypes)
      bs <- sequence $ zipWithExact (\ps br -> ppCode br >>= \br' -> return $ ps <+> kwAssign <+> br') pats branchBodies
      let bss = bracesIndent $ align $ concatWith (\a b -> a <> kwSemicolon <> line <> b) bs
      rty <- ppTypeAnnot _matchReturnType
      return $ kwMatch <+> hsep (punctuate comma vs') <+> kwWith <> rty <+> bss
    NPi p -> ppCode p
    NUniv u -> ppCode u
    NPrim TypePrim {..} -> ppCode _typePrimPrimitive
    NTyp TypeConstr {..} -> do
      args' <- mapM (ppRightExpression appFixity) _typeConstrArgs
      n' <- ppName KNameInductive (getInfoName _typeConstrInfo)
      return $ foldl' (<+>) n' args'
    NDyn {} -> return kwDynamic
    NBot b -> ppCode b
    Closure env n ->
      ppCode (substEnv env n)

instance PrettyCode Pi where
  ppCode Pi {..} =
    let piType = _piBinder ^. binderType
     in if
            | varOccurs 0 _piBody -> do
                n <- ppName KNameLocal (_piBinder ^. binderName)
                ty <- ppCode piType
                b <- ppCode _piBody
                return $ kwPi <+> n <+> kwColon <+> ty <> comma <+> b
            | otherwise -> do
                ty <- ppLeftExpression funFixity piType
                b <- ppRightExpression funFixity _piBody
                return $ ty <+> kwArrow <+> b

instance PrettyCode (Univ' i) where
  ppCode Univ {..} =
    return $
      if
          | _univLevel == 0 -> kwType
          | otherwise -> kwType <+> pretty _univLevel

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

ppTypeStripped :: Member (Reader Options) r => Stripped.Type -> Sem r (Maybe (Doc Ann))
ppTypeStripped = \case
  Stripped.TyDynamic -> return Nothing
  ty -> Just <$> ppCode ty

instance PrettyCode Stripped.Node where
  ppCode = \case
    Stripped.NVar x ->
      let name = x ^. (varInfo . Stripped.varInfoName)
       in ppCodeVar' name x
    Stripped.NIdt x ->
      let name = x ^. (identInfo . Stripped.identInfoName)
       in ppIdentName name (x ^. identSymbol)
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
    Stripped.NCase x@Stripped.Case {..} -> do
      let branchBinderNames = map (map (^. binderName) . (^. caseBranchBinders)) _caseBranches
          branchTagNames = map (^. (caseBranchInfo . Stripped.caseBranchInfoConstrName)) _caseBranches
      branchBinderTypes <- mapM (\CaseBranch {..} -> mapM (ppTypeStripped . (^. binderType)) _caseBranchBinders) _caseBranches
      ppCodeCase' branchBinderNames branchBinderTypes branchTagNames x
    Stripped.NIf x -> ppCode x

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
    sigs <- ppSigs (sortOn (^. identifierSymbol) $ toList (tbl ^. infoIdentifiers))
    ctx' <- ppContext (tbl ^. identContext)
    main <- maybe (return "") (\s -> (<> line) . (line <>) <$> ppName KNameFunction (identName tbl s)) (tbl ^. infoMain)
    return (tys <> line <> line <> sigs <> line <> ctx' <> line <> main)
    where
      ppSig :: Symbol -> Sem r (Maybe (Doc Ann))
      ppSig s = do
        showIds <- asks (^. optShowIdentIds)
        let mname :: Text
            mname = tbl ^. infoIdentifiers . at s . _Just . identifierName
            mname' = if showIds then (\nm -> nm <> "!" <> prettyText s) mname else mname
        sym' <- ppName KNameFunction mname'
        let -- the identifier may be missing if we have filtered out some
            -- identifiers for printing purposes
            mii = lookupIdentifierInfo' tbl s
        case mii of
          Nothing -> return Nothing
          Just ii -> do
            showArgsNum <- asks (^. optShowArgsNum)
            let argsNum = if showArgsNum then brackets (pretty (ii ^. identifierArgsNum)) else mempty
                ty = ii ^. identifierType
            ty' <- ppCode ty
            let tydoc
                  | isDynamic ty = mempty
                  | otherwise = space <> colon <+> ty'
                blt = if isJust (ii ^. identifierBuiltin) then (Str.builtin <+> mempty) else mempty
            return (Just (blt <> kwDef <+> sym' <> argsNum <> tydoc))

      ppSigs :: [IdentifierInfo] -> Sem r (Doc Ann)
      ppSigs idents = do
        pp <- mapM (\ii -> ppSig (ii ^. identifierSymbol)) idents
        return $ foldr (\p acc -> p <> kwSemicolon <> line <> acc) "" (catMaybes pp)

      ppContext :: IdentContext -> Sem r (Doc Ann)
      ppContext ctx = do
        defs <- mapM (uncurry ppDef) (sortOn fst (HashMap.toList ctx))
        return (vsep (catMaybes defs))
        where
          ppDef :: Symbol -> Node -> Sem r (Maybe (Doc Ann))
          ppDef s n = do
            msig <- ppSig s
            case msig of
              Just sig -> do
                body' <- ppCode n
                return (Just (sig <+> kwAssign <> oneLineOrNext body' <> kwSemicolon))
              Nothing ->
                return Nothing

      ppInductives :: [InductiveInfo] -> Sem r (Doc Ann)
      ppInductives inds = do
        inds' <- mapM ppInductive (filter (isNothing . (^. inductiveBuiltin)) inds)
        return (vsep inds')
        where
          ppInductive :: InductiveInfo -> Sem r (Doc Ann)
          ppInductive ii = do
            name <- ppName KNameInductive (ii ^. inductiveName)
            ctrs <- mapM (fmap (<> semi) . ppCode . lookupConstructorInfo tbl) (ii ^. inductiveConstructors)
            return (kwInductive <+> name <+> braces (line <> indent' (vsep ctrs) <> line) <> kwSemicolon)

instance PrettyCode Stripped.ArgumentInfo where
  ppCode :: (Member (Reader Options) r) => Stripped.ArgumentInfo -> Sem r (Doc Ann)
  ppCode Stripped.ArgumentInfo {..} = do
    name <- ppName KNameLocal _argumentName
    ty <- ppCode _argumentType
    return $ name <+> colon <+> ty

instance PrettyCode Stripped.ConstructorInfo where
  ppCode :: (Member (Reader Options) r) => Stripped.ConstructorInfo -> Sem r (Doc Ann)
  ppCode ci = do
    name <- ppName KNameConstructor (ci ^. Stripped.constructorName)
    ty <- ppCode (ci ^. Stripped.constructorType)
    return $ name <+> colon <+> ty

instance PrettyCode Stripped.InfoTable where
  ppCode :: forall r. (Member (Reader Options) r) => Stripped.InfoTable -> Sem r (Doc Ann)
  ppCode tbl = do
    inds' <- ppInductives (HashMap.elems (tbl ^. Stripped.infoInductives))
    ctx' <- ppFunctions (tbl ^. Stripped.infoFunctions)
    return ("-- Types" <> line <> inds' <> line <> "-- Functions" <> line <> ctx' <> line)
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

      ppInductives :: [Stripped.InductiveInfo] -> Sem r (Doc Ann)
      ppInductives inds = do
        inds' <- mapM ppInductive inds
        return (vsep inds')
        where
          ppInductive :: Stripped.InductiveInfo -> Sem r (Doc Ann)
          ppInductive ii = do
            name <- ppName KNameInductive (ii ^. Stripped.inductiveName)
            ctrs <- mapM (fmap (<> semi) . ppCode . Stripped.lookupConstructorInfo tbl) (ii ^. Stripped.inductiveConstructors)
            return (kwInductive <+> name <+> braces (line <> indent' (vsep ctrs) <> line))

instance (PrettyCode a) => PrettyCode (NonEmpty a) where
  ppCode x = ppCode (toList x)

instance (PrettyCode a) => PrettyCode [a] where
  ppCode x = do
    cs <- mapM ppCode x
    return $ encloseSep "(" ")" ", " cs

--------------------------------------------------------------------------------
-- printing values
--------------------------------------------------------------------------------

goBinary :: Member (Reader Options) r => Fixity -> Doc Ann -> [Value] -> Sem r (Doc Ann)
goBinary fixity name = \case
  [] -> return name
  [arg] -> do
    arg' <- ppRightExpression appFixity arg
    return $ parens name <+> arg'
  [arg1, arg2] -> do
    arg1' <- ppLeftExpression fixity arg1
    arg2' <- ppRightExpression fixity arg2
    return $ arg1' <+> name <+> arg2'
  _ ->
    impossible

goUnary :: Member (Reader Options) r => Fixity -> Doc Ann -> [Value] -> Sem r (Doc Ann)
goUnary fixity name = \case
  [] -> return name
  [arg] -> do
    arg' <- ppPostExpression fixity arg
    return $ arg' <+> name
  _ ->
    impossible

instance PrettyCode ConstrApp where
  ppCode ConstrApp {..} = do
    n <- ppName KNameConstructor _constrAppName
    case _constrAppFixity of
      Nothing -> do
        args <- mapM (ppRightExpression appFixity) _constrAppArgs
        return $ hsep (n : args)
      Just fixity
        | isBinary fixity ->
            goBinary fixity n _constrAppArgs
        | isUnary fixity ->
            goUnary fixity n _constrAppArgs
      _ -> impossible

instance PrettyCode Value where
  ppCode = \case
    ValueConstrApp x -> ppCode x
    ValueConstant c -> ppCode c
    ValueWildcard -> return "_"
    ValueFun -> return "<function>"
    ValueType -> return "<type>"

ppValueSequence :: Member (Reader Options) r => [Value] -> Sem r (Doc Ann)
ppValueSequence vs = hsep <$> mapM (ppRightExpression appFixity) vs

docValueSequence :: [Value] -> Doc Ann
docValueSequence =
  run
    . runReader defaultOptions
    . ppValueSequence

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

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

primSeq :: Doc Ann
primSeq = primitive Str.seqq_

primFail :: Doc Ann
primFail = primitive Str.fail_

primTrace :: Doc Ann
primTrace = primitive Str.trace_

kwFail :: Doc Ann
kwFail = keyword Str.fail_

kwDynamic :: Doc Ann
kwDynamic = keyword Str.any

kwBottomAscii :: Doc Ann
kwBottomAscii = keyword Str.bottomAscii

kwBottom :: Doc Ann
kwBottom = keyword Str.bottom
