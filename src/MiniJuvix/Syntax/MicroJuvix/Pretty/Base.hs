module MiniJuvix.Syntax.MicroJuvix.Pretty.Base
  ( module MiniJuvix.Syntax.MicroJuvix.Pretty.Base,
    module MiniJuvix.Syntax.MicroJuvix.Pretty.Ann,
    module MiniJuvix.Syntax.MicroJuvix.Pretty.Options,
  )
where

import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.ForeignBlock
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Pretty.Options
import Prettyprinter

docStream :: PrettyCode c => Options -> c -> SimpleDocStream Ann
docStream opts =
  layoutPretty defaultLayoutOptions
    . run
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
    showNameId <- asks _optShowNameId
    uid <-
      if
          | showNameId -> Just . ("@" <>) <$> ppCode (n ^. nameId)
          | otherwise -> return Nothing
    return $
      annotate (AnnKind (n ^. nameKind)) $
        pretty (n ^. nameText) <?> uid

instance PrettyCode Iden where
  ppCode :: Member (Reader Options) r => Iden -> Sem r (Doc Ann)
  ppCode i = case i of
    IdenFunction na -> ppCode na
    IdenConstructor na -> ppCode na
    IdenVar na -> ppCode na
    IdenAxiom a -> ppCode a
    IdenInductive a -> ppCode a

instance PrettyCode TypeApplication where
  ppCode (TypeApplication l r) = do
    l' <- ppLeftExpression appFixity l
    r' <- ppRightExpression appFixity r
    return $ l' <+> r'

instance PrettyCode Application where
  ppCode a = do
    l' <- ppLeftExpression appFixity (a ^. appLeft)
    r' <- ppRightExpression appFixity (a ^. appRight)
    return $ l' <+> r'

instance PrettyCode TypedExpression where
  ppCode e = ppCode (e ^. typedExpression)

instance PrettyCode Expression where
  ppCode = \case
    ExpressionIden i -> ppCode i
    ExpressionApplication a -> ppCode a
    ExpressionTyped a -> ppCode a
    ExpressionLiteral l -> return (pretty l)

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

kwArrow :: Doc Ann
kwArrow = keyword Str.toAscii

kwMapsto :: Doc Ann
kwMapsto = keyword Str.mapstoUnicode

kwForeign :: Doc Ann
kwForeign = keyword Str.foreign_

kwCompile :: Doc Ann
kwCompile = keyword Str.compile

kwAgda :: Doc Ann
kwAgda = keyword Str.agda

kwGhc :: Doc Ann
kwGhc = keyword Str.ghc

kwColon :: Doc Ann
kwColon = keyword Str.colon

kwData :: Doc Ann
kwData = keyword Str.data_

kwEquals :: Doc Ann
kwEquals = keyword Str.equal

kwColonColon :: Doc Ann
kwColonColon = keyword (Str.colon <> Str.colon)

kwPipe :: Doc Ann
kwPipe = keyword Str.pipe

kwAxiom :: Doc Ann
kwAxiom = keyword Str.axiom

kwWhere :: Doc Ann
kwWhere = keyword Str.where_

kwModule :: Doc Ann
kwModule = keyword Str.module_

kwAny :: Doc Ann
kwAny = keyword Str.any

kwType :: Doc Ann
kwType = keyword Str.type_

kwWildcard :: Doc Ann
kwWildcard = keyword Str.underscore

instance PrettyCode BackendItem where
  ppCode BackendItem {..} = do
    backend <- ppCode _backendItemBackend
    return $
      backend <+> kwMapsto <+> pretty _backendItemCode

instance PrettyCode TypeAbstraction where
  ppCode (TypeAbstraction v r) = do
    v' <- ppCode v
    let l' = parens (v' <+> colon <+> kwType)
    r' <- ppRightExpression funFixity r
    return $ l' <+> kwArrow <+> r'

instance PrettyCode Function where
  ppCode (Function l r) = do
    l' <- ppLeftExpression funFixity l
    r' <- ppRightExpression funFixity r
    return $ l' <+> kwArrow <+> r'

instance PrettyCode TypeIden where
  ppCode = \case
    TypeIdenInductive i -> ppCode i
    TypeIdenAxiom i -> ppCode i
    TypeIdenVariable i -> ppCode i

instance PrettyCode FunctionArgType where
  ppCode = \case
    FunctionArgTypeType t -> ppCode t
    FunctionArgTypeAbstraction v -> ppCode v

instance PrettyCode Type where
  ppCode = \case
    TypeIden i -> ppCode i
    TypeFunction f -> ppCode f
    TypeUniverse -> return kwType
    TypeAny -> return kwAny
    TypeApp a -> ppCode a
    TypeAbs a -> ppCode a

instance PrettyCode InductiveConstructorDef where
  ppCode c = do
    constructorName' <- ppCode (c ^. constructorName)
    constructorParameters' <- mapM ppCodeAtom (c ^. constructorParameters)
    return (hsep $ constructorName' : constructorParameters')

indent' :: Member (Reader Options) r => Doc a -> Sem r (Doc a)
indent' d = do
  i <- asks _optIndent
  return $ indent i d

ppBlock ::
  (PrettyCode a, Members '[Reader Options] r, Traversable t) =>
  t a ->
  Sem r (Doc Ann)
ppBlock items = mapM ppCode items >>= bracesIndent . vsep . toList

bracesIndent :: Members '[Reader Options] r => Doc Ann -> Sem r (Doc Ann)
bracesIndent d = do
  d' <- indent' d
  return $ braces (line <> d' <> line)

instance PrettyCode InductiveParameter where
  ppCode (InductiveParameter v) = do
    v' <- ppCode v
    return $ parens (v' <+> kwColon <+> kwType)

instance PrettyCode InductiveDef where
  ppCode d = do
    inductiveName' <- ppCode (d ^. inductiveName)
    params <- hsep' <$> mapM ppCode (d ^. inductiveParameters)
    inductiveConstructors' <- mapM ppCode (d ^. inductiveConstructors)
    rhs <- indent' $ align $ concatWith (\a b -> a <> line <> kwPipe <+> b) inductiveConstructors'
    return $ kwData <+> inductiveName' <+?> params <+> kwEquals <> line <> rhs
    where
      hsep' l
        | null l = Nothing
        | otherwise = Just (hsep l)

instance PrettyCode ConstructorApp where
  ppCode c = do
    constr' <- ppCode (c ^. constrAppConstructor)
    params' <- mapM ppCodeAtom (c ^. constrAppParameters)
    return $ hsep $ constr' : params'

instance PrettyCode Pattern where
  ppCode p = case p of
    PatternVariable v -> ppCode v
    PatternConstructorApp a -> ppCode a
    PatternWildcard -> return kwWildcard

instance PrettyCode FunctionDef where
  ppCode f = do
    funDefName' <- ppCode (f ^. funDefName)
    funDefType' <- ppCode (f ^. funDefType)
    clauses' <- mapM ppCode (f ^. funDefClauses)
    return $
      funDefName' <+> kwColonColon <+> funDefType' <> line
        <> vsep (toList clauses')

instance PrettyCode FunctionClause where
  ppCode c = do
    funName <- ppCode (c ^. clauseName)
    clausePatterns' <- mapM ppCodeAtom (c ^. clausePatterns)
    clauseBody' <- ppCode (c ^. clauseBody)
    return $ funName <+> hsep clausePatterns' <+> kwEquals <+> clauseBody'

instance PrettyCode Backend where
  ppCode = \case
    BackendGhc -> return kwGhc
    BackendAgda -> return kwAgda

instance PrettyCode ForeignBlock where
  ppCode ForeignBlock {..} = do
    _foreignBackend' <- ppCode _foreignBackend
    return $
      kwForeign <+> _foreignBackend' <+> lbrace <> line
        <> pretty _foreignCode
        <> line
        <> rbrace

instance PrettyCode AxiomDef where
  ppCode AxiomDef {..} = do
    axiomName' <- ppCode _axiomName
    axiomType' <- ppCode _axiomType
    return $ kwAxiom <+> axiomName' <+> kwColon <+> axiomType'

instance PrettyCode Statement where
  ppCode = \case
    StatementForeign f -> ppCode f
    StatementFunction f -> ppCode f
    StatementInductive f -> ppCode f
    StatementAxiom f -> ppCode f

instance PrettyCode ModuleBody where
  ppCode m = do
    everything <- mapM ppCode (m ^. moduleStatements)
    return $ vsep2 everything
    where
      vsep2 = concatWith (\a b -> a <> line <> line <> b)

instance PrettyCode Module where
  ppCode m = do
    name' <- ppCode (m ^. moduleName)
    body' <- ppCode (m ^. moduleBody)
    return $
      kwModule <+> name' <+> kwWhere
        <> line
        <> line
        <> body'
        <> line

parensCond :: Bool -> Doc Ann -> Doc Ann
parensCond t d = if t then parens d else d

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
  parensCond (atomParens associates (atomicity e) fixlr)
    <$> ppCode e

ppCodeAtom :: (HasAtomicity c, PrettyCode c, Members '[Reader Options] r) => c -> Sem r (Doc Ann)
ppCodeAtom c = do
  p' <- ppCode c
  return $ if isAtomic c then p' else parens p'
