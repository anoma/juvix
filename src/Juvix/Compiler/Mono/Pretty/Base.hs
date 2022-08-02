module Juvix.Compiler.Mono.Pretty.Base
  ( module Juvix.Compiler.Mono.Pretty.Base,
    module Juvix.Compiler.Mono.Pretty.Options,
  )
where

import Juvix.Compiler.Mono.Language
import Juvix.Compiler.Mono.Pretty.Ann
import Juvix.Compiler.Mono.Pretty.Options
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

docStream :: PrettyCode c => Options -> c -> SimpleDocStream Ann
docStream opts = layoutPretty defaultLayoutOptions . doc opts

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
    uid <-
      if
          | showNameId -> Just . ("@" <>) <$> ppCode (n ^. nameId)
          | otherwise -> return Nothing
    return $
      annotate (AnnKind (n ^. nameKind)) $
        pretty (n ^. nameText)
          <>? uid

instance PrettyCode Iden where
  ppCode :: Member (Reader Options) r => Iden -> Sem r (Doc Ann)
  ppCode i = case i of
    IdenFunction na -> ppCode na
    IdenConstructor na -> ppCode na
    IdenVar na -> ppCode na
    IdenAxiom a -> ppCode a

instance PrettyCode Application where
  ppCode a = do
    l' <- ppLeftExpression appFixity (a ^. appLeft)
    r' <- ppRightExpression appFixity (a ^. appRight)
    return $ l' <+> r'

instance PrettyCode Expression where
  ppCode = \case
    ExpressionIden i -> ppCode i
    ExpressionApplication a -> ppCode a
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

kwC :: Doc Ann
kwC = keyword Str.cBackend

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

kwType :: Doc Ann
kwType = keyword Str.type_

kwWildcard :: Doc Ann
kwWildcard = keyword Str.underscore

instance PrettyCode BackendItem where
  ppCode BackendItem {..} = do
    backend <- ppCode _backendItemBackend
    return $
      backend <+> kwMapsto <+> pretty _backendItemCode

instance PrettyCode Function where
  ppCode (Function l r) = do
    l' <- ppLeftExpression funFixity l
    r' <- ppRightExpression funFixity r
    return $ l' <+> kwArrow <+> r'

instance PrettyCode TypeIden where
  ppCode = \case
    TypeIdenInductive i -> ppCode i
    TypeIdenAxiom i -> ppCode i

instance PrettyCode Type where
  ppCode = \case
    TypeIden i -> ppCode i
    TypeFunction f -> ppCode f
    TypeUniverse -> return kwType

instance PrettyCode InductiveConstructorDef where
  ppCode c = do
    constructorName' <- ppCode (c ^. constructorName)
    constructorParameters' <- mapM ppCodeAtom (c ^. constructorParameters)
    return (hsep $ constructorName' : constructorParameters')

indent' :: Member (Reader Options) r => Doc a -> Sem r (Doc a)
indent' d = do
  i <- asks (^. optIndent)
  return $ indent i d

ppBlock :: (PrettyCode a, Members '[Reader Options] r, Traversable t) => t a -> Sem r (Doc Ann)
ppBlock items = mapM ppCode items >>= bracesIndent . vsep . toList

bracesIndent :: Members '[Reader Options] r => Doc Ann -> Sem r (Doc Ann)
bracesIndent d = do
  d' <- indent' d
  return $ braces (line <> d' <> line)

instance PrettyCode InductiveDef where
  ppCode d = do
    inductiveName' <- ppCode (d ^. inductiveName)
    inductiveConstructors' <- mapM ppCode (d ^. inductiveConstructors)
    rhs <- indent' $ align $ concatWith (\a b -> a <> line <> kwPipe <+> b) inductiveConstructors'
    return $ kwData <+> inductiveName' <+> kwEquals <> line <> rhs

instance PrettyCode ConstructorApp where
  ppCode c = do
    constr' <- ppCode (c ^. constrAppConstructor)
    params' <- hsepMaybe <$> mapM ppCodeAtom (c ^. constrAppParameters)
    return (constr' <+?> params')

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
      funDefName'
        <+> kwColonColon
        <+> funDefType'
          <> line
          <> vsep (toList clauses')

instance PrettyCode FunctionClause where
  ppCode c = do
    funName <- ppCode (c ^. clauseName)
    clausePatterns' <- hsepMaybe <$> mapM ppCodeAtom (c ^. clausePatterns)
    clauseBody' <- ppCode (c ^. clauseBody)
    return $ funName <+?> clausePatterns' <+> kwEquals <+> clauseBody'

instance PrettyCode Backend where
  ppCode = \case
    BackendGhc -> return kwGhc
    BackendC -> return kwC

instance PrettyCode ForeignBlock where
  ppCode ForeignBlock {..} = do
    _foreignBackend' <- ppCode _foreignBackend
    return $
      kwForeign
        <+> _foreignBackend'
        <+> lbrace
          <> line
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

instance PrettyCode Module where
  ppCode m = do
    name' <- ppCode (m ^. moduleName)
    body' <- ppCode (m ^. moduleBody)
    return $
      kwModule
        <+> name'
        <+> kwWhere
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
