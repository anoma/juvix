module Juvix.Compiler.Backend.Haskell.Pretty.Base
  ( module Juvix.Compiler.Backend.Haskell.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Backend.Haskell.Pretty.Options,
  )
where

import Juvix.Compiler.Backend.Haskell.Language
import Juvix.Compiler.Backend.Haskell.Pretty.Options
import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Data.CodeAnn
import Juvix.Data.Fixity
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Prelude.Pretty qualified as PP

doc :: PrettyCode c => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

instance PrettyCode Name where
  ppCode n =
    return $
      annotate (AnnKind (n ^. nameKind)) $
        pretty (n ^. nameText)

instance PrettyCode Application where
  ppCode a = do
    l' <- ppLeftExpression appFixity (a ^. appLeft)
    r' <- ppRightExpression appFixity (a ^. appRight)
    return $ l' <+> r'

instance PrettyCode Expression where
  ppCode e = case e of
    ExpressionIden i -> ppCode i
    ExpressionApplication a -> ppCode a
    ExpressionVerbatim c -> return (pretty c)
    ExpressionLiteral l -> ppCode (l ^. withLocParam)

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

kwArrow :: Doc Ann
kwArrow = keyword Str.toAscii

kwData :: Doc Ann
kwData = keyword Str.data_

kwEquals :: Doc Ann
kwEquals = keyword Str.equal

kwColonColon :: Doc Ann
kwColonColon = keyword (Str.colon <> Str.colon)

kwPipe :: Doc Ann
kwPipe = keyword Str.pipe

kwWhere :: Doc Ann
kwWhere = keyword Str.where_

kwModule :: Doc Ann
kwModule = keyword Str.module_

kwWildcard :: Doc Ann
kwWildcard = keyword Str.underscore

instance PrettyCode Function where
  ppCode (Function l r) = do
    l' <- ppLeftExpression funFixity l
    r' <- ppRightExpression funFixity r
    return $ l' <+> kwArrow <+> r'

instance PrettyCode TypeIden where
  ppCode = \case
    TypeIdenInductive t -> ppCode t

instance PrettyCode Type where
  ppCode t = case t of
    TypeIden n -> ppCode n
    TypeFunction f -> ppCode f
    TypeVerbatim c -> return (pretty c)

instance PrettyCode InductiveConstructorDef where
  ppCode c = do
    constructorName' <- ppCode (c ^. constructorName)
    constructorParameters' <- mapM ppCode (c ^. constructorParameters)
    return (hsep $ constructorName' : constructorParameters')

instance PrettyCode InductiveDef where
  ppCode d = do
    inductiveName' <- ppCode (d ^. inductiveName)
    inductiveConstructors' <- mapM ppCode (d ^. inductiveConstructors)
    let rhs = indent' $ align $ concatWith (\a b -> a <> line <> kwPipe <+> b) inductiveConstructors'
    return $ kwData <+> inductiveName' <+> kwEquals <> line <> rhs

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
    funDefTypeSig' <- ppCode (f ^. funDefType)
    clauses' <- mapM (ppClause funDefName') (f ^. funDefClauses)
    return $
      funDefName'
        <+> kwColonColon
        <+> funDefTypeSig'
          <> line
          <> vsep (toList clauses')
    where
      ppClause :: Member (Reader Options) r => Doc Ann -> FunctionClause -> Sem r (Doc Ann)
      ppClause fun c = do
        clausePatterns' <- mapM ppCodeAtom (c ^. clausePatterns)
        clauseBody' <- ppCode (c ^. clauseBody)
        return $ fun <+> hsep clausePatterns' <+> kwEquals <+> clauseBody'

instance PrettyCode Statement where
  ppCode = \case
    StatementFunction f -> ppCode f
    StatementInductive d -> ppCode d
    StatementVerbatim t -> return (pretty t)

instance PrettyCode ModuleBody where
  ppCode m = do
    statements' <- mapM ppCode (m ^. moduleStatements)
    return $ PP.vsep2 statements'

instance PrettyCode Literal where
  ppCode = \case
    LitInteger n -> return $ annotate AnnLiteralInteger (pretty n)
    LitString s -> return $ ppStringLit s

doubleQuotes :: Doc Ann -> Doc Ann
doubleQuotes = enclose kwDQuote kwDQuote

kwDQuote :: Doc Ann
kwDQuote = pretty ("\"" :: Text)

ppStringLit :: Text -> Doc Ann
ppStringLit = annotate AnnLiteralString . doubleQuotes . pretty

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
