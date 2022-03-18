-- TODO handle capital letters and characters not supported by Haskell.
module MiniJuvix.Syntax.MiniHaskell.Pretty.Base where


import MiniJuvix.Prelude
import Prettyprinter
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.MiniHaskell.Pretty.Ann
import MiniJuvix.Syntax.MiniHaskell.Language
import qualified MiniJuvix.Internal.Strings as Str

newtype Options = Options
  {
    _optIndent :: Int
  }

defaultOptions :: Options
defaultOptions = Options {
  _optIndent = 2
  }

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

instance PrettyCode Name where
  ppCode n =
    return $ annotate (AnnKind (n ^. nameKind))
           $ pretty (n ^. nameText)

instance PrettyCode Application where
  ppCode a = do
    l' <- ppLeftExpression appFixity (a ^. appLeft)
    r' <- ppRightExpression appFixity (a ^. appRight)
    return $ l' <+> r'

instance PrettyCode Expression where
  ppCode e = case e of
    ExpressionIden i -> ppCode i
    ExpressionApplication a -> ppCode a

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

instance PrettyCode Type where
  ppCode t = case t of
    TypeIden n -> ppCode n
    TypeFunction f -> ppCode f

instance PrettyCode InductiveConstructorDef where
  ppCode c = do
    constructorName' <- ppCode (c ^. constructorName)
    constructorParameters' <- mapM ppCode (c ^. constructorParameters)
    return (hsep $ constructorName' : constructorParameters')

indent' :: Member (Reader Options) r => Doc a -> Sem r (Doc a)
indent' d = do
  i <- asks _optIndent
  return $ indent i d

instance PrettyCode InductiveDef where
  ppCode d = do
    inductiveName' <- ppCode (d ^. inductiveName)
    inductiveConstructors' <- mapM ppCode (d ^. inductiveConstructors)
    rhs <- indent' $ align $ concatWith (\a b -> a <> line <> kwPipe <+> b) inductiveConstructors'
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
    funDefTypeSig' <- ppCode (f ^. funDefTypeSig)
    clauses' <- mapM (ppClause funDefName') (f ^. funDefClauses)
    return $ funDefName' <+> kwColonColon <+> funDefTypeSig' <> line
      <> vsep (toList clauses')
     where
     ppClause fun c = do
       clausePatterns' <- mapM ppCodeAtom (c ^. clausePatterns)
       clauseBody' <- ppCode (c ^. clauseBody)
       return $ fun <+> hsep clausePatterns' <+> kwEquals <+> clauseBody'

instance PrettyCode Statement where
  ppCode = \case
    StatementFunctionDef f -> ppCode f
    StatementInductiveDef d -> ppCode d

instance PrettyCode ModuleBody where
  ppCode m = do
    statements' <- mapM ppCode (m ^. moduleStatements)
    return $ vsep2 statements'
    where
    vsep2 = concatWith (\a b -> a <> line <> line <> b)

instance PrettyCode Module where
  ppCode m = do
    name' <- ppCode (m ^. moduleName)
    body' <- ppCode (m ^. moduleBody)
    return $ kwModule <+> name' <+> kwWhere
      <> line <> line <> body' <> line

parensCond :: Bool -> Doc Ann -> Doc Ann
parensCond t d = if t then parens d else d

ppPostExpression ::(PrettyCode a, HasAtomicity a, Member (Reader Options) r)  =>
  Fixity -> a -> Sem r (Doc Ann)
ppPostExpression = ppLRExpression isPostfixAssoc

ppRightExpression :: (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity -> a -> Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression :: (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity -> a -> Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression
  :: (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
     (Fixity -> Bool) -> Fixity -> a -> Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensCond (atomParens associates (atomicity e) fixlr)
      <$> ppCode e

ppCodeAtom :: (HasAtomicity c, PrettyCode c, Members '[Reader Options] r) => c -> Sem r (Doc Ann)
ppCodeAtom c = do
  p' <- ppCode c
  return $ if isAtomic c then p' else parens p'
