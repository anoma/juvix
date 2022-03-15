-- TODO handle capital letters and characters not supported by Haskell.
module MiniJuvix.Syntax.MiniHaskell.Pretty.Base where


import MiniJuvix.Prelude
import Prettyprinter
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
  ppCode :: Member (Reader Options) r => Name -> Sem r (Doc Ann)
  ppCode n =
    return $ annotate (AnnKind (n ^. nameKind))
           $ pretty (n ^. nameText) <> "_" <> pretty (n ^. nameId)

instance PrettyCode Iden where
  ppCode :: Member (Reader Options) r => Iden -> Sem r (Doc Ann)
  ppCode i = case i of
   IdenDefined na -> ppCode na
   IdenConstructor na -> ppCode na
   IdenVar na -> ppCode na

-- TODO optimize parentheses.
instance PrettyCode Application where
  ppCode a = do
    l <- ppCode (a ^. appLeft)
    r <- ppCode (a ^. appRight)
    return $ l <+> parens r

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

-- [a -> b -> c] -> d
-- TODO optimize parentheses.
instance PrettyCode Function where
  ppCode f = do
    funParameters' <- mapM ppCode (f ^. funParameters)
    funReturn' <- ppCode (f ^. funReturn)
    return $ concatWith (\a b -> parens a <+> kwArrow <+> parens b)
     (toList funParameters' ++ [funReturn'])

instance PrettyCode Type where
  ppCode t = case t of
    TypeIden (TypeIdenInductive n) -> ppCode n
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
    params' <- mapM ppCode (c ^. constrAppParameters)
    return $ parens (hsep $ constr' : params')

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
       clausePatterns' <- mapM ppCode (c ^. clausePatterns)
       clauseBody' <- ppCode (c ^. clauseBody)
       return $ fun <+> hsep clausePatterns' <+> kwEquals <+> clauseBody'

instance PrettyCode ModuleBody where
  ppCode m = do
    types' <- mapM ppCode (toList (m ^. moduleInductives))
    funs' <- mapM ppCode (toList (m ^. moduleFunctions))
    return $ vsep types' <> line <> vsep funs'

instance PrettyCode Module where
  ppCode m = do
    name' <- ppCode (m ^. moduleName)
    body' <- ppCode (m ^. moduleBody)
    return $ kwModule <+> name' <+> kwWhere <> line <> body'
