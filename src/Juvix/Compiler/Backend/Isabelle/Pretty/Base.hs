module Juvix.Compiler.Backend.Isabelle.Pretty.Base where

import Juvix.Compiler.Backend.Isabelle.Language
import Juvix.Compiler.Backend.Isabelle.Pretty.Keywords
import Juvix.Compiler.Backend.Isabelle.Pretty.Options
import Juvix.Data.CodeAnn

arrow :: Doc Ann
arrow = "⇒"

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts = run . runReader opts . ppCode

ppCodeQuoted :: (HasAtomicity c, PrettyCode c, Member (Reader Options) r) => c -> Sem r (Doc Ann)
ppCodeQuoted c
  | atomicity c == Atom = ppCode c
  | otherwise = dquotes <$> ppCode c

instance PrettyCode Name where
  ppCode = return . prettyName False

instance PrettyCode Type where
  ppCode = \case
    TyVar v -> ppCode v
    TyBool -> return $ primitive "bool"
    TyNat -> return $ primitive "nat"
    TyInt -> return $ primitive "int"
    TyFun x -> ppCode x
    TyInd x -> ppCode x

instance PrettyCode Var where
  ppCode Var {..} =
    (squote <>) <$> ppCode _varName

instance PrettyCode FunType where
  ppCode FunType {..} = do
    l <- ppLeftExpression funFixity _funTypeLeft
    r <- ppRightExpression funFixity _funTypeRight
    return $ l <+> arrow <+> r

instance PrettyCode IndApp where
  ppCode IndApp {..} = do
    params <- mapM ppCode _indAppParams
    ind <- ppCode _indAppInductive
    return $ hsep (params ++ [ind])

instance PrettyCode Statement where
  ppCode = \case
    StmtDefinition x -> ppCode x
    StmtFunction x -> ppCode x
    StmtDatatype x -> ppCode x
    StmtSynonym x -> ppCode x

instance PrettyCode Definition where
  ppCode Definition {..} = do
    n <- ppCode _definitionName
    ty <- ppCodeQuoted _definitionType
    return $ kwDefinition <+> n <+> "::" <+> ty <+> kwWhere <> line <> dquotes (n <+> "=" <+> kwUndefined)

instance PrettyCode Function where
  ppCode Function {..} = do
    n <- ppCode _functionName
    ty <- ppCodeQuoted _functionType
    return $ kwFun <+> n <+> "::" <+> ty <+> kwWhere <> line <> dquotes (n <+> "_" <+> "=" <+> kwUndefined)

instance PrettyCode Datatype where
  ppCode Datatype {..} = do
    n <- ppCode _datatypeName
    params <- mapM ppCode _datatypeParams
    ctrs <- mapM ppCode _datatypeConstructors
    return $ kwDatatype <+> hsep (params ++ [n]) <> line <> "=" <+> vsep (punctuate "|" ctrs)

instance PrettyCode Constructor where
  ppCode Constructor {..} = do
    n <- ppCode _constructorName
    tys <- mapM ppCodeQuoted _constructorArgTypes
    return $ hsep (n : tys)

instance PrettyCode Synonym where
  ppCode Synonym {..} = do
    n <- ppCode _synonymName
    ty <- ppCodeQuoted _synonymType
    return $ kwTypeSynonym <+> n <+> "=" <+> ty

instance PrettyCode Theory where
  ppCode Theory {..} = do
    n <- ppCode _theoryName
    imps <- mapM ppCode _theoryImports
    stmts <- mapM ppCode _theoryStatements
    return $
      kwTheory
        <+> n
          <> line
          <> kwImports
        <+> hsep ("Main" : imps)
          <> line
          <> kwBegin
          <> line
          <> line
          <> vsep stmts
          <> line
          <> line
          <> kwEnd

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
