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

ppParams :: (HasAtomicity c, PrettyCode c, Member (Reader Options) r) => [c] -> Sem r (Maybe (Doc Ann))
ppParams = \case
  [] -> return Nothing
  [x] -> Just <$> ppRightExpression appFixity x
  params -> do
    ps <- mapM ppCode params
    return $ Just $ parens (hsep (punctuate comma ps))

instance PrettyCode Name where
  ppCode = return . prettyName False

instance PrettyCode Type where
  ppCode = \case
    TyVar v -> ppCode v
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

instance PrettyCode Inductive where
  ppCode = \case
    IndBool -> return $ primitive "bool"
    IndNat -> return $ primitive "nat"
    IndInt -> return $ primitive "int"
    IndUser name -> ppCode name

instance PrettyCode IndApp where
  ppCode IndApp {..} = do
    params <- ppParams _indAppParams
    ind <- ppCode _indAppInductive
    return $ params <?+> ind

instance PrettyCode Statement where
  ppCode = \case
    StmtDefinition x -> ppCode x
    StmtFunction x -> ppCode x
    StmtSynonym x -> ppCode x
    StmtDatatype x -> ppCode x
    StmtRecord x -> ppCode x

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

instance PrettyCode Synonym where
  ppCode Synonym {..} = do
    n <- ppCode _synonymName
    ty <- ppCodeQuoted _synonymType
    return $ kwTypeSynonym <+> n <+> "=" <+> ty

instance PrettyCode Datatype where
  ppCode Datatype {..} = do
    n <- ppCode _datatypeName
    params <- ppParams _datatypeParams
    ctrs <- mapM ppCode _datatypeConstructors
    return $ kwDatatype <+> params <?+> n <> line <> indent' ("=" <+> vsep (punctuate "|" ctrs))

instance PrettyCode Constructor where
  ppCode Constructor {..} = do
    n <- ppCode _constructorName
    tys <- mapM ppCodeQuoted _constructorArgTypes
    return $ hsep (n : tys)

instance PrettyCode Record where
  ppCode Record {..} = do
    n <- ppCode _recordName
    params <- ppParams _recordParams
    fields <- mapM ppCode _recordFields
    return $ kwRecord <+> params <?+> n <+> "=" <> line <> indent' (vsep fields)

instance PrettyCode RecordField where
  ppCode RecordField {..} = do
    n <- ppCode _recordFieldName
    ty <- ppCodeQuoted _recordFieldType
    return $ n <+> "::" <+> ty

instance PrettyCode Theory where
  ppCode Theory {..} = do
    n <- ppCode _theoryName
    stmts <- mapM ppCode _theoryStatements
    return $
      kwTheory
        <+> n
          <> line
          <> kwImports
        <+> "Main"
          <> line
          <> kwBegin
          <> line
          <> line
          <> vsep (punctuate line stmts)
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
