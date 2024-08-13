module Juvix.Compiler.Backend.Isabelle.Pretty.Base where

import Data.Text qualified as Text
import Juvix.Compiler.Backend.Isabelle.Language
import Juvix.Compiler.Backend.Isabelle.Pretty.Keywords
import Juvix.Compiler.Backend.Isabelle.Pretty.Options
import Juvix.Data.CodeAnn

arrow :: Doc Ann
arrow = "\\<Rightarrow>"

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts = run . runReader opts . ppCode

ppCodeQuoted :: (HasAtomicity c, PrettyCode c, Member (Reader Options) r) => c -> Sem r (Doc Ann)
ppCodeQuoted c
  | atomicity c == Atom = ppCode c
  | otherwise = dquotes <$> ppCode c

ppTopCode :: (HasAtomicity c, PrettyCode c, Member (Reader Options) r) => c -> Sem r (Doc Ann)
ppTopCode c = parensIf (not (isAtomic c)) <$> ppCode c

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

instance PrettyCode TypeVar where
  ppCode TypeVar {..} =
    (squote <>) <$> ppCode _typeVarName

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
    IndList -> return $ primitive "list"
    IndString -> return $ primitive "string"
    IndOption -> return $ primitive "option"
    IndTuple -> return $ primitive "\\<times>"
    IndUser name -> ppCode name

instance PrettyCode IndApp where
  ppCode IndApp {..}
    | _indAppInductive == IndTuple = do
        ps <- mapM ppCode _indAppParams
        ind <- ppCode _indAppInductive
        return $ hsep $ punctuate (space <> ind) ps
    | otherwise = do
        params <- ppParams _indAppParams
        ind <- ppCode _indAppInductive
        return $ params <?+> ind

instance PrettyCode Expression where
  ppCode = \case
    ExprIden x -> ppCode x
    ExprUndefined -> return kwUndefined
    ExprLiteral x -> ppCode x
    ExprApp x -> ppCode x
    ExprBinop x -> ppCode x
    ExprTuple x -> ppCode x
    ExprList x -> ppCode x
    ExprCons x -> ppCode x
    ExprLet x -> ppCode x
    ExprIf x -> ppCode x
    ExprCase x -> ppCode x
    ExprLambda x -> ppCode x

instance PrettyCode Literal where
  ppCode = \case
    LitNumeric x -> return $ annotate AnnLiteralInteger (pretty x)
    LitString x -> return $ annotate AnnLiteralString $ squotes $ squotes $ pretty x

instance PrettyCode Application where
  ppCode Application {..} = do
    l <- ppLeftExpression appFixity _appLeft
    r <- ppRightExpression appFixity _appRight
    return $ l <+> r

instance PrettyCode Binop where
  ppCode Binop {..} = do
    op <- ppCode _binopOperator
    l <- ppLeftExpression _binopFixity _binopLeft
    r <- ppRightExpression _binopFixity _binopRight
    return $ l <+> op <+> r

instance PrettyCode LetClause where
  ppCode LetClause {..} = do
    name <- ppCode _letClauseName
    val <- ppCode _letClauseValue
    return $ name <+> "=" <+> align val

instance PrettyCode Let where
  ppCode Let {..} = do
    cls <- mapM ppCode _letClauses
    body <- ppCode _letBody
    return $ align $ kwLet <> blockIndent (vsep (punctuate semi (toList cls))) <> kwIn <+> body

instance PrettyCode If where
  ppCode If {..} = do
    val <- ppCode _ifValue
    br1 <- ppCode _ifBranchTrue
    br2 <- ppCode _ifBranchFalse
    return $ align $ kwIf <> oneLineOrNextBlock val <> kwThen <> oneLineOrNextBlock br1 <> kwElse <> oneLineOrNext br2

instance PrettyCode Case where
  ppCode Case {..} = do
    val <- ppCode _caseValue
    brs <- toList <$> mapM ppCode _caseBranches
    let brs' = punctuate (space <> kwPipe) brs
    return $ align $ kwCase <> oneLineOrNextBlock val <> kwOf <> hardline <> indent' (vsepHard brs')

instance PrettyCode CaseBranch where
  ppCode CaseBranch {..} = do
    pat <- ppTopCode _caseBranchPattern
    body <- ppRightExpression caseFixity _caseBranchBody
    return $ pat <+> arrow <> oneLineOrNext body

instance (PrettyCode a) => PrettyCode (Tuple a) where
  ppCode Tuple {..} = do
    elems <- mapM ppCode _tupleComponents
    return $ parens $ hsep (punctuate comma (toList elems))

instance (PrettyCode a) => PrettyCode (List a) where
  ppCode List {..} = do
    elems <- mapM ppCode _listElements
    return $ brackets $ hsep (punctuate comma (toList elems))

instance (PrettyCode a, HasAtomicity a) => PrettyCode (Cons a) where
  ppCode Cons {..} = do
    h <- ppLeftExpression consFixity _consHead
    t <- ppRightExpression consFixity _consTail
    return $ h <+> "#" <+> t

instance PrettyCode Pattern where
  ppCode = \case
    PatVar x -> ppCode x
    PatZero -> return $ annotate AnnLiteralInteger (pretty (0 :: Int))
    PatConstrApp x -> ppCode x
    PatTuple x -> ppCode x
    PatList x -> ppCode x
    PatCons x -> ppCode x

instance PrettyCode ConstrApp where
  ppCode ConstrApp {..} = do
    args <- mapM ppCode _constrAppArgs
    name <- ppCode _constrAppConstructor
    if
        | null args ->
            return name
        | otherwise ->
            return $ name <+> hsep args

instance PrettyCode Lambda where
  ppCode Lambda {..} = do
    name <- ppCode _lambdaVar
    mty <- maybe (return Nothing) (ppCode >=> return . Just) _lambdaType
    body <- ppCode _lambdaBody
    let ty = fmap (\t -> colon <> colon <+> t) mty
    return $ "\\<lambda>" <+> name <+?> ty <+> dot <+> body

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
    body <- ppCode _definitionBody
    return $ kwDefinition <+> n <+> "::" <+> ty <+> kwWhere <> line <> indent' (dquotes (n <+> "=" <> oneLineOrNext body))

instance PrettyCode Function where
  ppCode Function {..} = do
    n <- ppCode _functionName
    ty <- ppCodeQuoted _functionType
    cls <- mapM ppCode _functionClauses
    let cls' = punctuate (space <> kwPipe) $ map (dquotes . (n <+>)) (toList cls)
    return $ kwFun <+> n <+> "::" <+> ty <+> kwWhere <> line <> indent' (vsep cls')

instance PrettyCode Clause where
  ppCode Clause {..} = do
    pats <- mapM ppTopCode _clausePatterns
    body <- ppTopCode _clauseBody
    return $ hsep pats <+> "=" <> oneLineOrNext body

instance PrettyCode Synonym where
  ppCode Synonym {..} = do
    n <- ppCode _synonymName
    ty <- ppCodeQuoted _synonymType
    return $ kwTypeSynonym <+> n <+> "=" <> oneLineOrNext ty

instance PrettyCode Datatype where
  ppCode Datatype {..} = do
    n <- ppCode _datatypeName
    params <- ppParams _datatypeParams
    ctrs <- mapM ppCode _datatypeConstructors
    return $ kwDatatype <+> params <?+> n <> line <> indent' ("=" <+> align (vsep (punctuate (space <> kwPipe) ctrs)))

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

ppImports :: [Name] -> Sem r [Doc Ann]
ppImports ns =
  return $ map (dquotes . pretty) $ filter (not . Text.isPrefixOf "Stdlib/") $ map (Text.replace "." "/" . (^. namePretty)) ns

instance PrettyCode Theory where
  ppCode Theory {..} = do
    n <- ppCode _theoryName
    imports <- ppImports _theoryImports
    stmts <- mapM ppCode _theoryStatements
    return $
      kwTheory
        <+> n
          <> line
          <> kwImports
        <+> align (vsep ("Main" : imports))
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
