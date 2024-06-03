module Juvix.Compiler.Backend.Rust.Pretty.Base where

import Juvix.Compiler.Backend.Rust.Language
import Juvix.Compiler.Backend.Rust.Pretty.Keywords
import Juvix.Compiler.Backend.Rust.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Prelude

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts x =
  run $
    runReader opts $
      ppCode x

ampersand :: Doc Ann
ampersand = "&"

hashsym :: Doc Ann
hashsym = "#"

ppName :: NameKind -> Text -> Sem r (Doc Ann)
ppName k n = return $ annotate (AnnKind k) (pretty n)

ppMut :: IsMut -> Sem r (Maybe (Doc Ann))
ppMut = \case
  Mut -> return $ Just kwMut
  NotMut -> return Nothing

ppAttrs :: Maybe Text -> Sem r (Doc Ann)
ppAttrs = \case
  Just attrs -> return $ hashsym <> brackets (pretty attrs) <> line
  Nothing -> return mempty

ppBlock :: (Member (Reader Options) r) => [Statement] -> Sem r (Doc Ann)
ppBlock stmts = do
  stmts' <- mapM ppCode stmts
  let stmts'' = punctuate semi stmts'
  return $ oneLineOrNextBraces (vsep stmts'')

instance PrettyCode Type where
  ppCode = \case
    Word -> return kwWord
    VecOfWord -> return $ kwVector <> angles kwWord
    Memory -> return $ ampersand <> kwMut <+> kwMemory

instance PrettyCode FunctionArgument where
  ppCode FunctionArgument {..} = do
    ty <- ppCode _functionArgumentType
    n <- ppName KNameLocal _functionArgumentName
    mut <- ppMut _functionArgumentMutable
    return $ mut <?+> n <> colon <+> ty

instance PrettyCode Function where
  ppCode Function {..} = do
    attrs <- ppAttrs _functionAttributes
    name <- ppName KNameFunction _functionName
    args <- mapM ppCode _functionArguments
    rty <- maybe (return Nothing) (ppCode >=> return . Just . ("->" <+>)) _functionReturnType
    body <- ppBlock _functionBody
    let args' = punctuate comma args
    return $ attrs <> kwFn <+> name <> parens (hsep args') <+> rty <?+> body <> line

instance PrettyCode Statement where
  ppCode = \case
    StatementLet x -> ppCode x
    StatementConst x -> ppCode x
    StatementAssignment x -> ppCode x
    StatementIf x -> ppCode x
    StatementMatch x -> ppCode x
    StatementLoop x -> ppCode x
    StatementContinue -> return kwContinue
    StatementReturn x -> ppCode x
    StatementExpression x -> ppCode x

instance PrettyCode Let where
  ppCode Let {..} = do
    name <- ppName KNameLocal _letVariable
    ty <- maybe (return Nothing) (ppCode >=> return . Just) _letType
    mut <- ppMut _letMutable
    ini <- maybe (return Nothing) (ppCode >=> return . Just) _letInitializer
    let ini' = fmap ("=" <+>) ini
        ty' = fmap (colon <+>) ty
    return $ kwLet <+> mut <?+> (name <>? ty' <+?> ini')

instance PrettyCode ConstDecl where
  ppCode ConstDecl {..} = do
    name <- ppName KNameLocal _constVariable
    ty <- ppCode _constType
    val <- ppCode _constValue
    return $ kwConst <+> name <> colon <+> ty <+> "=" <+> val

instance PrettyCode Assignment where
  ppCode Assignment {..} = do
    name <- ppName KNameLocal _assignmentVariable
    val <- ppCode _assignmentValue
    return $ name <+> "=" <+> val

instance PrettyCode If where
  ppCode If {..} = do
    val <- ppCode _ifValue
    br1 <- ppBlock _ifBranchTrue
    br2 <- ppBlock _ifBranchFalse
    return $ kwIf <+> val <+> br1 <+> kwElse <+> br2

instance PrettyCode MatchBranch where
  ppCode MatchBranch {..} = do
    pat <- case _matchBranchPattern of
      Just p -> ppCode p
      Nothing -> return "_"
    body <- ppBlock _matchBranchBody
    return $ pat <+> "=>" <+> body

instance PrettyCode Match where
  ppCode Match {..} = do
    val <- ppCode _matchValue
    brs <- mapM ppCode _matchBranches
    return $ kwMatch <+> val <+> oneLineOrNextBraces (vsep brs)

instance PrettyCode Loop where
  ppCode Loop {..} = do
    let lab = fmap ((<> colon) . pretty) _loopLabel
    body <- ppBlock _loopBody
    return $ lab <?+> kwLoop <+> body

instance PrettyCode Return where
  ppCode Return {..} = do
    val <- maybe (return Nothing) (ppCode >=> return . Just) _returnValue
    return $ kwReturn <+?> val

instance PrettyCode Expression where
  ppCode = \case
    ExprVar x -> ppCode x
    ExprCall x -> ppCode x
    ExprVec x -> ppCode x
    ExprArray x -> ppCode x
    ExprLiteral x -> ppCode x
    ExprBlock x -> ppCode x
    ExprVerbatim x -> return $ pretty x

instance PrettyCode Var where
  ppCode Var {..} = ppName KNameLocal _varName

instance PrettyCode Call where
  ppCode Call {..} = do
    name <- ppName KNameFunction _callFunction
    args <- mapM ppCode _callArgs
    return $ name <> parens (hsep (punctuate comma args))

instance PrettyCode Vec where
  ppCode Vec {..} = do
    args <- mapM ppCode _vecArgs
    return $ kwVec <> brackets (hsep (punctuate comma args))

instance PrettyCode Array where
  ppCode Array {..} = do
    args <- mapM ppCode _arrayArgs
    return $ ampersand <> brackets (hsep (punctuate comma args))

instance PrettyCode Literal where
  ppCode = \case
    LitInteger i -> return $ annotate AnnLiteralInteger (pretty i)
    LitString s -> return $ annotate AnnLiteralString (show s)

instance PrettyCode Block where
  ppCode Block {..} = ppBlock _blockBody

instance PrettyCode Program where
  ppCode Program {..} = do
    funs <- mapM ppCode _programFunctions
    return $ pretty prelude <> line <> vsep funs
    where
      prelude :: Text
      prelude =
        "extern crate juvix;\n\
        \\n\
        \#[allow(unused_imports)]\n\
        \use juvix::defs::*;\n\
        \#[allow(unused_imports)]\n\
        \use juvix::apply;\n\
        \#[allow(unused_imports)]\n\
        \use juvix::tapply;\n\
        \#[allow(unused_imports)]\n\
        \use juvix::equality::*;\n\
        \#[allow(unused_imports)]\n\
        \use juvix::integer::*;\n\
        \#[allow(unused_imports)]\n\
        \use juvix::memory::*;\n"
