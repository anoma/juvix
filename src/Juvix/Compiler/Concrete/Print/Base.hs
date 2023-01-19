module Juvix.Compiler.Concrete.Print.Base where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Base qualified as P
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.CodeAnn (CodeAnn(..),Ann)
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.Keyword.All
import Juvix.Prelude.Base hiding ((<+>), (?<>), (<?+>))
import Juvix.Prelude.Path
import Juvix.Prelude.Pretty (pretty,annotate)

class PrettyPrint a where
  ppCode :: Members '[ExactPrint, Reader Options] r => a -> Sem r ()

instance PrettyPrint Keyword where
  ppCode = noLoc . pretty

instance PrettyPrint KeywordRef where
  ppCode = ppMorpheme

doc :: (PrettyPrint c, HasLoc c) => Options -> Comments -> c -> Doc Ann
doc opts cs x =
  run
    . execExactPrint (fileComments file cs)
    . runReader opts
    . ppCode
    $ x
  where
    file :: Path Abs File
    file = getLoc x ^. intervalFile

ppModulePathType ::
  forall t s r.
  (SingI t, SingI s, Members '[ExactPrint, Reader Options] r) =>
  ModulePathType s t ->
  Sem r ()
ppModulePathType x = case sing :: SStage s of
  SParsed -> case sing :: SModuleIsTop t of
    SModuleLocal -> noLoc (pretty x)
    SModuleTop -> ppCode x
  SScoped -> case sing :: SModuleIsTop t of
    SModuleLocal -> P.ppCode x >>= morpheme (getLoc x) . P.annSDef x
    SModuleTop -> P.ppCode x >>= morpheme (getLoc x) . P.annSDef x

instance (SingI t) => PrettyPrint (Module 'Scoped t) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Module 'Scoped t -> Sem r ()
  ppCode Module {..} = do
    let moduleBody' = indent (ppCode _moduleBody)
        modulePath' = ppModulePathType _modulePath
        moduleDoc' :: Sem r () = maybe (return ()) ppCode _moduleDoc
    moduleDoc'
      <> ppCode _moduleKw
      <+> modulePath'
        <> ppCode kwSemicolon
        <> line
        <> moduleBody'
        <> line
        <> ppCode kwEnd
        <> lastSemicolon
    where
      lastSemicolon :: Sem r ()
      lastSemicolon = case sing :: SModuleIsTop t of
        SModuleLocal -> return ()
        SModuleTop -> semicolon

instance PrettyPrint [Statement 'Scoped] where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => [Statement 'Scoped] -> Sem r ()
  ppCode ss = vsep2 (map ppGroup (P.groupStatements ss))
    where
      ppGroup :: [Statement 'Scoped] -> Sem r ()
      ppGroup = vsep . endSemicolon . map ppCode

instance PrettyPrint TopModulePath where
  ppCode t@TopModulePath {..} =
    mapM P.ppSymbol (_modulePathDir ++ [_modulePathName]) >>= morpheme (getLoc t) . P.dotted

instance PrettyPrint c => PrettyPrint (WithLoc c) where
  ppCode = ppCode . (^. withLocParam)

instance (HasLoc n, P.PrettyCode n) => PrettyPrint (S.Name' n) where
  ppCode = ppMorpheme

instance PrettyPrint Name where
  ppCode n = case n of
    NameUnqualified s -> ppMorpheme s
    NameQualified s -> ppCode s

instance PrettyPrint QualifiedName where
  ppCode :: Members '[ExactPrint, Reader Options] r => QualifiedName -> Sem r ()
  ppCode q@QualifiedName {..} = do
    let symbols = _qualifiedPath ^. pathParts NonEmpty.|> _qualifiedSymbol
    str <- P.dotted <$> mapM P.ppSymbol symbols
    morpheme (getLoc q) str

ppMorpheme :: (Members '[ExactPrint, Reader Options] r, P.PrettyCode c, HasLoc c) => c -> Sem r ()
ppMorpheme n = P.ppCode n >>= morpheme (getLoc n)

instance PrettyPrint (ModuleRef'' 'S.Concrete 'ModuleTop) where
  ppCode m = ppCode (m ^. moduleRefName)

instance PrettyPrint (Import 'Scoped) where
  ppCode :: Members '[ExactPrint, Reader Options] r => Import 'Scoped -> Sem r ()
  ppCode i = do
    ppCode (i ^. importKw)
      <+> ppCode (i ^. importModule)

instance PrettyPrint OperatorSyntaxDef where
  ppCode OperatorSyntaxDef {..} = do
    opSymbol' <- P.ppUnkindedSymbol _opSymbol
    fi
      <+> morpheme (getLoc _opSymbol) opSymbol'
   where
   fi = do
     p <- P.ppCode (_opFixity ^. fixityPrecedence)
     ppCode _opKw <+> noLoc p

instance PrettyPrint Expression where
  ppCode = ppMorpheme

instance PrettyPrint (Example 'Scoped) where
  ppCode e =
    noLoc P.ppJudocStart
     <+> noLoc P.ppJudocExampleStart
     <+> ppCode (e ^. exampleExpression)
     <> noLoc P.kwSemicolon
     <> line

instance PrettyPrint (JudocParagraphLine 'Scoped) where
  ppCode = ppMorpheme

instance PrettyPrint (Judoc 'Scoped) where
  ppCode (Judoc blocks) = mconcatMapM ppCode blocks

instance PrettyPrint (JudocBlock 'Scoped) where
  ppCode = \case
    JudocParagraph l -> vsep (ppCode <$> l)
    JudocExample e -> ppCode e

instance PrettyPrint (JudocAtom 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => JudocAtom 'Scoped -> Sem r ()
  ppCode = \case
    JudocExpression e -> semiDelim (ppCode e)
    JudocText t -> noLoc (annotate AnnComment (pretty t))
    where
      semiDelim :: Sem r () -> Sem r ()
      semiDelim x = semi >> x >> semi
        where
        semi :: Sem r ()
        semi = noLoc (annotate AnnComment (pretty @Text ";"))

instance PrettyPrint (AxiomDef 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => AxiomDef 'Scoped -> Sem r ()
  ppCode AxiomDef {..} = do
    axiomName' <- P.annDef _axiomName <$> P.ppSymbol _axiomName
    let builtin' :: Maybe (Sem r ()) = (\x -> P.ppCode x >>= morpheme (getLoc x)) <$> _axiomBuiltin
        _axiomDoc' :: Maybe (Sem r ()) =  ppCode <$> _axiomDoc
    _axiomDoc'
      ?<> builtin'
      <?+> hang
        (ppCode _axiomKw
         <+> morpheme (getLoc _axiomName) axiomName'
         <+> noLoc P.kwColon
         <+> ppCode _axiomType)

instance PrettyPrint (Statement 'Scoped) where
  ppCode = \case
    StatementOperator o -> ppCode o
    StatementTypeSignature {} -> todo
    StatementImport i -> ppCode i
    StatementInductive {} -> todo
    StatementModule {} -> todo
    StatementOpenModule {} -> todo
    StatementFunctionClause {} -> todo
    StatementAxiom a -> ppCode a
    StatementForeign {} -> todo
    StatementCompile {} -> todo

todo :: a
todo = error "todo"
