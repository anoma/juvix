module Juvix.Compiler.Concrete.Print.Base where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language

import Juvix.Compiler.Concrete.Pretty.Base qualified as P
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.CodeAnn (Ann)
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.Keyword.All
import Juvix.Prelude.Base hiding ((<+>))
import Juvix.Prelude.Path
import Juvix.Prelude.Pretty (pretty)

class PrettyPrint a where
  ppCode :: Members '[ExactPrint, Reader Options] r => a -> Sem r ()

instance PrettyPrint Keyword where
  ppCode = noLoc . pretty

instance PrettyPrint KeywordRef where
  ppCode = ppMorpheme

instance (SingI s) => PrettyPrint (Judoc s) where
  ppCode = P.ppCode >=> noLoc

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

instance (SingI s, SingI t) => PrettyPrint (Module s t) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Module s t -> Sem r ()
  ppCode Module {..} = do
    let moduleBody' = indent (ppCode _moduleBody)
        modulePath' = ppModulePathType _modulePath
        moduleDoc' :: Sem r () = maybe (return ()) ppCode _moduleDoc
    -- return $
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

instance SingI s => PrettyPrint [Statement s] where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => [Statement s] -> Sem r ()
  ppCode ss = vsep2 (map ppGroup (P.groupStatements ss))
    where
      ppGroup :: [Statement s] -> Sem r ()
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

instance SingI s => PrettyPrint (Import s) where
  ppCode :: Members '[ExactPrint, Reader Options] r => Import s -> Sem r ()
  ppCode i = do
    ppCode (i ^. importKw)
      <+> ppImportType
    where
      ppImportType = case sing :: SStage s of
        SParsed -> ppCode (i ^. importModule)
        SScoped -> ppCode (i ^. importModule)

instance SingI s => PrettyPrint (Statement s) where
  ppCode = \case
    StatementOperator {} -> todo
    StatementTypeSignature {} -> todo
    StatementImport i -> ppCode i
    StatementInductive {} -> todo
    StatementModule {} -> todo
    StatementOpenModule {} -> todo
    StatementFunctionClause {} -> todo
    StatementAxiom {} -> todo
    StatementForeign {} -> todo
    StatementCompile {} -> todo

todo :: a
todo = error "todo"
