module Juvix.Compiler.Concrete.Print.Base where

import Data.List.NonEmpty.Extra qualified as NonEmpty
-- import Data.Text qualified as T
-- import Juvix.Compiler.Concrete.Data.ScopedName (AbsModulePath)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
-- import Juvix.Compiler.Concrete.Extra (unfoldApplication)
import Juvix.Compiler.Concrete.Language
-- import Juvix.Extra.Strings qualified as Str

import Juvix.Compiler.Concrete.Pretty.Base qualified as P
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.CodeAnn (Ann)
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.Keyword.All
import Juvix.Prelude.Base hiding ((<+>))
import Juvix.Prelude.Path
import Juvix.Prelude.Pretty (pretty)

class PrettyPrint a where
  ppCode :: Members '[ExactPrint Ann, Reader Options] r => a -> Sem r ()

instance PrettyPrint Keyword where
  ppCode = noLoc @Ann . pretty

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
  (SingI t, SingI s, Members '[ExactPrint Ann, Reader Options] r) =>
  ModulePathType s t ->
  Sem r ()
ppModulePathType x = case sing :: SStage s of
  SParsed -> case sing :: SModuleIsTop t of
    SModuleLocal -> noLoc @Ann (pretty x)
    SModuleTop -> ppCode x
  SScoped -> case sing :: SModuleIsTop t of
    SModuleLocal -> P.ppCode x >>= morpheme (getLoc x) . P.annSDef x
    SModuleTop -> P.ppCode x >>= morpheme (getLoc x) . P.annSDef x

instance (SingI s, SingI t) => PrettyPrint (Module s t) where
  ppCode :: forall r. (Members '[ExactPrint Ann, Reader Options] r) => Module s t -> Sem r ()
  ppCode Module {..} = do
    let moduleBody' = indent (Proxy @Ann) (ppCode _moduleBody)
        modulePath' = ppModulePathType _modulePath
        moduleDoc' :: Sem r () = maybe (return ()) ppCode _moduleDoc
    -- return $
    moduleDoc'
      <> ppCode kwModule
      <+> modulePath'
        <> ppCode kwSemicolon
        <> line (Proxy @Ann)
        <> moduleBody'
        <> line (Proxy @Ann)
        <> ppCode kwEnd
        <> lastSemicolon
    where
      lastSemicolon :: Sem r ()
      lastSemicolon = case sing :: SModuleIsTop t of
        SModuleLocal -> return ()
        SModuleTop -> semicolon

instance SingI s => PrettyPrint [Statement s] where
  ppCode :: forall r. Members '[ExactPrint Ann, Reader Options] r => [Statement s] -> Sem r ()
  ppCode ss = vsep2 (Proxy @Ann) (map ppGroup (P.groupStatements ss))
    where
      ppGroup :: [Statement s] -> Sem r ()
      ppGroup = vsep (Proxy @Ann) . endSemicolon . map ppCode

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
  ppCode :: Members '[ExactPrint Ann, Reader Options] r => QualifiedName -> Sem r ()
  ppCode q@QualifiedName {..} = do
    let symbols = _qualifiedPath ^. pathParts NonEmpty.|> _qualifiedSymbol
    str <- P.dotted <$> mapM P.ppSymbol symbols
    morpheme (getLoc q) str

ppMorpheme :: (Members '[ExactPrint Ann, Reader Options] r, P.PrettyCode c, HasLoc c) => c -> Sem r ()
ppMorpheme n = P.ppCode n >>= morpheme (getLoc n)

instance SingI s => PrettyPrint (Import s) where
  ppCode :: Members '[ExactPrint Ann, Reader Options] r => Import s -> Sem r ()
  ppCode (Import m) = do
    ppCode kwImport
      <+> ppModulePath
    where
      ppModulePath = case sing :: SStage s of
        SParsed -> ppCode m
        SScoped -> ppMorpheme (m ^. modulePath)

instance SingI s => PrettyPrint (Statement s) where
  ppCode = \case
    StatementOperator op -> todo
    StatementTypeSignature sig -> todo
    StatementImport i -> ppCode i
    StatementInductive d -> todo
    StatementModule m -> todo
    StatementOpenModule o -> todo
    StatementFunctionClause c -> todo
    StatementAxiom a -> todo
    StatementForeign p -> todo
    StatementCompile c -> todo

todo :: a
todo = error "todo"
