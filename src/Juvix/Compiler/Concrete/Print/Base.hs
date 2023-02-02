module Juvix.Compiler.Concrete.Print.Base
  ( module Juvix.Compiler.Concrete.Print.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Base qualified as P
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.CodeAnn (Ann, CodeAnn (..), ppStringLit)
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.Keyword.All
import Juvix.Prelude.Base hiding ((<+>), (<+?>), (<?+>), (?<>))
import Juvix.Prelude.Path
import Juvix.Prelude.Pretty (annotate, pretty)

class PrettyPrint a where
  ppCode :: Members '[ExactPrint, Reader Options] r => a -> Sem r ()

instance PrettyPrint Keyword where
  ppCode = noLoc . pretty

instance PrettyPrint KeywordRef where
  ppCode = ppMorpheme

docNoComments :: PrettyPrint c => Options -> c -> Doc Ann
docNoComments = docHelper Nothing

docHelper :: PrettyPrint c => Maybe FileComments -> Options -> c -> Doc Ann
docHelper cs opts x =
  run
    . execExactPrint cs
    . runReader opts
    . ppCode
    $ x

doc :: (PrettyPrint c, HasLoc c) => Options -> Comments -> c -> Doc Ann
doc opts cs x = docHelper (Just (fileComments file cs)) opts x
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

instance SingI t => PrettyPrint (Module 'Scoped t) where
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
        SModuleTop -> semicolon >> line <> end

instance PrettyPrint [Statement 'Scoped] where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => [Statement 'Scoped] -> Sem r ()
  ppCode ss = vsep2 (map ppGroup (P.groupStatements ss))
    where
      ppGroup :: [Statement 'Scoped] -> Sem r ()
      ppGroup = vsep . endSemicolon . map ppCode

instance PrettyPrint TopModulePath where
  ppCode t@TopModulePath {..} =
    mapM P.ppSymbol (_modulePathDir ++ [_modulePathName]) >>= morpheme (getLoc t) . P.dotted

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
        _axiomDoc' :: Maybe (Sem r ()) = ppCode <$> _axiomDoc
    _axiomDoc'
      ?<> builtin'
      <?+> ppCode _axiomKw
        <+> morpheme (getLoc _axiomName) axiomName'
        <+> noLoc P.kwColon
        <+> ppCode _axiomType

instance PrettyPrint (WithLoc BuiltinInductive) where
  ppCode b = P.ppCode (b ^. withLocParam) >>= morpheme (getLoc b)

instance PrettyPrint (WithLoc BuiltinFunction) where
  ppCode b = P.ppCode (b ^. withLocParam) >>= morpheme (getLoc b)

instance PrettyPrint (TypeSignature 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => TypeSignature 'Scoped -> Sem r ()
  ppCode TypeSignature {..} = do
    let termin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _sigTerminating
        doc' :: Maybe (Sem r ()) = ppCode <$> _sigDoc
        builtin' :: Maybe (Sem r ()) = ppCode <$> _sigBuiltin
        type' = ppCode _sigType
        name' = region (P.annDef _sigName) (ppCode _sigName)
        body' = case _sigBody of
          Nothing -> Nothing
          Just body -> Just (noLoc P.kwAssign <+> ppCode body)
    doc'
      ?<> builtin'
      <?+> termin'
        ?<> hang
          ( name'
              <+> noLoc P.kwColon
              <+> type'
              <+?> body'
          )

instance PrettyPrint Pattern where
  ppCode = ppMorpheme

delimIf :: Members '[ExactPrint] r => IsImplicit -> Bool -> Sem r () -> Sem r ()
delimIf Implicit _ = braces
delimIf Explicit True = parens
delimIf Explicit False = id

instance PrettyPrint PatternArg where
  ppCode PatternArg {..} = do
    let name' = ppCode <$> _patternArgName
        pat' = ppCode _patternArgPattern
    (name' <&> (<> noLoc P.kwAt))
      ?<> delimIf _patternArgIsImplicit delimCond pat'
    where
      delimCond :: Bool
      delimCond = isJust _patternArgName && not (isAtomic _patternArgPattern)

instance PrettyPrint (WithLoc Text) where
  ppCode k = morpheme (getLoc k) (pretty (k ^. withLocParam))

ppUnkindedSymbol :: Members '[Reader Options, ExactPrint] r => WithLoc Text -> Sem r ()
ppUnkindedSymbol = region (annotate AnnUnkindedSym) . ppCode

ppAtom :: (HasAtomicity c, PrettyPrint c, Members '[ExactPrint, Reader Options] r) => c -> Sem r ()
ppAtom c
  | isAtomic c = ppCode c
  | otherwise = parens (ppCode c)

instance PrettyPrint UsingHiding where
  ppCode uh = do
    let bracedList =
          encloseSep
            (noLoc P.kwBraceL)
            (noLoc P.kwBraceR)
            (noLoc P.kwSemicolon)
            (ppUnkindedSymbol <$> syms)
    noLoc (pretty word) <+> bracedList
    where
      (word, syms) = case uh of
        Using s -> (kwUsing, s)
        Hiding s -> (kwHiding, s)

instance PrettyPrint ModuleRef where
  ppCode (ModuleRef' (_ :&: ModuleRef'' {..})) = ppCode _moduleRefName

instance PrettyPrint (OpenModule 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => OpenModule 'Scoped -> Sem r ()
  ppCode OpenModule {..} = do
    let name' = ppCode _openModuleName
        usingHiding' = ppCode <$> _openUsingHiding
        openParameters' = hsep . fmap ppAtom <$> nonEmpty _openParameters
        importkw' = ppCode <$> _openModuleImportKw
        public' = case _openPublic of
          Public -> Just (noLoc P.kwPublic)
          NoPublic -> Nothing
    ppCode _openModuleKw
      <+?> importkw'
      <+> name'
      <+?> openParameters'
      <+?> usingHiding'
      <+?> public'

instance PrettyPrint (FunctionClause 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => FunctionClause 'Scoped -> Sem r ()
  ppCode FunctionClause {..} = do
    let clauseFun' = ppCode _clauseOwnerFunction
        clausePatterns' = case nonEmpty _clausePatterns of
          Nothing -> Nothing
          Just ne -> Just (hsep (ppPatternAtom <$> ne))
        clauseBody' = ppCode _clauseBody
    clauseFun'
      <+?> clausePatterns'
      <+> noLoc P.kwAssign
      <+> nest clauseBody'

ppPatternAtom :: forall r. (Members '[Reader Options, ExactPrint] r) => PatternArg -> Sem r ()
ppPatternAtom pat =
  case pat ^. patternArgPattern of
    PatternVariable s | s ^. S.nameVerbatim == "=" -> parens (ppAtom pat)
    _ -> ppAtom pat

instance PrettyPrint (InductiveParameter 'Scoped) where
  ppCode InductiveParameter {..} = do
    let name' = region (P.annDef _inductiveParameterName) (ppCode _inductiveParameterName)
        ty' = ppCode _inductiveParameterType
    parens (name' <+> ppCode kwColon <+> ty')

ppInductiveParameters ::
  (Members '[ExactPrint, Reader Options] r) =>
  [InductiveParameter 'Scoped] ->
  Maybe (Sem r ())
ppInductiveParameters params = case params of
  InductiveParameter {..} : _ -> Just $ do
      let params0 = takeWhile (\p -> p ^. inductiveParameterType == _inductiveParameterType) params
          params1 = dropWhile (\p -> p ^. inductiveParameterType == _inductiveParameterType) params
          names = hsep $ fmap (ppCode . (^. inductiveParameterName)) params0
          ty = ppCode _inductiveParameterType
          params' = ppInductiveParameters params1
      parens (names <+> ppCode kwColon <+> ty) <+?> params'
  _ ->
    Nothing

instance (PrettyPrint a) => PrettyPrint (Irrelevant a) where
  ppCode (Irrelevant a) = ppCode a

instance PrettyPrint (InductiveConstructorDef 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => InductiveConstructorDef 'Scoped -> Sem r ()
  ppCode InductiveConstructorDef {..} = do
    let constructorName' = region (P.annDef _constructorName) (ppCode _constructorName)
        constructorType' = ppCode _constructorType
        doc' = ppCode <$> _constructorDoc
    hang (pipeHelper <+> doc' ?<> constructorName' <+> noLoc P.kwColon <+> constructorType')
    where
      -- we use this helper so that comments appear before the first optional pipe if the pipe was omitted
      pipeHelper :: Sem r ()
      pipeHelper = case _constructorPipe ^. unIrrelevant of
        Just p -> ppCode p
        Nothing -> P.ppCode kwPipe >>= morpheme (getLoc _constructorName)

ppInductiveSignature :: forall r. Members '[ExactPrint, Reader Options] r => InductiveDef 'Scoped -> Sem r ()
ppInductiveSignature InductiveDef {..} = do
  let builtin' = ppCode <$> _inductiveBuiltin
      name' = region (P.annDef _inductiveName) (ppCode _inductiveName)
      params' = ppInductiveParameters _inductiveParameters
      ty' = case _inductiveType of
        Nothing -> Nothing
        Just e -> Just (noLoc P.kwColon <+> ppCode e)
  builtin'
    <?+> ppCode _inductiveKw
    <+> name'
    <+?> params'
    <+?> ty'

instance PrettyPrint (InductiveDef 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => InductiveDef 'Scoped -> Sem r ()
  ppCode d@InductiveDef {..} = do
    let doc' = ppCode <$> _inductiveDoc
        constrs' = ppConstructorBlock _inductiveConstructors
        sig' = ppInductiveSignature d
    doc'
      ?<> sig'
      <+> noLoc P.kwAssign
        <> line
        <> (indent . align) constrs'
    where
      ppConstructorBlock :: NonEmpty (InductiveConstructorDef 'Scoped) -> Sem r ()
      ppConstructorBlock cs = vsep (ppCode <$> cs)

instance PrettyPrint (WithLoc Backend) where
  ppCode = ppMorpheme

instance PrettyPrint ForeignBlock where
  ppCode ForeignBlock {..} = do
    let _foreignBackend' = ppCode _foreignBackend
    ppCode _foreignKw
      <+> _foreignBackend'
      <+> lbrace
        <> line
        <> noLoc (pretty (escape _foreignCode))
        <> line
        <> rbrace
    where
      escape :: Text -> Text
      escape = Text.replace "}" "\\}"

instance PrettyPrint BackendItem where
  ppCode BackendItem {..} = do
    let backend' = ppCode _backendItemBackend
    backend'
      <+> noLoc P.kwMapsto
      <+> noLoc (ppStringLit _backendItemCode)

instance PrettyPrint (Compile 'Scoped) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => Compile 'Scoped -> Sem r ()
  ppCode Compile {..} = do
    let name' = ppCode _compileName
        items' = ppBlock _compileBackendItems
    ppCode _compileKw
      <+> name'
      <+> items'
    where
      ppBlock :: PrettyPrint c => [c] -> Sem r ()
      ppBlock = bracesIndent . vsep . map ((<> semicolon) . ppCode)

instance PrettyPrint (Statement 'Scoped) where
  ppCode = \case
    StatementOperator o -> ppCode o
    StatementTypeSignature s -> ppCode s
    StatementImport i -> ppCode i
    StatementInductive i -> ppCode i
    StatementModule m -> ppCode m
    StatementOpenModule o -> ppCode o
    StatementFunctionClause c -> ppCode c
    StatementAxiom a -> ppCode a
    StatementForeign f -> ppCode f
    StatementCompile c -> ppCode c
