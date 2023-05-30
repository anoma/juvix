module Juvix.Compiler.Concrete.Print.Base
  ( module Juvix.Compiler.Concrete.Print.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Base qualified as P
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.CodeAnn (Ann, CodeAnn (..), ppStringLit)
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.Keyword.All
import Juvix.Prelude hiding ((<+>), (<+?>), (<?+>), (?<>))
import Juvix.Prelude.Pretty (annotate, pretty)

type PrettyPrinting a = forall r. Members '[ExactPrint, Reader Options] r => a -> Sem r ()

class PrettyPrint a where
  ppCode :: Members '[ExactPrint, Reader Options] r => a -> Sem r ()

instance PrettyPrint Keyword where
  ppCode p = noLoc . annotate ann . pretty $ p
    where
      ann = case p ^. keywordType of
        KeywordTypeDelimiter -> AnnDelimiter
        KeywordTypeKeyword -> AnnKeyword
        KeywordTypeJudoc -> AnnJudoc

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
    SModuleLocal -> ppCode x
    SModuleTop -> ppCode x
  SScoped -> case sing :: SModuleIsTop t of
    SModuleLocal -> P.ppCode x >>= morpheme (getLoc x) . P.annSDef x
    SModuleTop -> P.ppCode x >>= morpheme (getLoc x) . P.annSDef x

ppSymbolType :: forall s. SingI s => PrettyPrinting (SymbolType s)
ppSymbolType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppIdentifierType :: forall s. SingI s => PrettyPrinting (IdentifierType s)
ppIdentifierType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppModuleRefType :: forall s. SingI s => PrettyPrinting (ModuleRefType s)
ppModuleRefType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppImportType :: forall s. SingI s => PrettyPrinting (ImportType s)
ppImportType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppHoleType :: forall s. SingI s => PrettyPrinting (HoleType s)
ppHoleType w = case sing :: SStage s of
  SParsed -> ppCode kwWildcard
  SScoped -> ppCode w

ppPatternAtomIdenType :: forall s. SingI s => PrettyPrinting (PatternAtomIdenType s)
ppPatternAtomIdenType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppExpressionType :: forall s. SingI s => PrettyPrinting (ExpressionType s)
ppExpressionType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternType :: forall s. SingI s => PrettyPrinting (PatternType s)
ppPatternType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternParensType :: forall s. SingI s => PrettyPrinting (PatternParensType s)
ppPatternParensType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtType :: forall s. SingI s => PrettyPrinting (PatternAtType s)
ppPatternAtType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

instance PrettyPrint PatternBinding where
  ppCode PatternBinding {..} = do
    let n' = ppSymbolType _patternBindingName
        p' = ppCode _patternBindingPattern
    n' <> ppCode kwAt <> p'

instance SingI s => PrettyPrint (PatternAtom s) where
  ppCode = \case
    PatternAtomIden n -> ppPatternAtomIdenType n
    PatternAtomWildcard {} -> ppCode kwWildcard
    PatternAtomEmpty {} -> parens (return ())
    PatternAtomParens p -> parens (ppPatternParensType p)
    PatternAtomBraces p -> braces (ppPatternParensType p)
    PatternAtomAt p -> ppPatternAtType p

instance SingI s => PrettyPrint (PatternAtoms s) where
  ppCode (PatternAtoms ps _) = hsep (ppCode <$> ps)

instance SingI s => PrettyPrint (ExpressionAtoms s) where
  ppCode as = hsep (ppCode <$> as ^. expressionAtoms)

instance SingI s => PrettyPrint (ExpressionAtom s) where
  ppCode = \case
    AtomIdentifier n -> ppIdentifierType n
    AtomLambda l -> ppCode l
    AtomLetBlock lb -> ppCode lb
    AtomCase c -> ppCode c
    AtomUniverse uni -> ppCode uni
    AtomFunction fun -> ppCode fun
    AtomLiteral lit -> ppCode lit
    AtomFunArrow a -> ppCode a
    AtomParens e -> parens (ppExpressionType e)
    AtomBraces e -> braces (ppExpressionType (e ^. withLocParam))
    AtomHole w -> ppHoleType w

instance PrettyPrint PatternScopedIden where
  ppCode = \case
    PatternScopedVar v -> ppCode v
    PatternScopedConstructor c -> ppCode c

instance PrettyPrint Hole where
  ppCode h = do
    let uid = h ^. holeId
    withNameIdSuffix uid (ppCode kwWildcard)

withNameIdSuffix :: Members '[ExactPrint, Reader Options] r => S.NameId -> Sem r () -> Sem r ()
withNameIdSuffix nid a = do
  showNameId <- asks (^. optShowNameIds)
  a
  when showNameId (noLoc "@" <> ppCode nid)

instance PrettyPrint S.NameId where
  ppCode (S.NameId k) = noLoc (pretty k)

instance (SingI t, SingI s) => PrettyPrint (Module s t) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Module s t -> Sem r ()
  ppCode Module {..} = do
    let moduleBody' = localIndent (ppCode _moduleBody)
        modulePath' = ppModulePathType _modulePath
        moduleDoc' = whenJust _moduleDoc ppCode
        modulePragmas' = whenJust _modulePragmas ppCode
        body'
          | null _moduleBody = ensureEmptyLine
          | otherwise =
              topSpace
                <> moduleBody'
                <> line
    moduleDoc'
      <> modulePragmas'
      <> ppCode _moduleKw
      <+> modulePath'
        <> ppCode kwSemicolon
        <> line
        <> body'
        <> ending
    where
      topSpace :: Sem r ()
      topSpace = case sing :: SModuleIsTop t of
        SModuleLocal -> mempty
        SModuleTop -> ensureEmptyLine

      localIndent :: Sem r () -> Sem r ()
      localIndent = case sing :: SModuleIsTop t of
        SModuleLocal -> indent
        SModuleTop -> id

      ending :: Sem r ()
      ending = case sing :: SModuleIsTop t of
        SModuleLocal -> ppCode _moduleKwEnd
        SModuleTop -> end

instance SingI s => PrettyPrint [Statement s] where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => [Statement s] -> Sem r ()
  ppCode ss = paragraphs (ppGroup <$> P.groupStatements ss)
    where
      ppGroup :: NonEmpty (Statement s) -> Sem r ()
      ppGroup = vsep . endSemicolon . fmap ppCode

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

instance PrettyPrint AxiomRef where
  ppCode a = ppCode (a ^. axiomRefName)

instance PrettyPrint InductiveRef where
  ppCode a = ppCode (a ^. inductiveRefName)

instance PrettyPrint FunctionRef where
  ppCode a = ppCode (a ^. functionRefName)

instance PrettyPrint ConstructorRef where
  ppCode a = ppCode (a ^. constructorRefName)

instance PrettyPrint ScopedIden where
  ppCode = \case
    ScopedAxiom a -> ppCode a
    ScopedInductive i -> ppCode i
    ScopedVar n -> ppCode n
    ScopedFunction f -> ppCode f
    ScopedConstructor c -> ppCode c

instance SingI s => PrettyPrint (Import s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => Import s -> Sem r ()
  ppCode i = do
    ppCode (i ^. importKw)
      <+> ppImportType (i ^. importModule)
      <+?> ppQual
    where
      ppQual :: Maybe (Sem r ())
      ppQual = case i ^. importAsName of
        Nothing -> Nothing
        Just as -> Just (noLoc P.kwAs <+> ppModulePathType as)

instance PrettyPrint SyntaxDef where
  ppCode = \case
    SyntaxOperator op -> ppCode op

instance PrettyPrint OperatorSyntaxDef where
  ppCode OperatorSyntaxDef {..} = do
    opSymbol' <- P.ppUnkindedSymbol _opSymbol
    fi
      <+> morpheme (getLoc _opSymbol) opSymbol'
    where
      fi = do
        p <- P.ppCode (_opFixity ^. fixityPrecedence)
        ppCode _opSyntaxKw <+> ppCode _opKw <+> noLoc p

instance PrettyPrint Expression where
  ppCode = ppMorpheme

instance PrettyPrint ParsedPragmas where
  ppCode = ppMorpheme

ppJudocStart :: Members '[ExactPrint, Reader Options] r => Sem r (Maybe ())
ppJudocStart = do
  inBlock <- asks (^. optInJudocBlock)
  if
      | inBlock -> return Nothing
      | otherwise -> ppCode delimJudocStart $> Just ()

instance SingI s => PrettyPrint (Example s) where
  ppCode e =
    ppJudocStart
      <??+> noLoc P.ppJudocExampleStart
      <+> ppExpressionType (e ^. exampleExpression)
        <> noLoc P.kwSemicolon

instance SingI s => PrettyPrint (JudocParagraphLine s) where
  ppCode = ppMorpheme

instance SingI s => PrettyPrint (Judoc s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => Judoc s -> Sem r ()
  ppCode (Judoc groups) = ppGroups groups <> line
    where
      ppGroups :: NonEmpty (JudocGroup s) -> Sem r ()
      ppGroups = \case
        g :| [] -> ppCode g
        g :| b : bs -> ppCode g <> groupSep <> ppGroups (b :| bs)
          where
            groupSep :: Sem r ()
            groupSep = line <> extraLine
            extraLine :: Sem r ()
            extraLine = case (g, b) of
              (JudocGroupLines {}, JudocGroupLines {}) -> ppCode delimJudocStart <> line
              _ -> return ()

instance SingI s => PrettyPrint (JudocBlockParagraph s) where
  ppCode p = do
    let start' = ppCode (p ^. judocBlockParagraphStart)
        contents' = inJudocBlock (vsep2 (ppCode <$> p ^. judocBlockParagraphBlocks))
        endpar' = ppCode (p ^. judocBlockParagraphEnd)
    start' <+> contents' <+> endpar'

instance SingI s => PrettyPrint (JudocGroup s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => JudocGroup s -> Sem r ()
  ppCode = \case
    JudocGroupLines l -> goLines l
    JudocGroupBlock l -> ppCode l
    where
      goLines blocks = sequenceWith blockSep (fmap ppCode blocks)
        where
          blockSep :: Sem r ()
          blockSep = line >> ppJudocStart >> line

instance SingI s => PrettyPrint (JudocBlock s) where
  ppCode = \case
    JudocParagraphLines l -> vsep (ppCode <$> l)
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

instance SingI s => PrettyPrint (AxiomDef s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => AxiomDef s -> Sem r ()
  ppCode AxiomDef {..} = do
    axiomName' <- P.annDef _axiomName <$> P.ppSymbol _axiomName
    let builtin' :: Maybe (Sem r ()) = (<> line) . (\x -> P.ppCode x >>= morpheme (getLoc x)) <$> _axiomBuiltin
        _axiomDoc' :: Maybe (Sem r ()) = ppCode <$> _axiomDoc
        _axiomPragmas' :: Maybe (Sem r ()) = ppCode <$> _axiomPragmas
    _axiomDoc'
      ?<> _axiomPragmas'
      ?<> builtin'
      ?<> ppCode _axiomKw
      <+> morpheme (getLocSymbolType _axiomName) axiomName'
      <+> noLoc P.kwColon
      <+> ppExpressionType _axiomType

instance PrettyPrint (WithLoc BuiltinInductive) where
  ppCode b = P.ppCode (b ^. withLocParam) >>= morpheme (getLoc b)

instance PrettyPrint (WithLoc BuiltinFunction) where
  ppCode b = P.ppCode (b ^. withLocParam) >>= morpheme (getLoc b)

instance SingI s => PrettyPrint (TypeSignature s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => TypeSignature s -> Sem r ()
  ppCode TypeSignature {..} = do
    let termin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _sigTerminating
        doc' :: Maybe (Sem r ()) = ppCode <$> _sigDoc
        pragmas' :: Maybe (Sem r ()) = ppCode <$> _sigPragmas
        builtin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _sigBuiltin
        type' = ppExpressionType _sigType
        name' = region (P.annDef _sigName) (ppSymbolType _sigName)
        body' = case _sigBody of
          Nothing -> Nothing
          Just body -> Just (noLoc P.kwAssign <> oneLineOrNext (ppExpressionType body))
    doc'
      ?<> pragmas'
      ?<> builtin'
      ?<> termin'
      ?<> ( name'
              <+> noLoc P.kwColon
                <> oneLineOrNext
                  ( type'
                      <+?> body'
                  )
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

instance SingI s => PrettyPrint (UsingHiding s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => UsingHiding s -> Sem r ()
  ppCode uh = do
    let bracedList =
          encloseSep
            (noLoc P.kwBraceL)
            (noLoc P.kwBraceR)
            (noLoc P.kwSemicolon <> space)
            ppItems
    kw' <+> bracedList
    where
      kw' :: Sem r ()
      kw' = case uh of
        Using {} -> ppCode kwUsing
        Hiding {} -> ppCode kwHiding
      ppItems :: NonEmpty (Sem r ())
      ppItems = case uh of
        Using s -> fmap ppCode s
        Hiding s -> fmap ppSymbolType s

instance SingI s => PrettyPrint (UsingItem s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => UsingItem s -> Sem r ()
  ppCode ui = do
    let kwAs' :: Sem r () = ppCode kwAs
        as' = (kwAs' <+>) . ppSymbolType <$> ui ^. usingAs
        sym' = ppSymbolType (ui ^. usingSymbol)
    sym' <+?> as'

instance PrettyPrint ModuleRef where
  ppCode (ModuleRef' (_ :&: ModuleRef'' {..})) = ppCode _moduleRefName

instance SingI s => PrettyPrint (OpenModule s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => OpenModule s -> Sem r ()
  ppCode OpenModule {..} = do
    let name' = ppModuleRefType _openModuleName
        usingHiding' = ppCode <$> _openUsingHiding
        importkw' = ppCode <$> _openModuleImportKw
        openkw = ppCode _openModuleKw
        alias' = (noLoc P.kwAs <+>) . ppModulePathType <$> _openImportAsName
        public' = case _openPublic of
          Public -> Just (noLoc P.kwPublic)
          NoPublic -> Nothing
    case importkw' of
      Nothing -> do
        openkw
          <+> name'
          <+?> usingHiding'
          <+?> public'
      Just importkw ->
        importkw
          <+> name'
          <+?> alias'
          <+> openkw
          <+?> usingHiding'
          <+?> public'

instance SingI s => PrettyPrint (FunctionClause s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => FunctionClause s -> Sem r ()
  ppCode FunctionClause {..} = do
    let clauseFun' = ppSymbolType _clauseOwnerFunction
        clausePatterns' = case nonEmpty _clausePatterns of
          Nothing -> Nothing
          Just ne -> Just (hsep (ppPatternType <$> ne))
        clauseBody' = ppExpressionType _clauseBody
    clauseFun'
      <+?> clausePatterns'
      <+> noLoc P.kwAssign
        <> oneLineOrNext clauseBody'

-- TODO was this necessary??
-- instance PrettyPrint PatternArg where
--   ppCode pat =
--     case pat ^. patternArgPattern of
--       PatternVariable s | s ^. S.nameVerbatim == "=" -> parens (ppAtom pat)
--       _ -> ppAtom pat

instance SingI s => PrettyPrint (InductiveParameters s) where
  ppCode InductiveParameters {..} = do
    let names' = fmap (\nm -> region (P.annDef nm) (ppSymbolType nm)) _inductiveParametersNames
        ty' = ppExpressionType _inductiveParametersType
    parens (hsep names' <+> ppCode kwColon <+> ty')

instance SingI s => PrettyPrint (NonEmpty (InductiveParameters s)) where
  ppCode = hsep . fmap ppCode

instance (PrettyPrint a) => PrettyPrint (Irrelevant a) where
  ppCode (Irrelevant a) = ppCode a

instance SingI s => PrettyPrint (InductiveConstructorDef s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => InductiveConstructorDef s -> Sem r ()
  ppCode InductiveConstructorDef {..} = do
    let constructorName' = region (P.annDef _constructorName) (ppSymbolType _constructorName)
        constructorType' = ppExpressionType _constructorType
        doc' = ppCode <$> _constructorDoc
        pragmas' = ppCode <$> _constructorPragmas
    nest (pipeHelper <+> doc' ?<> pragmas' ?<> constructorName' <+> noLoc P.kwColon <+> constructorType')
    where
      -- we use this helper so that comments appear before the first optional pipe if the pipe was omitted
      pipeHelper :: Sem r ()
      pipeHelper = case _constructorPipe ^. unIrrelevant of
        Just p -> ppCode p
        Nothing -> P.ppCode kwPipe >>= morpheme (getLocSymbolType _constructorName)

ppInductiveSignature :: SingI s => PrettyPrinting (InductiveDef s)
ppInductiveSignature InductiveDef {..} = do
  let builtin' = (<> line) . ppCode <$> _inductiveBuiltin
      name' = region (P.annDef _inductiveName) (ppSymbolType _inductiveName)
      params' = ppCode <$> nonEmpty _inductiveParameters
      ty' = case _inductiveType of
        Nothing -> Nothing
        Just e -> Just (noLoc P.kwColon <+> ppExpressionType e)
      positive'
        | Just k <- _inductivePositive = (<> line) <$> Just (ppCode k)
        | otherwise = Nothing
  builtin'
    ?<> positive'
    ?<> ppCode _inductiveKw
    <+> name'
    <+?> params'
    <+?> ty'

instance SingI s => PrettyPrint (InductiveDef s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => InductiveDef s -> Sem r ()
  ppCode d@InductiveDef {..} = do
    let doc' = ppCode <$> _inductiveDoc
        pragmas' = ppCode <$> _inductivePragmas
        constrs' = ppConstructorBlock _inductiveConstructors
        sig' = ppInductiveSignature d
    doc'
      ?<> pragmas'
      ?<> sig'
      <+> noLoc P.kwAssign
        <> line
        <> (indent . align) constrs'
    where
      ppConstructorBlock :: NonEmpty (InductiveConstructorDef s) -> Sem r ()
      ppConstructorBlock cs = vsep (ppCode <$> cs)

instance SingI s => PrettyPrint (Statement s) where
  ppCode = \case
    StatementSyntax s -> ppCode s
    StatementTypeSignature s -> ppCode s
    StatementImport i -> ppCode i
    StatementInductive i -> ppCode i
    StatementModule m -> ppCode m
    StatementOpenModule o -> ppCode o
    StatementFunctionClause c -> ppCode c
    StatementAxiom a -> ppCode a
