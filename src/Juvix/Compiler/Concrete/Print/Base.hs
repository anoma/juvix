{-# LANGUAGE QuantifiedConstraints #-}

module Juvix.Compiler.Concrete.Print.Base
  ( module Juvix.Compiler.Concrete.Print.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.InfoTable
import Juvix.Compiler.Concrete.Data.NameSignature.Base
import Juvix.Compiler.Concrete.Data.Scope.Base
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra qualified as Concrete
import Juvix.Compiler.Concrete.Keywords qualified as Kw
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.Ape.Base
import Juvix.Data.Ape.Print
import Juvix.Data.CodeAnn (Ann, CodeAnn (..), ppStringLit)
import Juvix.Data.CodeAnn qualified as C
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.Keyword.All qualified as Kw
import Juvix.Data.NameKind
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude hiding ((<+>), (<+?>), (<?+>), (?<>))
import Juvix.Prelude.Pretty (annotate, pretty)
import Juvix.Prelude.Pretty qualified as P

type PrettyPrinting a = forall r. (Members '[ExactPrint, Reader Options] r) => a -> Sem r ()

class PrettyPrint a where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => a -> Sem r ()

instance PrettyPrint Keyword where
  ppCode p = noLoc . annotate ann . pretty $ p
    where
      ann = case p ^. keywordType of
        KeywordTypeDelimiter -> AnnDelimiter
        KeywordTypeKeyword -> AnnKeyword
        KeywordTypeJudoc -> AnnJudoc

instance PrettyPrint KeywordRef where
  ppCode p =
    morphemeM
      (getLoc p)
      ( annotated (C.kwTypeAnn (p ^. keywordRefKeyword . keywordType))
          . noLoc
          . pretty
          $ p
      )

docNoComments :: (PrettyPrint c) => Options -> c -> Doc Ann
docNoComments = docHelper Nothing

docHelper :: (PrettyPrint c) => Maybe FileComments -> Options -> c -> Doc Ann
docHelper cs opts x =
  run
    . execExactPrint cs
    . runReader opts
    . ppCode
    $ x

docNoLoc :: (PrettyPrint c) => Options -> c -> Doc Ann
docNoLoc opts x = docHelper Nothing opts x

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
    SModuleLocal -> annSDef x (ppCode x)
    SModuleTop -> annSDef x (ppCode x)

ppSymbolType :: forall s. (SingI s) => PrettyPrinting (SymbolType s)
ppSymbolType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppIdentifierType :: forall s. (SingI s) => PrettyPrinting (IdentifierType s)
ppIdentifierType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppModuleRefType :: forall s. (SingI s) => PrettyPrinting (ModuleRefType s)
ppModuleRefType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppImportType :: forall s. (SingI s) => PrettyPrinting (ImportType s)
ppImportType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppHoleType :: forall s. (SingI s) => PrettyPrinting (HoleType s)
ppHoleType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtomIdenType :: forall s. (SingI s) => PrettyPrinting (PatternAtomIdenType s)
ppPatternAtomIdenType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppExpressionType :: forall s. (SingI s) => PrettyPrinting (ExpressionType s)
ppExpressionType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppExpressionAtomType :: forall s. (SingI s) => PrettyPrinting (ExpressionType s)
ppExpressionAtomType = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

ppPatternAtomType :: forall s. (SingI s) => PrettyPrinting (PatternAtomType s)
ppPatternAtomType = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

ppPatternParensType :: forall s. (SingI s) => PrettyPrinting (PatternParensType s)
ppPatternParensType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtType :: forall s. (SingI s) => PrettyPrinting (PatternAtType s)
ppPatternAtType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppAnyStage :: forall k. (forall s. (SingI s) => PrettyPrint (k s)) => PrettyPrinting (AnyStage k)
ppAnyStage (s :&: p) = case s of
  SParsed -> ppCode p
  SScoped -> ppCode p

instance PrettyPrint S.AbsModulePath where
  ppCode S.AbsModulePath {..} = do
    let absLocalPath' = ppCode <$> _absLocalPath
        absTopModulePath' = ppCode _absTopModulePath
    dotted (absTopModulePath' : absLocalPath')

instance PrettyPrint PatternBinding where
  ppCode PatternBinding {..} = do
    let n' = ppSymbolType _patternBindingName
        p' = ppCode _patternBindingPattern
    n' <> ppCode Kw.kwAt <> p'

instance (SingI s) => PrettyPrint (ListPattern s) where
  ppCode ListPattern {..} = do
    let l = ppCode _listpBracketL
        r = ppCode _listpBracketR
        e = hsepSemicolon (map ppPatternParensType _listpItems)
    l <> e <> r

instance PrettyPrint Void where
  ppCode = absurd

instance PrettyPrint NameBlock where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => NameBlock -> Sem r ()
  ppCode NameBlock {..} = do
    let delims = case _nameImplicit of
          Implicit -> braces
          ImplicitInstance -> doubleBraces
          Explicit -> parens
        ppElem :: (Symbol, Int) -> Sem r ()
        ppElem (sym, idx) = ppCode sym <> ppCode Kw.kwExclamation <> noLoc (pretty idx)
    delims (hsepSemicolon (map ppElem (toList _nameBlock)))

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
  ppCode (a, b) = tuple [ppCode a, ppCode b]

instance PrettyPrint NameSignature where
  ppCode NameSignature {..}
    | null _nameSignatureArgs = noLoc (pretty @Text "<empty name signature>")
    | otherwise = hsep . map ppCode $ _nameSignatureArgs

instance (SingI s) => PrettyPrint (PatternAtom s) where
  ppCode = \case
    PatternAtomIden n -> ppPatternAtomIdenType n
    PatternAtomWildcard w -> ppCode w
    PatternAtomList l -> ppCode l
    PatternAtomEmpty {} -> parens (return ())
    PatternAtomParens p -> parens (ppPatternParensType p)
    PatternAtomBraces p -> braces (ppPatternParensType p)
    PatternAtomDoubleBraces p -> doubleBraces (ppPatternParensType p)
    PatternAtomAt p -> ppPatternAtType p
    PatternAtomRecord p -> ppCode p

instance (SingI s) => PrettyPrint (PatternAtoms s) where
  ppCode (PatternAtoms ps _) = hsep (ppCode <$> ps)

instance (SingI s) => PrettyPrint (ExpressionAtoms s) where
  ppCode ::
    forall r.
    (Members '[ExactPrint, Reader Options] r) =>
    ExpressionAtoms s ->
    Sem r ()
  ppCode as = go (toList $ as ^. expressionAtoms)
    where
      go :: [ExpressionAtom s] -> Sem r ()
      go = \case
        [] -> return ()
        [x] -> ppCode x
        (x : xs@(AtomRecordUpdate {} : _)) -> ppCode x >> go xs
        (x : xs) -> ppCode x >> space >> go xs

instance (SingI s) => PrettyPrint (Initializer s) where
  ppCode Initializer {..} = do
    let n = ppPatternParensType _initializerPattern
        e = ppExpressionType _initializerExpression
    n <+> ppCode _initializerAssignKw <+> e

instance (SingI s) => PrettyPrint (Range s) where
  ppCode Range {..} = do
    let n = ppPatternParensType _rangePattern
        e = ppExpressionType _rangeExpression
    n <+> ppCode _rangeInKw <+> e

instance (SingI s) => PrettyPrint (Iterator s) where
  ppCode Iterator {..} = do
    let n = ppIdentifierType _iteratorName
        is = ppCode <$> _iteratorInitializers
        rngs = ppCode <$> _iteratorRanges
        is' = parens . hsepSemicolon <$> nonEmpty is
        rngs' = parens . hsepSemicolon <$> nonEmpty rngs
        b = ppExpressionType _iteratorBody
        b'
          | _iteratorBodyBraces = braces (oneLineOrNextNoIndent b)
          | otherwise = line <> b
    parensIf _iteratorParens $
      hang (n <+?> is' <+?> rngs' <> b')

instance PrettyPrint S.AName where
  ppCode n = annotated (AnnKind (S.getNameKind n)) (noLoc (pretty (n ^. S.anameVerbatim)))

instance PrettyPrint FunctionInfo where
  ppCode = \case
    FunctionInfo f -> ppCode f

instance (SingI s) => PrettyPrint (List s) where
  ppCode List {..} = do
    let l = ppCode _listBracketL
        r = ppCode _listBracketR
        e = hsepSemicolon (map ppExpressionType _listItems)
    l <> e <> r

instance (SingI s) => PrettyPrint (NamedArgument s) where
  ppCode NamedArgument {..} = do
    let s = ppCode _namedArgName
        kwassign = ppCode _namedArgAssignKw
        val = ppExpressionType _namedArgValue
    s <+> kwassign <+> val

instance (SingI s) => PrettyPrint (ArgumentBlock s) where
  ppCode ArgumentBlock {..} = do
    let args' = ppCode <$> _argBlockArgs
        (l, r) = case d of
          Nothing -> (enqueue C.kwParenL, noLoc C.kwParenR)
          Just (l', r') -> (ppCode l', ppCode r')
    l <> align (sepSemicolon args') <> r
    where
      Irrelevant d = _argBlockDelims

instance (SingI s) => PrettyPrint (NamedApplication s) where
  -- ppCode :: Members '[ExactPrint, Reader Options] r => NamedApplication s -> Sem r ()
  ppCode = apeHelper

instance (SingI s) => PrettyPrint (RecordCreation s) where
  ppCode RecordCreation {..} = do
    let fields' =
          blockIndent
            ( sequenceWith
                (semicolon >> line)
                (ppCode <$> _recordCreationFields)
            )
    ppIdentifierType _recordCreationConstructor
      <> ppCode _recordCreationAtKw
      <> braces fields'

instance (SingI s) => PrettyPrint (RecordUpdateField s) where
  ppCode RecordUpdateField {..} =
    ppSymbolType _fieldUpdateName <+> ppCode _fieldUpdateAssignKw <+> ppExpressionType _fieldUpdateValue

instance (SingI s) => PrettyPrint (RecordDefineField s) where
  ppCode RecordDefineField {..} = ppCode _fieldDefineFunDef

instance (SingI s) => PrettyPrint (RecordUpdate s) where
  ppCode RecordUpdate {..} = do
    let Irrelevant (l, r) = _recordUpdateDelims
        fields'
          | null (_recordUpdateFields ^. _tail1) = ppCode (_recordUpdateFields ^. _head1)
          | otherwise =
              line
                <> indent
                  ( sequenceWith
                      (semicolon >> line)
                      (ppCode <$> _recordUpdateFields)
                  )
                <> line
    ppCode _recordUpdateAtKw
      <> ppIdentifierType _recordUpdateTypeName
      <> ppCode l
      <> fields'
      <> ppCode r

instance (SingI s) => PrettyPrint (DoubleBracesExpression s) where
  ppCode DoubleBracesExpression {..} = do
    let (l, r) = _doubleBracesDelims ^. unIrrelevant
    ppCode l <> ppExpressionType _doubleBracesExpression <> ppCode r

instance (SingI s) => PrettyPrint (ExpressionAtom s) where
  ppCode = \case
    AtomIdentifier n -> ppIdentifierType n
    AtomLambda l -> ppCode l
    AtomLet lb -> ppCode lb
    AtomCase c -> ppCode c
    AtomNewCase c -> ppCode c
    AtomList l -> ppCode l
    AtomUniverse uni -> ppCode uni
    AtomRecordUpdate u -> ppCode u
    AtomFunction fun -> ppCode fun
    AtomLiteral lit -> ppCode lit
    AtomFunArrow a -> ppCode a
    AtomParens e -> parens (ppExpressionType e)
    AtomDoubleBraces e -> ppCode e
    AtomBraces e -> braces (ppExpressionType (e ^. withLocParam))
    AtomHole w -> ppHoleType w
    AtomInstanceHole w -> ppHoleType w
    AtomIterator i -> ppCode i
    AtomNamedApplication i -> ppCode i
    AtomRecordCreation i -> ppCode i

instance PrettyPrint PatternScopedIden where
  ppCode = \case
    PatternScopedVar v -> ppCode v
    PatternScopedConstructor c -> ppCode c

instance PrettyPrint Hole where
  ppCode h = do
    let uid = h ^. holeId
    withNameIdSuffix uid (ppCode (h ^. holeKw))

withNameIdSuffix :: (Members '[ExactPrint, Reader Options] r) => S.NameId -> Sem r () -> Sem r ()
withNameIdSuffix nid a = do
  showNameId <- asks (^. optShowNameIds)
  a
  when showNameId (noLoc "@" <> ppCode nid)

instance PrettyPrint S.NameId where
  ppCode (S.NameId k) = noLoc (pretty k)

ppModuleHeader :: (SingI t, SingI s) => PrettyPrinting (Module s t)
ppModuleHeader Module {..} = do
  let modulePath' = ppModulePathType _modulePath
  ppCode _moduleKw
    <+> modulePath'

instance (SingI t, SingI s) => PrettyPrint (Module s t) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Module s t -> Sem r ()
  ppCode m@Module {..} = do
    let moduleBody' = localIndent (ppStatements _moduleBody)
        moduleDoc' = whenJust _moduleDoc ppCode
        modulePragmas' = whenJust _modulePragmas ppCode
        header' = ppModuleHeader m
        body'
          | null _moduleBody = ensureEmptyLine
          | otherwise =
              topSpace
                <> moduleBody'
                <> line
    moduleDoc'
      <> modulePragmas'
      <> header'
      <> ppCode Kw.delimSemicolon
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

instance (PrettyPrint a) => PrettyPrint [a] where
  ppCode x = do
    let cs = map ppCode (toList x)
    encloseSep (ppCode @Text "[") (ppCode @Text "]") (ppCode @Text ", ") cs

ppStatements :: forall s r. (SingI s, Members '[ExactPrint, Reader Options] r) => [Statement s] -> Sem r ()
ppStatements ss = paragraphs (ppGroup <$> Concrete.groupStatements (filter (not . isInductiveModule) ss))
  where
    isInductiveModule :: Statement s -> Bool
    isInductiveModule = \case
      StatementModule m -> m ^. moduleInductive
      _ -> False
    ppGroup :: NonEmpty (Statement s) -> Sem r ()
    ppGroup = vsep . sepEndSemicolon . fmap ppCode

instance PrettyPrint TopModulePath where
  ppCode TopModulePath {..} =
    dotted (map ppSymbolType (_modulePathDir ++ [_modulePathName]))

instance (PrettyPrint n) => PrettyPrint (S.Name' n) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => S.Name' n -> Sem r ()
  ppCode S.Name' {..} = do
    let nameConcrete' = region (C.annotateKind _nameKind) (ppCode _nameConcrete)
    annSRef (withNameIdSuffix _nameId nameConcrete')
    where
      annSRef :: Sem r () -> Sem r ()
      annSRef = annotated (AnnRef (_nameDefinedIn ^. S.absTopModulePath) _nameId)

instance PrettyPrint Name where
  ppCode n = case n of
    NameUnqualified s -> ppCode s
    NameQualified s -> ppCode s

instance PrettyPrint QualifiedName where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => QualifiedName -> Sem r ()
  ppCode QualifiedName {..} = do
    let symbols = _qualifiedPath ^. pathParts NonEmpty.|> _qualifiedSymbol
    dotted (ppSymbolType <$> symbols)

instance (SingI t) => PrettyPrint (ModuleRef'' 'S.NotConcrete t) where
  ppCode = ppCode @(ModuleRef' 'S.NotConcrete) . project

instance PrettyPrint (ModuleRef'' 'S.Concrete t) where
  ppCode m = ppCode (m ^. moduleRefName)

instance PrettyPrint ScopedIden where
  ppCode = ppCode . (^. scopedIdenName)

instance (SingI s) => PrettyPrint (AliasDef s) where
  ppCode AliasDef {..} =
    ppCode _aliasDefSyntaxKw
      <+> ppCode _aliasDefAliasKw
      <+> ppSymbolType _aliasDefName
      <+> ppCode Kw.kwAssign
      <+> ppIdentifierType _aliasDefAsName

instance (SingI s) => PrettyPrint (SyntaxDef s) where
  ppCode = \case
    SyntaxFixity f -> ppCode f
    SyntaxOperator op -> ppCode op
    SyntaxIterator it -> ppCode it
    SyntaxAlias it -> ppCode it

instance PrettyPrint Literal where
  ppCode = noLoc . ppLiteral

ppLiteral :: Literal -> Doc Ann
ppLiteral = \case
  LitInteger n -> annotate AnnLiteralInteger (pretty n)
  LitString s -> ppStringLit s

instance (SingI s) => PrettyPrint (LambdaClause s) where
  ppCode LambdaClause {..} = do
    let lambdaParameters' = hsep (ppPatternAtom <$> _lambdaParameters)
        lambdaBody' = ppExpressionType _lambdaBody
        lambdaPipe' = ppCode <$> _lambdaPipe ^. unIrrelevant
    lambdaPipe' <?+> lambdaParameters' <+> ppCode _lambdaAssignKw <> oneLineOrNext lambdaBody'

instance (SingI s) => PrettyPrint (LetStatement s) where
  ppCode = \case
    LetFunctionDef f -> ppCode f
    LetAliasDef f -> ppCode f
    LetOpen f -> ppCode f

instance (SingI s) => PrettyPrint (Let s) where
  ppCode Let {..} = do
    let letFunDefs' = blockIndent (ppBlock _letFunDefs)
        letExpression' = ppExpressionType _letExpression
    align $ ppCode _letKw <> letFunDefs' <> ppCode _letInKw <+> letExpression'

instance (SingI s) => PrettyPrint (NewCase s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => NewCase s -> Sem r ()
  ppCode NewCase {..} = do
    let exp' = ppExpressionType _newCaseExpression
    align $ ppCode _newCaseKw <> oneLineOrNextBlock exp' <> ppCode _newCaseOfKw <+> ppBranches _newCaseBranches
    where
      ppBranches :: NonEmpty (NewCaseBranch s) -> Sem r ()
      ppBranches = \case
        b :| [] -> oneLineOrNextBraces (ppCaseBranch True b)
        _ -> braces (blockIndent (vsepHard (ppCaseBranch False <$> _newCaseBranches)))

      ppCaseBranch :: Bool -> NewCaseBranch s -> Sem r ()
      ppCaseBranch singleBranch b = pipeHelper <?+> ppCode b
        where
          pipeHelper :: Maybe (Sem r ())
          pipeHelper
            | singleBranch = Nothing
            | otherwise = Just $ case b ^. newCaseBranchPipe . unIrrelevant of
                Just p -> ppCode p
                Nothing -> ppCode Kw.kwPipe

instance (SingI s) => PrettyPrint (Case s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Case s -> Sem r ()
  ppCode Case {..} = do
    let exp' = ppExpressionType _caseExpression
    ppCode _caseKw <+> exp' <+> ppCode Kw.kwOf <+> ppBranches _caseBranches
    where
      ppBranches :: NonEmpty (CaseBranch s) -> Sem r ()
      ppBranches = \case
        b :| [] -> braces (ppCaseBranch True b)
        _ -> braces (blockIndent (vsepHard (ppCaseBranch False <$> _caseBranches)))

      ppCaseBranch :: Bool -> CaseBranch s -> Sem r ()
      ppCaseBranch singleBranch b = pipeHelper <?+> ppCode b
        where
          pipeHelper :: Maybe (Sem r ())
          pipeHelper
            | singleBranch = Nothing
            | otherwise = Just (ppCode (b ^. caseBranchPipe . unIrrelevant))

instance PrettyPrint Universe where
  ppCode Universe {..} = ppCode _universeKw <+?> (noLoc <$> (pretty <$> _universeLevel))

apeHelper :: (IsApe a ApeLeaf, Members '[Reader Options, ExactPrint] r) => a -> Sem r ()
apeHelper a = do
  opts <- ask @Options
  let params :: ApeParams ApeLeaf
      params = ApeParams (runReader opts . ppCode)
  runApe params a

instance PrettyPrint ApeLeaf where
  ppCode = \case
    ApeLeafExpression e -> ppCode e
    ApeLeafFunctionParams a -> ppCode a
    ApeLeafFunctionKw r -> ppCode r
    ApeLeafPattern r -> ppCode r
    ApeLeafPatternArg r -> ppCode r
    ApeLeafAtom r -> ppAnyStage r
    ApeLeafArgumentBlock r -> ppAnyStage r

annDef :: forall s r. (SingI s, Members '[ExactPrint] r) => SymbolType s -> Sem r () -> Sem r ()
annDef nm = case sing :: SStage s of
  SScoped -> annSDef nm
  SParsed -> id

annSDef :: (Members '[ExactPrint] r) => S.Name' n -> Sem r () -> Sem r ()
annSDef S.Name' {..} = annotated (AnnDef (_nameDefinedIn ^. S.absTopModulePath) _nameId)

instance (SingI s) => PrettyPrint (FunctionParameters s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => FunctionParameters s -> Sem r ()
  ppCode FunctionParameters {..} = do
    case _paramNames of
      []
        | _paramImplicit == Explicit -> ppLeftExpression' funFixity _paramType
      _ -> do
        let paramNames' = map ppCode _paramNames
            paramType' = ppExpressionType _paramType
            delims' = over both ppCode <$> _paramDelims ^. unIrrelevant
            colon' = ppCode <$> _paramColon ^. unIrrelevant
            pre = case colon' of
              Just col -> hsep paramNames' <+> col <> space
              Nothing -> mempty
        delimIf' delims' _paramImplicit True (pre <> paramType')
    where
      ppLeftExpression' = case sing :: SStage s of
        SParsed -> ppLeftExpression
        SScoped -> ppLeftExpression

instance (SingI s) => PrettyPrint (FunctionParameter s) where
  ppCode = \case
    FunctionParameterName n -> annDef n (ppSymbolType n)
    FunctionParameterWildcard w -> ppCode w

instance (SingI s) => PrettyPrint (Function s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Function s -> Sem r ()
  ppCode a = case sing :: SStage s of
    SParsed -> helper a
    SScoped -> apeHelper a
    where
      helper :: Function 'Parsed -> Sem r ()
      helper Function {..} = do
        let funParameter' = ppCode _funParameters
            funReturn' = ppRightExpression' funFixity _funReturn
            funKw' = ppCode _funKw
        funParameter' <+> funKw' <+> funReturn'
        where
          ppRightExpression' = case sing :: SStage s of
            SParsed -> ppRightExpression
            SScoped -> ppRightExpression

ppRightExpression ::
  (PrettyPrint a, HasAtomicity a, Members '[Reader Options, ExactPrint] r) =>
  Fixity ->
  a ->
  Sem r ()
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyPrint a, HasAtomicity a, Members '[Reader Options, ExactPrint] r) =>
  Fixity ->
  a ->
  Sem r ()
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyPrint a, Members '[Reader Options, ExactPrint] r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r ()
ppLRExpression associates fixlr e =
  parensIf
    (atomParens associates (atomicity e) fixlr)
    (ppCode e)

instance (SingI s) => PrettyPrint (CaseBranch s) where
  ppCode CaseBranch {..} = do
    let pat' = ppPatternParensType _caseBranchPattern
        e' = ppExpressionType _caseBranchExpression
    pat' <+> ppCode _caseBranchAssignKw <> oneLineOrNext e'

instance (SingI s) => PrettyPrint (NewCaseBranch s) where
  ppCode NewCaseBranch {..} = do
    let pat' = ppPatternParensType _newCaseBranchPattern
        e' = ppExpressionType _newCaseBranchExpression
    pat' <+> ppCode _newCaseBranchAssignKw <> oneLineOrNext e'

ppBlock :: (PrettyPrint a, Members '[Reader Options, ExactPrint] r, Traversable t) => t a -> Sem r ()
ppBlock items = vsep (sepEndSemicolon (fmap ppCode items))

instance (SingI s) => PrettyPrint (Lambda s) where
  ppCode Lambda {..} = do
    let lambdaKw' = ppCode _lambdaKw
        braces' = uncurry enclose (over both ppCode (_lambdaBraces ^. unIrrelevant))
        lambdaClauses' = case _lambdaClauses of
          s :| [] -> braces' (ppCode s)
          _ -> braces' (blockIndent (vsepHard (ppCode <$> _lambdaClauses)))
    lambdaKw' <+> lambdaClauses'

instance PrettyPrint Precedence where
  ppCode = \case
    PrecArrow -> noLoc (pretty ("-ω" :: Text))
    PrecNat n -> noLoc (pretty n)
    PrecApp -> noLoc (pretty ("ω" :: Text))
    PrecUpdate -> noLoc (pretty ("ω₁" :: Text))

ppFixityDefHeaderNew :: (SingI s) => PrettyPrinting (FixitySyntaxDef s)
ppFixityDefHeaderNew FixitySyntaxDef {..} = do
  let sym' = annotated (AnnKind KNameFixity) (ppSymbolType _fixitySymbol)
  ppCode _fixitySyntaxKw <+> ppCode _fixityKw <+> sym'

instance PrettyPrint Arity where
  ppCode = \case
    Unary -> noLoc Str.unary
    Binary -> noLoc Str.binary
    None -> noLoc Str.none

instance PrettyPrint BinaryAssoc where
  ppCode a = noLoc $ case a of
    AssocNone -> Str.none
    AssocLeft -> Str.left
    AssocRight -> Str.right

ppSymbolList :: (SingI s) => PrettyPrinting [SymbolType s]
ppSymbolList items = do
  ppCode Kw.kwBracketL
  hsepSemicolon (map ppSymbolType items)
  ppCode Kw.kwBracketR

instance (SingI s) => PrettyPrint (ParsedFixityInfo s) where
  ppCode ParsedFixityInfo {..} = do
    let rhs = do
          ParsedFixityFields {..} <- _fixityFields
          let assocItem = do
                a <- _fixityFieldsAssoc
                return (ppCode Kw.kwAssoc <+> ppCode Kw.kwAssign <+> ppCode a)
              sameItem = do
                a <- _fixityFieldsPrecSame
                return (ppCode Kw.kwSame <+> ppCode Kw.kwAssign <+> ppSymbolType a)
              aboveItem = do
                a <- _fixityFieldsPrecAbove
                return (ppCode Kw.kwAbove <+> ppCode Kw.kwAssign <+> ppSymbolList a)
              belowItem = do
                a <- _fixityFieldsPrecBelow
                return (ppCode Kw.kwBelow <+> ppCode Kw.kwAssign <+> ppSymbolList a)
              items = hsepSemicolon (catMaybes [assocItem, sameItem, aboveItem, belowItem])
              (l, r) = _fixityFieldsBraces ^. unIrrelevant
          return (ppCode l <> items <> ppCode r)
    ppCode _fixityParsedArity <+?> rhs

instance (SingI s) => PrettyPrint (FixitySyntaxDef s) where
  ppCode f@FixitySyntaxDef {..} = do
    let header' = ppFixityDefHeaderNew f
        body' = ppCode _fixityInfo
    header' <+> ppCode _fixityAssignKw <+> body'

instance PrettyPrint OperatorSyntaxDef where
  ppCode OperatorSyntaxDef {..} = do
    let opSymbol' = ppUnkindedSymbol _opSymbol
        p = ppUnkindedSymbol _opFixity
    ppCode _opSyntaxKw <+> ppCode _opKw <+> opSymbol' <+> p

instance PrettyPrint PatternApp where
  ppCode = apeHelper

instance PrettyPrint Application where
  ppCode = apeHelper

instance PrettyPrint InfixApplication where
  ppCode = apeHelper

instance PrettyPrint PostfixApplication where
  ppCode = apeHelper

instance PrettyPrint ParsedIteratorInfo where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => ParsedIteratorInfo -> Sem r ()
  ppCode ParsedIteratorInfo {..} = do
    let (l, r) = _parsedIteratorInfoBraces ^. unIrrelevant
        ppInt :: WithLoc Int -> Sem r ()
        ppInt = morphemeWithLoc . fmap (annotate AnnLiteralInteger . pretty)
        iniItem = do
          a <- _parsedIteratorInfoInitNum
          return (ppCode Kw.kwInit <+> ppCode Kw.kwAssign <+> ppInt a)
        rangeItem = do
          a <- _parsedIteratorInfoRangeNum
          return (ppCode Kw.kwRange <+> ppCode Kw.kwAssign <+> ppInt a)
        items = hsepSemicolon (catMaybes [iniItem, rangeItem])
    ppCode l <> items <> ppCode r

instance PrettyPrint IteratorSyntaxDef where
  ppCode IteratorSyntaxDef {..} = do
    let iterSymbol' = ppUnkindedSymbol _iterSymbol
    ppCode _iterSyntaxKw
      <+> ppCode _iterIteratorKw
      <+> iterSymbol'
      <+?> fmap ppCode _iterInfo

instance PrettyPrint RecordUpdateApp where
  ppCode = apeHelper

instance PrettyPrint ParensRecordUpdate where
  ppCode = parens . ppCode . (^. parensRecordUpdate)

instance PrettyPrint Expression where
  ppCode = \case
    ExpressionIdentifier n -> ppCode n
    ExpressionHole w -> ppCode w
    ExpressionInstanceHole w -> ppCode w
    ExpressionParensIdentifier n -> parens (ppCode n)
    ExpressionBraces b -> braces (ppCode b)
    ExpressionDoubleBraces b -> ppCode b
    ExpressionApplication a -> ppCode a
    ExpressionList a -> ppCode a
    ExpressionInfixApplication a -> ppCode a
    ExpressionPostfixApplication a -> ppCode a
    ExpressionLambda l -> ppCode l
    ExpressionLet lb -> ppCode lb
    ExpressionUniverse u -> ppCode u
    ExpressionLiteral l -> ppCode l
    ExpressionFunction f -> ppCode f
    ExpressionCase c -> ppCode c
    ExpressionNewCase c -> ppCode c
    ExpressionIterator i -> ppCode i
    ExpressionNamedApplication i -> ppCode i
    ExpressionRecordCreation i -> ppCode i
    ExpressionRecordUpdate i -> ppCode i
    ExpressionParensRecordUpdate i -> ppCode i

instance PrettyPrint (WithSource Pragmas) where
  ppCode pragma = do
    b <- asks (^. optPrintPragmas)
    when b $
      let txt = pretty (Str.pragmasStart <> pragma ^. withSourceText <> Str.pragmasEnd)
       in annotated AnnComment (noLoc txt) <> line

ppJudocStart :: (Members '[ExactPrint, Reader Options] r) => Sem r (Maybe ())
ppJudocStart = do
  inBlock <- asks (^. optInJudocBlock)
  if
      | inBlock -> return Nothing
      | otherwise -> ppCode Kw.delimJudocStart $> Just ()

instance (SingI s) => PrettyPrint (Example s) where
  ppCode e =
    ppJudocStart
      <??+> ppCode Kw.delimJudocExample
      <+> ppExpressionType (e ^. exampleExpression)
        <> semicolon

instance (PrettyPrint a) => PrettyPrint (WithLoc a) where
  ppCode a = morphemeM (getLoc a) (ppCode (a ^. withLocParam))

instance (SingI s) => PrettyPrint (JudocAtom s) where
  ppCode :: forall r. (Members '[Reader Options, ExactPrint] r) => JudocAtom s -> Sem r ()
  ppCode = \case
    JudocExpression e -> goExpression e
    JudocText t -> annotated AnnJudoc (noLoc (pretty t))
    where
      goExpression :: ExpressionType s -> Sem r ()
      goExpression = semiDelim . ppExpressionType
      semiDelim :: Sem r () -> Sem r ()
      semiDelim = enclose1 (annotated AnnJudoc (noLoc (";" :: Doc Ann)))

instance (SingI s) => PrettyPrint (JudocLine s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => JudocLine s -> Sem r ()
  ppCode (JudocLine deli atoms) = do
    let start' :: Maybe (Sem r ()) = ppCode <$> deli
        atoms' = mapM_ ppCode atoms
    start' <?+> atoms'

instance (SingI s) => PrettyPrint (Judoc s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Judoc s -> Sem r ()
  ppCode (Judoc groups) = ppGroups groups <> hardline
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
              (JudocGroupLines {}, JudocGroupLines {}) -> ppCode Kw.delimJudocStart <> line
              _ -> return ()

instance (SingI s) => PrettyPrint (JudocBlockParagraph s) where
  ppCode p = do
    let start' = ppCode (p ^. judocBlockParagraphStart)
        contents' = inJudocBlock (vsep2 (ppCode <$> p ^. judocBlockParagraphBlocks))
        endpar' = ppCode (p ^. judocBlockParagraphEnd)
    start' <+> contents' <+> endpar'

instance (SingI s) => PrettyPrint (JudocGroup s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => JudocGroup s -> Sem r ()
  ppCode = \case
    JudocGroupLines l -> goLines l
    JudocGroupBlock l -> ppCode l
    where
      goLines blocks = sequenceWith blockSep (fmap ppCode blocks)
        where
          blockSep :: Sem r ()
          blockSep = hardline >> ppJudocStart >> hardline

instance (SingI s) => PrettyPrint (JudocBlock s) where
  ppCode = \case
    JudocLines l -> vsep (ppCode <$> l)
    JudocExample e -> ppCode e

instance (SingI s) => PrettyPrint (AxiomDef s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => AxiomDef s -> Sem r ()
  ppCode AxiomDef {..} = do
    let axiomName' = annDef _axiomName (ppSymbolType _axiomName)
        builtin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _axiomBuiltin
        _axiomDoc' :: Maybe (Sem r ()) = ppCode <$> _axiomDoc
        _axiomPragmas' :: Maybe (Sem r ()) = ppCode <$> _axiomPragmas
    _axiomDoc'
      ?<> _axiomPragmas'
      ?<> builtin'
      ?<> ppCode _axiomKw
      <+> axiomName'
      <+> ppCode _axiomColonKw
      <+> ppExpressionType _axiomType

instance PrettyPrint BuiltinInductive where
  ppCode i = ppCode Kw.kwBuiltin <+> keywordText (P.prettyText i)

instance PrettyPrint BuiltinFunction where
  ppCode i = ppCode Kw.kwBuiltin <+> keywordText (P.prettyText i)

instance PrettyPrint BuiltinAxiom where
  ppCode i = ppCode Kw.kwBuiltin <+> keywordText (P.prettyText i)

instance (SingI s) => PrettyPrint (FunctionClause s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => FunctionClause s -> Sem r ()
  ppCode FunctionClause {..} = do
    let pats' = hsep (ppPatternAtomType <$> _clausenPatterns)
        e' = ppExpressionType _clausenBody
    ppCode _clausenPipeKw <+> pats' <+> ppCode _clausenAssignKw <> oneLineOrNext e'

instance (SingI s) => PrettyPrint (Argument s) where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => Argument s -> Sem r ()
  ppCode = \case
    ArgumentSymbol s -> ppSymbolType s
    ArgumentWildcard w -> ppCode w

instance (SingI s) => PrettyPrint (SigArg s) where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => SigArg s -> Sem r ()
  ppCode SigArg {..} = do
    let Irrelevant (l, r) = _sigArgDelims
        names' = hsep (fmap ppCode _sigArgNames)
        colon' = ppCode <$> _sigArgColon
        ty = ppExpressionType <$> _sigArgType
        arg = case _sigArgImplicit of
          ImplicitInstance | isNothing colon' -> mempty <>? ty
          _ -> names' <+?> colon' <+?> ty
    ppCode l <> arg <> ppCode r

ppFunctionSignature :: (SingI s) => PrettyPrinting (FunctionDef s)
ppFunctionSignature FunctionDef {..} = do
  let termin' = (<> line) . ppCode <$> _signTerminating
      instance' = (<> line) . ppCode <$> _signInstance
      args' = hsep . fmap ppCode <$> nonEmpty _signArgs
      builtin' = (<> line) . ppCode <$> _signBuiltin
      type' = case _signColonKw ^. unIrrelevant of
        Just col -> oneLineOrNext (ppCode col <+> ppExpressionType (fromJust _signRetType))
        Nothing -> mempty
      name' = annDef _signName (ppSymbolType _signName)
   in builtin'
        ?<> termin'
        ?<> instance'
        ?<> ( name'
                <+?> args'
                  <> type'
            )

instance (SingI s) => PrettyPrint (FunctionDef s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => FunctionDef s -> Sem r ()
  ppCode fun@FunctionDef {..} = do
    let doc' :: Maybe (Sem r ()) = ppCode <$> _signDoc
        pragmas' :: Maybe (Sem r ()) = ppCode <$> _signPragmas
        sig' = ppFunctionSignature fun
        body' = case _signBody of
          SigBodyExpression e -> space <> ppCode Kw.kwAssign <> oneLineOrNext (ppExpressionType e)
          SigBodyClauses k -> line <> indent (vsep (ppCode <$> k))
    doc'
      ?<> pragmas'
      ?<> sig'
      <> body'

instance PrettyPrint Wildcard where
  ppCode w = morpheme (getLoc w) C.kwWildcard

instance (SingI s) => PrettyPrint (FieldPun s) where
  ppCode = ppSymbolType . (^. fieldPunField)

instance (SingI s) => PrettyPrint (RecordPatternAssign s) where
  ppCode a = do
    ppCode (a ^. recordPatternAssignField)
      <+> ppCode (a ^. recordPatternAssignKw)
      <+> ppPatternParensType (a ^. recordPatternAssignPattern)

instance (SingI s) => PrettyPrint (RecordPatternItem s) where
  ppCode = \case
    RecordPatternItemFieldPun f -> ppCode f
    RecordPatternItemAssign f -> ppCode f

instance (SingI s) => PrettyPrint (RecordPattern s) where
  ppCode r = do
    let c = ppIdentifierType (r ^. recordPatternConstructor)
        items = sepSemicolon (map ppCode (r ^. recordPatternItems))
    c <> noLoc C.kwAt <> align (braces items)

instance PrettyPrint Pattern where
  ppCode = \case
    PatternVariable v -> annDef v (ppCode v)
    PatternApplication (PatternApp l r) -> do
      let l' = ppLeftExpression appFixity l
          r' = ppRightExpression appFixity r
      l' <+> r'
    PatternWildcard w -> ppCode w
    PatternList w -> ppCode w
    PatternEmpty {} -> parens (return ())
    PatternConstructor constr -> ppCode constr
    PatternInfixApplication i -> apeHelper i
    PatternPostfixApplication i -> apeHelper i
    PatternRecord i -> ppCode i

instance PrettyPrint PatternArg where
  ppCode PatternArg {..} = do
    let name' = ppCode <$> _patternArgName
        pat' = ppCode _patternArgPattern
    (name' <&> (<> ppCode Kw.kwAt))
      ?<> delimIf _patternArgIsImplicit delimCond pat'
    where
      delimCond :: Bool
      delimCond = isJust _patternArgName && not (isAtomic _patternArgPattern)

instance PrettyPrint Text where
  ppCode = noLoc . pretty

ppUnkindedSymbol :: (Members '[Reader Options, ExactPrint] r) => WithLoc Text -> Sem r ()
ppUnkindedSymbol = region (annotate AnnUnkindedSym) . ppCode

instance (SingI s) => PrettyPrint (HidingItem s) where
  ppCode h = do
    let sym = ppSymbolType (h ^. hidingSymbol)
        kwmodule = ppCode <$> (h ^. hidingModuleKw)
    kwmodule <?+> sym

instance (SingI s) => PrettyPrint (HidingList s) where
  ppCode HidingList {..} = do
    let (openb, closeb) = _hidingBraces ^. unIrrelevant
        items' = sequenceWith (semicolon <> space) (ppCode <$> _hidingList)
    ppCode _hidingKw <+> ppCode openb <> items' <> ppCode closeb

instance (SingI s) => PrettyPrint (UsingList s) where
  ppCode UsingList {..} = do
    let (openb, closeb) = _usingBraces ^. unIrrelevant
        items' = sequenceWith (semicolon <> space) (ppCode <$> _usingList)
    ppCode _usingKw <+> ppCode openb <> items' <> ppCode closeb

instance (SingI s) => PrettyPrint (UsingHiding s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => UsingHiding s -> Sem r ()
  ppCode = \case
    Using u -> ppCode u
    Hiding h -> ppCode h

instance (SingI s) => PrettyPrint (UsingItem s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => UsingItem s -> Sem r ()
  ppCode ui = do
    let kwAs' :: Maybe (Sem r ()) = ppCode <$> ui ^. usingAsKw . unIrrelevant
        alias' = ppSymbolType <$> ui ^. usingAs
        sym' = ppSymbolType (ui ^. usingSymbol)
        kwmodule = ppCode <$> (ui ^. usingModuleKw)
    kwmodule <?+> (sym' <+?> kwAs' <+?> alias')

instance PrettyPrint (ModuleRef' 'S.NotConcrete) where
  ppCode (ModuleRef' (t :&: m)) =
    let path = m ^. moduleRefModule . modulePath
        txt = case t of
          SModuleTop -> annotate (AnnKind KNameTopModule) (pretty path)
          SModuleLocal -> annotate (AnnKind KNameLocalModule) (pretty path)
     in noLoc txt

instance PrettyPrint ModuleRef where
  ppCode (ModuleRef' (_ :&: ModuleRef'' {..})) = ppCode _moduleRefName

instance (SingI s) => PrettyPrint (Import s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => Import s -> Sem r ()
  ppCode i = do
    let open' = ppOpenModuleHelper Nothing <$> (i ^. importOpen)
    ppCode (i ^. importKw)
      <+> ppImportType (i ^. importModule)
      <+?> ppAlias
      <+?> open'
    where
      ppAlias :: Maybe (Sem r ())
      ppAlias = case i ^. importAsName of
        Nothing -> Nothing
        Just as -> Just (ppCode Kw.kwAs <+> ppModulePathType as)

ppOpenModuleHelper :: (SingI s) => Maybe (ModuleRefType s) -> PrettyPrinting (OpenModuleParams s)
ppOpenModuleHelper modName OpenModuleParams {..} = do
  let name' = ppModuleRefType <$> modName
      usingHiding' = ppCode <$> _openUsingHiding
      openkw = ppCode _openModuleKw
      public' = ppCode <$> _openPublicKw ^. unIrrelevant
  openkw
    <+?> name'
    <+?> usingHiding'
    <+?> public'

instance (SingI s) => PrettyPrint (OpenModule s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => OpenModule s -> Sem r ()
  ppCode OpenModule {..} = ppOpenModuleHelper (Just _openModuleName) _openModuleParams

ppCodeAtom :: (HasAtomicity c, PrettyPrint c) => PrettyPrinting c
ppCodeAtom c = do
  let p' = ppCode c
  if isAtomic c then p' else parens p'

ppPatternAtom :: forall s. (SingI s) => PrettyPrinting (PatternAtomType s)
ppPatternAtom = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> \pat ->
    case pat ^. patternArgPattern of
      PatternVariable s | s ^. S.nameVerbatim == "=" -> parens (ppCodeAtom pat)
      _ -> ppCodeAtom pat

instance (SingI s) => PrettyPrint (InductiveParametersRhs s) where
  ppCode InductiveParametersRhs {..} =
    ppCode _inductiveParametersColon <+> ppExpressionType _inductiveParametersType

instance (SingI s) => PrettyPrint (InductiveParameters s) where
  ppCode InductiveParameters {..} = do
    let names' = fmap (\nm -> annDef nm (ppSymbolType nm)) _inductiveParametersNames
    case _inductiveParametersRhs of
      Just rhs -> parens (hsep names' <+> ppCode rhs)
      Nothing -> hsep names'

instance (SingI s) => PrettyPrint (NonEmpty (InductiveParameters s)) where
  ppCode = hsep . fmap ppCode

instance (PrettyPrint a) => PrettyPrint (Irrelevant a) where
  ppCode (Irrelevant a) = ppCode a

instance (SingI s) => PrettyPrint (RhsGadt s) where
  ppCode RhsGadt {..} =
    ppCode _rhsGadtColon <+> ppExpressionType _rhsGadtType

instance (SingI s) => PrettyPrint (RecordField s) where
  ppCode RecordField {..} =
    ppSymbolType _fieldName <+> ppCode _fieldColon <+> ppExpressionType _fieldType

instance (SingI s) => PrettyPrint (RhsRecord s) where
  ppCode RhsRecord {..} = do
    let Irrelevant (l, r) = _rhsRecordDelim
        fields'
          | null (_rhsRecordFields ^. _tail1) = ppCode (_rhsRecordFields ^. _head1)
          | otherwise =
              hardline
                <> indent
                  ( sequenceWith
                      (semicolon >> line)
                      (ppCode <$> _rhsRecordFields)
                  )
                <> hardline
    ppCode l <> fields' <> ppCode r

instance (SingI s) => PrettyPrint (RhsAdt s) where
  ppCode = align . sep . fmap ppExpressionAtomType . (^. rhsAdtArguments)

instance (SingI s) => PrettyPrint (ConstructorRhs s) where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => ConstructorRhs s -> Sem r ()
  ppCode = \case
    ConstructorRhsGadt r -> ppCode r
    ConstructorRhsRecord r -> ppCode r
    ConstructorRhsAdt r -> ppCode r

ppConstructorDef :: forall s r. (SingI s, Members '[ExactPrint, Reader Options] r) => Bool -> ConstructorDef s -> Sem r ()
ppConstructorDef singleConstructor ConstructorDef {..} = do
  let constructorName' = annDef _constructorName (ppSymbolType _constructorName)
      constructorRhs' = constructorRhsHelper _constructorRhs
      doc' = ppCode <$> _constructorDoc
      pragmas' = ppCode <$> _constructorPragmas
  pipeHelper <?+> nestHelper (doc' ?<> pragmas' ?<> constructorName' <> constructorRhs')
  where
    constructorRhsHelper :: ConstructorRhs s -> Sem r ()
    constructorRhsHelper r = spaceMay <> ppCode r
      where
        spaceMay = case r of
          ConstructorRhsGadt {} -> space
          ConstructorRhsRecord {} -> space
          ConstructorRhsAdt a
            | null (a ^. rhsAdtArguments) -> mempty
            | otherwise -> space

    nestHelper :: Sem r () -> Sem r ()
    nestHelper
      | singleConstructor = id
      | otherwise = nest

    -- we use this helper so that comments appear before the first optional pipe if the pipe was omitted
    pipeHelper :: Maybe (Sem r ())
    pipeHelper
      | singleConstructor = Nothing
      | otherwise = Just $ case _constructorPipe ^. unIrrelevant of
          Just p -> ppCode p
          Nothing -> ppCode Kw.kwPipe

ppInductiveSignature :: (SingI s) => PrettyPrinting (InductiveDef s)
ppInductiveSignature InductiveDef {..} = do
  let builtin' = (<> line) . ppCode <$> _inductiveBuiltin
      name' = annDef _inductiveName (ppSymbolType _inductiveName)
      params' = ppCode <$> nonEmpty _inductiveParameters
      ty' = case _inductiveType of
        Nothing -> Nothing
        Just e -> Just (colon <+> ppExpressionType e)
      positive'
        | Just k <- _inductivePositive = (<> line) <$> Just (ppCode k)
        | otherwise = Nothing
      trait' = (<> line) . ppCode <$> _inductiveTrait
  builtin'
    ?<> positive'
    ?<> trait'
    ?<> ppCode _inductiveKw
    <+> name'
    <+?> params'
    <+?> ty'

instance (SingI s) => PrettyPrint (InductiveDef s) where
  ppCode :: forall r. (Members '[ExactPrint, Reader Options] r) => InductiveDef s -> Sem r ()
  ppCode d@InductiveDef {..} = do
    let doc' = ppCode <$> _inductiveDoc
        pragmas' = ppCode <$> _inductivePragmas
        constrs' = ppConstructorBlock _inductiveConstructors
        sig' = ppInductiveSignature d
    doc'
      ?<> pragmas'
      ?<> sig'
      <+> ppCode _inductiveAssignKw
        <> constrs'
    where
      ppConstructorBlock :: NonEmpty (ConstructorDef s) -> Sem r ()
      ppConstructorBlock = \case
        c :| [] -> oneLineOrNext (ppConstructorDef True c)
        cs -> line <> indent (vsep (ppConstructorDef False <$> cs))

instance (SingI s) => PrettyPrint (ProjectionDef s) where
  ppCode ProjectionDef {..} =
    do
      ppSymbolType _projectionField
      <+> noLoc ":= projection"
      <+> noLoc (pretty _projectionFieldIx)
      <+> noLoc "for"
      <+> ppCode _projectionConstructor

instance (SingI s) => PrettyPrint (Statement s) where
  ppCode = \case
    StatementSyntax s -> ppCode s
    StatementFunctionDef f -> ppCode f
    StatementImport i -> ppCode i
    StatementInductive i -> ppCode i
    StatementModule m -> ppCode m
    StatementOpenModule o -> ppCode o
    StatementAxiom a -> ppCode a
    StatementProjectionDef a -> ppCode a

instance PrettyPrint PreSymbolEntry where
  ppCode = \case
    PreSymbolAlias a -> ppCode a
    PreSymbolFinal a -> ppCode a

instance PrettyPrint Alias where
  ppCode a =
    noLoc
      ( kindWord
          P.<+> C.code ((pretty (a ^. aliasName . S.nameVerbatim)))
          P.<+> "defined at"
          P.<+> pretty (getLoc a)
      )
    where
      kindWord :: Doc Ann = "Alias"

instance PrettyPrint SymbolEntry where
  ppCode ent =
    noLoc
      ( kindWord
          P.<+> C.code (kindAnn (pretty (ent ^. symbolEntry . S.nameVerbatim)))
          P.<+> "defined at"
          P.<+> pretty (getLoc ent)
      )
    where
      pretty' :: Text -> Doc a
      pretty' = pretty
      (kindAnn :: Doc Ann -> Doc Ann, kindWord :: Doc Ann) =
        let k = getNameKind ent
         in (annotate (AnnKind k), pretty' (nameKindText k))

instance PrettyPrint ModuleSymbolEntry where
  ppCode ent = do
    let mname = ppCode (ent ^. moduleEntry . S.nameVerbatim)
    noLoc
      kindWord
      <+> mname
      <+> noLoc "defined at"
      <+> noLoc (pretty (getLoc ent))
    where
      kindWord :: Doc Ann =
        let k = getNameKind ent
         in (pretty (nameKindText k))

header :: (Members '[ExactPrint] r) => Text -> Sem r ()
header txt = annotated AnnImportant (noLoc (pretty (txt <> "\n")))

instance PrettyPrint ScoperState where
  ppCode :: (Members '[ExactPrint, Reader Options] r) => ScoperState -> Sem r ()
  ppCode s =
    do
      header "scoperModules"
      <> sepSemicolon (map ppCode (HashMap.toList (s ^. scoperModules)))
