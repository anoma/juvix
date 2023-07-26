{-# LANGUAGE QuantifiedConstraints #-}

module Juvix.Compiler.Concrete.Print.Base
  ( module Juvix.Compiler.Concrete.Print.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Concrete.Pretty.Options,
  )
where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.InfoTable
import Juvix.Compiler.Concrete.Data.NameSignature.Base
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Extra (fromAmbiguousIterator)
import Juvix.Compiler.Concrete.Extra qualified as Concrete
import Juvix.Compiler.Concrete.Keywords qualified as Kw
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options
import Juvix.Data.Ape.Base
import Juvix.Data.Ape.Print
import Juvix.Data.CodeAnn (Ann, CodeAnn (..), ppStringLit)
import Juvix.Data.CodeAnn qualified as C
import Juvix.Data.Effect.ExactPrint
import Juvix.Data.IteratorAttribs
import Juvix.Data.Keyword.All qualified as Kw
import Juvix.Data.NameKind
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude hiding ((<+>), (<+?>), (<?+>), (?<>))
import Juvix.Prelude.Pretty (annotate, pretty)
import Juvix.Prelude.Pretty qualified as P

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
  ppCode p =
    morphemeM
      (getLoc p)
      ( annotated (C.kwTypeAnn (p ^. keywordRefKeyword . keywordType))
          . noLoc
          . pretty
          $ p
      )

docNoComments :: PrettyPrint c => Options -> c -> Doc Ann
docNoComments = docHelper Nothing

docHelper :: PrettyPrint c => Maybe FileComments -> Options -> c -> Doc Ann
docHelper cs opts x =
  run
    . execExactPrint cs
    . runReader opts
    . ppCode
    $ x

docNoLoc :: PrettyPrint c => Options -> c -> Doc Ann
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
ppHoleType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtomIdenType :: forall s. SingI s => PrettyPrinting (PatternAtomIdenType s)
ppPatternAtomIdenType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppExpressionType :: forall s. SingI s => PrettyPrinting (ExpressionType s)
ppExpressionType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppAmbiguousIteratorType :: forall s. SingI s => PrettyPrinting (AmbiguousIteratorType s)
ppAmbiguousIteratorType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtomType :: forall s. SingI s => PrettyPrinting (PatternAtomType s)
ppPatternAtomType = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> ppCodeAtom

ppPatternParensType :: forall s. SingI s => PrettyPrinting (PatternParensType s)
ppPatternParensType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppPatternAtType :: forall s. SingI s => PrettyPrinting (PatternAtType s)
ppPatternAtType = case sing :: SStage s of
  SParsed -> ppCode
  SScoped -> ppCode

ppAnyStage :: forall k. (forall s. SingI s => PrettyPrint (k s)) => PrettyPrinting (AnyStage k)
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

instance SingI s => PrettyPrint (ListPattern s) where
  ppCode ListPattern {..} = do
    let l = ppCode _listpBracketL
        r = ppCode _listpBracketR
        e = hsepSemicolon (map ppPatternParensType _listpItems)
    l <> e <> r

instance PrettyPrint Void where
  ppCode = absurd

instance PrettyPrint NameBlock where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => NameBlock -> Sem r ()
  ppCode NameBlock {..} = do
    let delims = case _nameImplicit of
          Implicit -> braces
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

instance SingI s => PrettyPrint (PatternAtom s) where
  ppCode = \case
    PatternAtomIden n -> ppPatternAtomIdenType n
    PatternAtomWildcard w -> ppCode w
    PatternAtomList l -> ppCode l
    PatternAtomEmpty {} -> parens (return ())
    PatternAtomParens p -> parens (ppPatternParensType p)
    PatternAtomBraces p -> braces (ppPatternParensType p)
    PatternAtomAt p -> ppPatternAtType p

instance SingI s => PrettyPrint (PatternAtoms s) where
  ppCode (PatternAtoms ps _) = hsep (ppCode <$> ps)

instance SingI s => PrettyPrint (ExpressionAtoms s) where
  ppCode as = hsep (ppCode <$> as ^. expressionAtoms)

instance SingI s => PrettyPrint (Initializer s) where
  ppCode Initializer {..} = do
    let n = ppPatternParensType _initializerPattern
        e = ppExpressionType _initializerExpression
    n <+> ppCode _initializerAssignKw <+> e

instance SingI s => PrettyPrint (Range s) where
  ppCode Range {..} = do
    let n = ppPatternParensType _rangePattern
        e = ppExpressionType _rangeExpression
    n <+> ppCode _rangeInKw <+> e

instance SingI s => PrettyPrint (Iterator s) where
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
  ppCode (S.AName n) = annotated (AnnKind (S.getNameKind n)) (noLoc (pretty (n ^. S.nameVerbatim)))

instance PrettyPrint FunctionInfo where
  ppCode = \case
    FunctionInfoOld f -> do
      let ty = StatementTypeSignature (f ^. oldFunctionInfoType)
          cs = map StatementFunctionClause (f ^. oldFunctionInfoClauses)
      ppStatements (ty : cs)
    FunctionInfoNew f -> ppCode f

instance SingI s => PrettyPrint (List s) where
  ppCode List {..} = do
    let l = ppCode _listBracketL
        r = ppCode _listBracketR
        e = hsepSemicolon (map ppExpressionType _listItems)
    l <> e <> r

instance PrettyPrint AmbiguousIterator where
  ppCode = ppCode . fromAmbiguousIterator

instance SingI s => PrettyPrint (NamedArgument s) where
  ppCode NamedArgument {..} = do
    let s = ppCode _namedArgName
        kwassign = ppCode _namedArgAssignKw
        val = ppExpressionType _namedArgValue
    s <+> kwassign <+> val

instance SingI s => PrettyPrint (ArgumentBlock s) where
  ppCode ArgumentBlock {..} = do
    let args' = ppCode <$> _argBlockArgs
        (l, r) = case d of
          Nothing -> (enqueue C.kwParenL, noLoc C.kwParenR)
          Just (l', r') -> (ppCode l', ppCode r')
    l <> align (sepSemicolon args') <> r
    where
      Irrelevant d = _argBlockDelims

instance SingI s => PrettyPrint (NamedApplication s) where
  -- ppCode :: Members '[ExactPrint, Reader Options] r => NamedApplication s -> Sem r ()
  ppCode = apeHelper

instance SingI s => PrettyPrint (ExpressionAtom s) where
  ppCode = \case
    AtomIdentifier n -> ppIdentifierType n
    AtomLambda l -> ppCode l
    AtomLet lb -> ppCode lb
    AtomCase c -> ppCode c
    AtomList l -> ppCode l
    AtomUniverse uni -> ppCode uni
    AtomFunction fun -> ppCode fun
    AtomLiteral lit -> ppCode lit
    AtomFunArrow a -> ppCode a
    AtomParens e -> parens (ppExpressionType e)
    AtomBraces e -> braces (ppExpressionType (e ^. withLocParam))
    AtomHole w -> ppHoleType w
    AtomIterator i -> ppCode i
    AtomAmbiguousIterator i -> ppAmbiguousIteratorType i
    AtomNamedApplication i -> ppCode i

instance PrettyPrint PatternScopedIden where
  ppCode = \case
    PatternScopedVar v -> ppCode v
    PatternScopedConstructor c -> ppCode c

instance PrettyPrint Hole where
  ppCode h = do
    let uid = h ^. holeId
    withNameIdSuffix uid (ppCode (h ^. holeKw))

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
    let moduleBody' = localIndent (ppStatements _moduleBody)
        modulePath' = ppModulePathType _modulePath
        moduleDoc' = whenJust _moduleDoc ppCode
        modulePragmas' = whenJust _modulePragmas ppCode
        body'
          | null _moduleBody = ensureEmptyLine
          | otherwise =
              topSpace
                <> moduleBody'
                <> line
    hideIfInductive $
      moduleDoc'
        <> modulePragmas'
        <> ppCode _moduleKw
        <+> modulePath'
          <> ppCode Kw.delimSemicolon
          <> line
          <> body'
          <> ending
    where
      hideIfInductive :: Sem r () -> Sem r ()
      hideIfInductive m = do
        let shouldHide = case sing :: SModuleIsTop t of
              SModuleLocal -> _moduleInductive
              SModuleTop -> False
        unless shouldHide m

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

instance PrettyPrint a => PrettyPrint [a] where
  ppCode x = do
    let cs = map ppCode (toList x)
    encloseSep (ppCode @Text "[") (ppCode @Text "]") (ppCode @Text ", ") cs

ppStatements :: forall s r. (SingI s, Members '[ExactPrint, Reader Options] r) => [Statement s] -> Sem r ()
ppStatements ss = paragraphs (ppGroup <$> Concrete.groupStatements ss)
  where
    ppGroup :: NonEmpty (Statement s) -> Sem r ()
    ppGroup = vsep . sepEndSemicolon . fmap ppCode

instance PrettyPrint TopModulePath where
  ppCode TopModulePath {..} =
    dotted (map ppSymbolType (_modulePathDir ++ [_modulePathName]))

instance PrettyPrint n => PrettyPrint (S.Name' n) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => S.Name' n -> Sem r ()
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
  ppCode :: Members '[ExactPrint, Reader Options] r => QualifiedName -> Sem r ()
  ppCode QualifiedName {..} = do
    let symbols = _qualifiedPath ^. pathParts NonEmpty.|> _qualifiedSymbol
    dotted (ppSymbolType <$> symbols)

instance SingI t => PrettyPrint (ModuleRef'' 'S.NotConcrete t) where
  ppCode = ppCode @(ModuleRef' 'S.NotConcrete) . project

instance PrettyPrint (ModuleRef'' 'S.Concrete t) where
  ppCode m = ppCode (m ^. moduleRefName)

instance PrettyPrint ScopedIden where
  ppCode = ppCode . (^. scopedIden)

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
        Just as -> Just (ppCode Kw.kwAs <+> ppModulePathType as)

instance PrettyPrint SyntaxDef where
  ppCode = \case
    SyntaxOperator op -> ppCode op
    SyntaxIterator it -> ppCode it

instance PrettyPrint Literal where
  ppCode = noLoc . ppLiteral

ppLiteral :: Literal -> Doc Ann
ppLiteral = \case
  LitInteger n -> annotate AnnLiteralInteger (pretty n)
  LitString s -> ppStringLit s

instance SingI s => PrettyPrint (LambdaClause s) where
  ppCode LambdaClause {..} = do
    let lambdaParameters' = hsep (ppPatternAtom <$> _lambdaParameters)
        lambdaBody' = ppExpressionType _lambdaBody
        lambdaPipe' = ppCode <$> _lambdaPipe ^. unIrrelevant
    lambdaPipe' <?+> lambdaParameters' <+> ppCode _lambdaAssignKw <> oneLineOrNext lambdaBody'

instance SingI s => PrettyPrint (Let s) where
  ppCode Let {..} = do
    let letClauses' = blockIndent (ppBlock _letClauses)
        letExpression' = ppExpressionType _letExpression
    ppCode _letKw <> letClauses' <> ppCode _letInKw <+> letExpression'

instance SingI s => PrettyPrint (Case s) where
  ppCode Case {..} = do
    let exp' = ppExpressionType _caseExpression
        branches' = indent . vsepHard $ fmap ppCode _caseBranches
    parensIf _caseParens (ppCode _caseKw <+> exp' <> hardline <> branches')

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

annSDef :: Members '[ExactPrint] r => S.Name' n -> Sem r () -> Sem r ()
annSDef S.Name' {..} = annotated (AnnDef (_nameDefinedIn ^. S.absTopModulePath) _nameId)

instance SingI s => PrettyPrint (FunctionParameters s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => FunctionParameters s -> Sem r ()
  ppCode FunctionParameters {..} = do
    case _paramNames of
      [] -> ppLeftExpression' funFixity _paramType
      _ -> do
        let paramNames' = map ppCode _paramNames
            paramType' = ppExpressionType _paramType
            delims' = over both ppCode <$> _paramDelims ^. unIrrelevant
            colon' = ppCode (fromJust (_paramColon ^. unIrrelevant))
        delimIf' delims' _paramImplicit True (hsep paramNames' <+> colon' <+> paramType')
    where
      ppLeftExpression' = case sing :: SStage s of
        SParsed -> ppLeftExpression
        SScoped -> ppLeftExpression

instance SingI s => PrettyPrint (FunctionParameter s) where
  ppCode = \case
    FunctionParameterName n -> annDef n (ppSymbolType n)
    FunctionParameterWildcard w -> ppCode w

instance SingI s => PrettyPrint (Function s) where
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
  (PrettyPrint a, HasAtomicity a, Members [Reader Options, ExactPrint] r) =>
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

instance SingI s => PrettyPrint (CaseBranch s) where
  ppCode CaseBranch {..} = do
    let pat' = ppPatternParensType _caseBranchPattern
        e' = ppExpressionType _caseBranchExpression
    ppCode _caseBranchPipe <+> pat' <+> ppCode _caseBranchAssignKw <> oneLineOrNext e'

instance SingI s => PrettyPrint (LetClause s) where
  ppCode c = case c of
    LetTypeSig sig -> ppCode sig
    LetFunClause cl -> ppCode cl

ppBlock :: (PrettyPrint a, Members '[Reader Options, ExactPrint] r, Traversable t) => t a -> Sem r ()
ppBlock items = vsep (sepEndSemicolon (fmap ppCode items))

instance SingI s => PrettyPrint (Lambda s) where
  ppCode Lambda {..} = do
    let lambdaKw' = ppCode _lambdaKw
        braces' = uncurry enclose (over both ppCode (_lambdaBraces ^. unIrrelevant))
        lambdaClauses' = case _lambdaClauses of
          s :| [] -> braces' (ppCode s)
          _ -> braces' (blockIndent (vsepHard (ppCode <$> _lambdaClauses)))
    lambdaKw' <+> lambdaClauses'

instance PrettyPrint Precedence where
  ppCode = \case
    PrecMinusOmega -> noLoc (pretty ("-ω" :: Text))
    PrecNat n -> noLoc (pretty n)
    PrecOmega -> noLoc (pretty ("ω" :: Text))

instance PrettyPrint OperatorSyntaxDef where
  ppCode OperatorSyntaxDef {..} = do
    let opSymbol' = ppUnkindedSymbol _opSymbol
    fi <+> opSymbol'
    where
      fi = do
        let p = ppCode (_opFixity ^. fixityPrecedence)
        ppCode _opSyntaxKw <+> ppCode _opKw <+> p

instance PrettyPrint PatternApp where
  ppCode = apeHelper

instance PrettyPrint Application where
  ppCode = apeHelper

instance PrettyPrint InfixApplication where
  ppCode = apeHelper

instance PrettyPrint PostfixApplication where
  ppCode = apeHelper

instance PrettyPrint IteratorSyntaxDef where
  ppCode IteratorSyntaxDef {..} = do
    let iterSymbol' = ppUnkindedSymbol _iterSymbol
    ppCode _iterSyntaxKw
      <+> ppCode _iterIteratorKw
      <+> iterSymbol'
      <+?> fmap ppCode _iterAttribs

instance PrettyPrint Expression where
  ppCode = \case
    ExpressionIdentifier n -> ppCode n
    ExpressionHole w -> ppCode w
    ExpressionParensIdentifier n -> parens (ppCode n)
    ExpressionBraces b -> braces (ppCode b)
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
    ExpressionIterator i -> ppCode i
    ExpressionNamedApplication i -> ppCode i

instance PrettyPrint (WithSource Pragmas) where
  ppCode pragma = do
    b <- asks (^. optPrintPragmas)
    when b $
      let txt = pretty (Str.pragmasStart <> pragma ^. withSourceText <> Str.pragmasEnd)
       in annotated AnnComment (noLoc txt) <> line

instance PrettyPrint (WithSource IteratorAttribs) where
  ppCode = annotated AnnComment . braces . noLoc . pretty . (^. withSourceText)

ppJudocStart :: Members '[ExactPrint, Reader Options] r => Sem r (Maybe ())
ppJudocStart = do
  inBlock <- asks (^. optInJudocBlock)
  if
      | inBlock -> return Nothing
      | otherwise -> ppCode Kw.delimJudocStart $> Just ()

instance SingI s => PrettyPrint (Example s) where
  ppCode e =
    ppJudocStart
      <??+> ppCode Kw.delimJudocExample
      <+> ppExpressionType (e ^. exampleExpression)
        <> semicolon

instance PrettyPrint a => PrettyPrint (WithLoc a) where
  ppCode a = morphemeM (getLoc a) (ppCode (a ^. withLocParam))

instance SingI s => PrettyPrint (JudocAtom s) where
  ppCode :: forall r. (Members '[Reader Options, ExactPrint] r) => JudocAtom s -> Sem r ()
  ppCode = \case
    JudocExpression e -> goExpression e
    JudocText t -> annotated AnnJudoc (noLoc (pretty t))
    where
      goExpression :: ExpressionType s -> Sem r ()
      goExpression = semiDelim . ppExpressionType
      semiDelim :: Sem r () -> Sem r ()
      semiDelim = enclose1 (annotated AnnJudoc (noLoc (";" :: Doc Ann)))

instance SingI s => PrettyPrint (JudocLine s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => JudocLine s -> Sem r ()
  ppCode (JudocLine deli atoms) = do
    let start' :: Maybe (Sem r ()) = ppCode <$> deli
        atoms' = mapM_ ppCode atoms
    start' <?+> atoms'

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
              (JudocGroupLines {}, JudocGroupLines {}) -> ppCode Kw.delimJudocStart <> line
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
          blockSep = hardline >> ppJudocStart >> hardline

instance SingI s => PrettyPrint (JudocBlock s) where
  ppCode = \case
    JudocLines l -> vsep (ppCode <$> l)
    JudocExample e -> ppCode e

instance SingI s => PrettyPrint (AxiomDef s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => AxiomDef s -> Sem r ()
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

instance SingI s => PrettyPrint (NewFunctionClause s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => NewFunctionClause s -> Sem r ()
  ppCode NewFunctionClause {..} = do
    let pats' = hsep (ppPatternAtomType <$> _clausenPatterns)
        e' = ppExpressionType _clausenBody
    ppCode _clausenPipeKw <+> pats' <+> ppCode _clausenAssignKw <> oneLineOrNext e'

instance SingI s => PrettyPrint (SigArg s) where
  ppCode :: Members '[ExactPrint, Reader Options] r => SigArg s -> Sem r ()
  ppCode SigArg {..} = do
    let Irrelevant (l, r) = _sigArgDelims
        names' = hsep (ppSymbolType <$> _sigArgNames)
        colon' = ppCode _sigArgColon
        ty' = ppExpressionType _sigArgType
    ppCode l <> names' <+> colon' <+> ty' <> ppCode r

instance SingI s => PrettyPrint (FunctionDef s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => FunctionDef s -> Sem r ()
  ppCode FunctionDef {..} = do
    let termin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _signTerminating
        doc' :: Maybe (Sem r ()) = ppCode <$> _signDoc
        pragmas' :: Maybe (Sem r ()) = ppCode <$> _signPragmas
        args' = hsep . fmap ppCode <$> nonEmpty _signArgs
        builtin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _signBuiltin
        type' = oneLineOrNext (ppCode _signColonKw <+> ppExpressionType _signRetType)
        name' = annDef _signName (ppSymbolType _signName)
        body' = case _signBody of
          SigBodyExpression e -> space <> ppCode Kw.kwAssign <> oneLineOrNext (ppExpressionType e)
          SigBodyClauses k -> line <> indent (vsep (ppCode <$> k))
    doc'
      ?<> pragmas'
      ?<> builtin'
      ?<> termin'
      ?<> ( name'
              <+?> args'
                <> type'
                <> body'
          )

instance SingI s => PrettyPrint (TypeSignature s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => TypeSignature s -> Sem r ()
  ppCode TypeSignature {..} = do
    let termin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _sigTerminating
        doc' :: Maybe (Sem r ()) = ppCode <$> _sigDoc
        pragmas' :: Maybe (Sem r ()) = ppCode <$> _sigPragmas
        builtin' :: Maybe (Sem r ()) = (<> line) . ppCode <$> _sigBuiltin
        type' = ppExpressionType _sigType
        name' = annDef _sigName (ppSymbolType _sigName)
        body' = case _sigBody of
          Nothing -> Nothing
          Just body -> Just (ppCode (fromJust <$> _sigAssignKw) <> oneLineOrNext (ppExpressionType body))
    doc'
      ?<> pragmas'
      ?<> builtin'
      ?<> termin'
      ?<> ( name'
              <+> ppCode _sigColonKw
                <> oneLineOrNext
                  ( type'
                      <+?> body'
                  )
          )

instance PrettyPrint Wildcard where
  ppCode w = morpheme (getLoc w) C.kwWildcard

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

ppUnkindedSymbol :: Members '[Reader Options, ExactPrint] r => WithLoc Text -> Sem r ()
ppUnkindedSymbol = region (annotate AnnUnkindedSym) . ppCode

instance SingI s => PrettyPrint (HidingItem s) where
  ppCode h = do
    let sym = ppSymbolType (h ^. hidingSymbol)
        kwmodule = ppCode <$> (h ^. hidingModuleKw)
    kwmodule <?+> sym

instance SingI s => PrettyPrint (HidingList s) where
  ppCode HidingList {..} = do
    let (openb, closeb) = _hidingBraces ^. unIrrelevant
        items' = sequenceWith (semicolon <> space) (ppCode <$> _hidingList)
    ppCode _hidingKw <+> ppCode openb <> items' <> ppCode closeb

instance SingI s => PrettyPrint (UsingList s) where
  ppCode UsingList {..} = do
    let (openb, closeb) = _usingBraces ^. unIrrelevant
        items' = sequenceWith (semicolon <> space) (ppCode <$> _usingList)
    ppCode _usingKw <+> ppCode openb <> items' <> ppCode closeb

instance SingI s => PrettyPrint (UsingHiding s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => UsingHiding s -> Sem r ()
  ppCode = \case
    Using u -> ppCode u
    Hiding h -> ppCode h

instance SingI s => PrettyPrint (UsingItem s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => UsingItem s -> Sem r ()
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

instance SingI s => PrettyPrint (OpenModule s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => OpenModule s -> Sem r ()
  ppCode OpenModule {..} = do
    let name' = ppModuleRefType _openModuleName
        usingHiding' = ppCode <$> _openUsingHiding
        importkw' = ppCode <$> _openModuleImportKw
        openkw = ppCode _openModuleKw
        alias' = (ppCode Kw.kwAs <+>) . ppModulePathType <$> _openImportAsName
        public' = ppCode <$> _openPublicKw ^. unIrrelevant
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
          Just ne -> Just (hsep (ppPatternAtom <$> ne))
        clauseBody' = ppExpressionType _clauseBody
    nest
      ( clauseFun'
          <+?> clausePatterns'
      )
      <+> ppCode _clauseAssignKw
        <> oneLineOrNext clauseBody'

ppCodeAtom :: (HasAtomicity c, PrettyPrint c) => PrettyPrinting c
ppCodeAtom c = do
  let p' = ppCode c
  if isAtomic c then p' else parens p'

ppPatternAtom :: forall s. SingI s => PrettyPrinting (PatternAtomType s)
ppPatternAtom = case sing :: SStage s of
  SParsed -> ppCodeAtom
  SScoped -> \pat ->
    case pat ^. patternArgPattern of
      PatternVariable s | s ^. S.nameVerbatim == "=" -> parens (ppCodeAtom pat)
      _ -> ppCodeAtom pat

instance SingI s => PrettyPrint (InductiveParameters s) where
  ppCode InductiveParameters {..} = do
    let names' = fmap (\nm -> annDef nm (ppSymbolType nm)) _inductiveParametersNames
        ty' = ppExpressionType _inductiveParametersType
    parens (hsep names' <+> ppCode Kw.kwColon <+> ty')

instance SingI s => PrettyPrint (NonEmpty (InductiveParameters s)) where
  ppCode = hsep . fmap ppCode

instance PrettyPrint a => PrettyPrint (Irrelevant a) where
  ppCode (Irrelevant a) = ppCode a

instance SingI s => PrettyPrint (RhsGadt s) where
  ppCode RhsGadt {..} =
    ppCode _rhsGadtColon <+> ppExpressionType _rhsGadtType

instance SingI s => PrettyPrint (RecordField s) where
  ppCode RecordField {..} =
    ppSymbolType _fieldName <+> ppCode _fieldColon <+> ppExpressionType _fieldType

instance SingI s => PrettyPrint (RhsRecord s) where
  ppCode RhsRecord {..} = do
    let Irrelevant (l, r) = _rhsRecordDelim
        fields'
          | null (_rhsRecordFields ^. _tail1) = ppCode (_rhsRecordFields ^. _head1)
          | otherwise =
              line
                <> indent
                  ( sequenceWith
                      (semicolon >> line)
                      (ppCode <$> _rhsRecordFields)
                  )
                <> line
    ppCode l <> fields' <> ppCode r

instance SingI s => PrettyPrint (ConstructorRhs s) where
  ppCode = \case
    ConstructorRhsGadt r -> ppCode r
    ConstructorRhsRecord r -> ppCode r

instance SingI s => PrettyPrint (ConstructorDef s) where
  ppCode :: forall r. Members '[ExactPrint, Reader Options] r => ConstructorDef s -> Sem r ()
  ppCode ConstructorDef {..} = do
    let constructorName' = annDef _constructorName (ppSymbolType _constructorName)
        constructorRhs' = ppCode _constructorRhs
        doc' = ppCode <$> _constructorDoc
        pragmas' = ppCode <$> _constructorPragmas
    pipeHelper <+> nest (doc' ?<> pragmas' ?<> constructorName' <+> constructorRhs')
    where
      -- we use this helper so that comments appear before the first optional pipe if the pipe was omitted
      pipeHelper :: Sem r ()
      pipeHelper = case _constructorPipe ^. unIrrelevant of
        Just p -> ppCode p
        Nothing -> ppCode Kw.kwPipe

ppInductiveSignature :: SingI s => PrettyPrinting (InductiveDef s)
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
      <+> ppCode _inductiveAssignKw
        <> line
        <> indent constrs'
    where
      ppConstructorBlock :: NonEmpty (ConstructorDef s) -> Sem r ()
      ppConstructorBlock cs = vsep (ppCode <$> cs)

instance SingI s => PrettyPrint (ProjectionDef s) where
  ppCode ProjectionDef {..} =
    do
      ppSymbolType _projectionField
      <+> noLoc ":= projection"
      <+> noLoc (pretty _projectionFieldIx)
      <+> noLoc "for"
      <+> ppCode _projectionConstructor

instance SingI s => PrettyPrint (Statement s) where
  ppCode = \case
    StatementSyntax s -> ppCode s
    StatementTypeSignature s -> ppCode s
    StatementFunctionDef f -> ppCode f
    StatementImport i -> ppCode i
    StatementInductive i -> ppCode i
    StatementModule m -> ppCode m
    StatementOpenModule o -> ppCode o
    StatementFunctionClause c -> ppCode c
    StatementAxiom a -> ppCode a
    StatementProjectionDef a -> ppCode a

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
