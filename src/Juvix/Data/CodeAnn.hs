module Juvix.Data.CodeAnn
  ( module Juvix.Data.CodeAnn,
    module Juvix.Data.NameKind,
    module Juvix.Prelude.Pretty,
    module Juvix.Data.CodeReference,
    module Data.Versions,
  )
where

import Data.Versions (prettySemVer)
import Juvix.Data.CodeReference
import Juvix.Data.Error.GenericError
import Juvix.Data.IsImplicit
import Juvix.Data.Keyword
import Juvix.Data.NameKind
import Juvix.Data.PackageId
import Juvix.Data.WithLoc
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty hiding (braces, brackets, group, list, parens)
import Prettyprinter.Render.Terminal (Color (..), bold, colorDull)

type Ann = CodeAnn

data CodeAnn
  = AnnKind NameKind
  | AnnKeyword
  | AnnCode
  | AnnComment
  | AnnPragma
  | AnnJudoc
  | AnnImportant
  | AnnDelimiter
  | AnnError
  | AnnLiteralString
  | AnnLiteralInteger
  | AnnUnkindedSym
  | AnnDef CodeReference
  | AnnRef CodeReference

type SemanticItem = WithLoc CodeAnn

instance PrettyCodeAnn PackageId where
  ppCodeAnn pid =
    annotate AnnImportant (pretty (pid ^. packageIdName))
      <+> pretty (prettySemVer (pid ^. packageIdVersion))

instance HasNameKindAnn Ann where
  annNameKind = AnnKind

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnUnkindedSym -> mempty
  AnnKeyword -> colorDull Blue
  AnnCode -> bold
  AnnImportant -> bold
  AnnComment -> colorDull Cyan
  AnnPragma -> colorDull Cyan
  AnnJudoc -> colorDull Cyan
  AnnDelimiter -> colorDull White
  AnnLiteralString -> colorDull Red
  AnnError -> colorDull Red
  AnnLiteralInteger -> colorDull Green
  AnnDef {} -> mempty
  AnnRef {} -> mempty

class PrettyCodeAnn a where
  ppCodeAnn :: a -> Doc CodeAnn

instance HasAnsiBackend (Doc CodeAnn) where
  toAnsiDoc = fmap stylize
  toAnsiStream = fmap stylize . layoutPretty defaultLayoutOptions

instance PrettyCodeAnn Keyword where
  ppCodeAnn = annotate AnnKeyword . pretty

simpleErrorCodeAnn :: (PrettyCodeAnn msg) => msg -> SimpleError
simpleErrorCodeAnn = SimpleError . mkAnsiText . ppCodeAnn

kwTypeAnn :: KeywordType -> CodeAnn
kwTypeAnn = \case
  KeywordTypeDelimiter -> AnnDelimiter
  KeywordTypeKeyword -> AnnKeyword
  KeywordTypeJudoc -> AnnJudoc

-- | for builtin stuff
primitive :: Text -> Doc Ann
primitive = annotate (AnnKind KNameAxiom) . pretty

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

constructor :: Text -> Doc Ann
constructor = annotate (AnnKind KNameConstructor) . pretty

kwNotMutual :: Doc Ann
kwNotMutual = keyword Str.notMutual

kwMutual :: Doc Ann
kwMutual = keyword Str.mutual

kwSimpleLambda :: Doc Ann
kwSimpleLambda = keyword (Str.lambdaUnicode <> Str.exclamation)

kwLambda :: Doc Ann
kwLambda = keyword Str.lambdaUnicode

kwCase :: Doc Ann
kwCase = keyword Str.case_

kwCaseOn :: Doc Ann
kwCaseOn = keyword Str.caseOn

kwInclude :: Doc Ann
kwInclude = keyword Str.include

kwArrow :: Doc Ann
kwArrow = keyword Str.toAscii

kwMapsto :: Doc Ann
kwMapsto = keyword Str.mapstoUnicode

kwColon :: Doc Ann
kwColon = keyword Str.colon

kwData :: Doc Ann
kwData = keyword Str.data_

kwIf :: Doc Ann
kwIf = keyword Str.if_

kwAssign :: Doc Ann
kwAssign = keyword Str.assignAscii

kwEquals :: Doc Ann
kwEquals = keyword Str.equal

kwColonColon :: Doc Ann
kwColonColon = keyword (Str.colon <> Str.colon)

kwPipe :: Doc Ann
kwPipe = delimiter Str.pipe

kwHole :: Doc Ann
kwHole = keyword Str.underscore

kwAxiom :: Doc Ann
kwAxiom = keyword Str.axiom

kwWhere :: Doc Ann
kwWhere = keyword Str.where_

kwModule :: Doc Ann
kwModule = keyword Str.module_

kwType :: Doc Ann
kwType = keyword Str.type_

kwWildcard :: Doc Ann
kwWildcard = keyword Str.underscore

kwEnd :: Doc Ann
kwEnd = keyword Str.end

kwBuiltin :: Doc Ann
kwBuiltin = keyword Str.builtin

kwInductive :: Doc Ann
kwInductive = keyword Str.inductive

kwLet :: Doc Ann
kwLet = keyword Str.let_

kwIn :: Doc Ann
kwIn = keyword Str.in_

kwPublic :: Doc Ann
kwPublic = keyword Str.public

kwSyntax :: Doc Ann
kwSyntax = keyword Str.syntax

kwIterator :: Doc Ann
kwIterator = keyword Str.iterator

kwPostfix :: Doc Ann
kwPostfix = keyword Str.postfix

kwInfixr :: Doc Ann
kwInfixr = keyword Str.infixr_

kwInfixl :: Doc Ann
kwInfixl = keyword Str.infixl_

kwInfix :: Doc Ann
kwInfix = keyword Str.infix_

kwOpen :: Doc Ann
kwOpen = keyword Str.open

kwUsing :: Doc Ann
kwUsing = keyword Str.using

kwHiding :: Doc Ann
kwHiding = keyword Str.hiding

kwAs :: Doc Ann
kwAs = keyword Str.as

kwImport :: Doc Ann
kwImport = keyword Str.import_

delimiter :: Text -> Doc Ann
delimiter = annotate AnnDelimiter . pretty

kwSemicolon :: Doc Ann
kwSemicolon = delimiter Str.semicolon

kwOf :: Doc Ann
kwOf = keyword Str.of_

kwTerminating :: Doc Ann
kwTerminating = keyword Str.terminating

kwPositive :: Doc Ann
kwPositive = keyword Str.positive

kwBraceL :: Doc Ann
kwBraceL = delimiter "{"

kwBraceR :: Doc Ann
kwBraceR = delimiter "}"

kwDoubleBraceL :: Doc Ann
kwDoubleBraceL = delimiter "{{"

kwDoubleBraceR :: Doc Ann
kwDoubleBraceR = delimiter "}}"

kwBracketL :: Doc Ann
kwBracketL = delimiter "["

kwBracketR :: Doc Ann
kwBracketR = delimiter "]"

kwParenL :: Doc Ann
kwParenL = delimiter "("

kwParenR :: Doc Ann
kwParenR = delimiter ")"

kwDQuote :: Doc Ann
kwDQuote = pretty ("\"" :: Text)

kwDot :: Doc Ann
kwDot = delimiter "."

kwAt :: Doc Ann
kwAt = keyword Str.at_

important :: Doc Ann -> Doc Ann
important = annotate AnnImportant

code :: Doc Ann -> Doc Ann
code = annotate AnnCode

brackets :: Doc Ann -> Doc Ann
brackets = enclose kwBracketL kwBracketR

braces :: Doc Ann -> Doc Ann
braces = enclose kwBraceL kwBraceR

doubleBraces :: Doc Ann -> Doc Ann
doubleBraces = enclose kwDoubleBraceL kwDoubleBraceR

parens :: Doc Ann -> Doc Ann
parens = enclose kwParenL kwParenR

bracesIf :: Bool -> Doc Ann -> Doc Ann
bracesIf t = if t then braces else id

parensIf :: Bool -> Doc Ann -> Doc Ann
parensIf t = if t then parens else id

implicitDelim :: IsImplicit -> Doc Ann -> Doc Ann
implicitDelim = \case
  Implicit -> braces
  Explicit -> parens
  ImplicitInstance -> doubleBraces

delimIf :: IsImplicit -> Bool -> Doc Ann -> Doc Ann
delimIf Implicit _ = braces
delimIf ImplicitInstance _ = doubleBraces
delimIf Explicit True = parens
delimIf Explicit False = id

doubleQuotes :: Doc Ann -> Doc Ann
doubleQuotes = enclose kwDQuote kwDQuote

annotateKind :: NameKind -> Doc Ann -> Doc Ann
annotateKind = annotate . AnnKind

parensCond :: Bool -> Doc Ann -> Doc Ann
parensCond t d = if t then parens d else d

bracesCond :: Bool -> Doc Ann -> Doc Ann
bracesCond t d = if t then braces d else d

endSemicolon :: Doc Ann -> Doc Ann
endSemicolon x = x <> kwSemicolon

ppStringLit :: Text -> Doc Ann
ppStringLit = annotate AnnLiteralString . pretty . pack . urecover . show

bracesIndent :: Doc Ann -> Doc Ann
bracesIndent = braces . blockIndent

blockIndent :: Doc Ann -> Doc Ann
blockIndent d = hardline <> indent' d <> line

commaSep :: (Foldable f) => f (Doc Ann) -> Doc Ann
commaSep ts = mconcat (intersperse (delimiter "," <> " ") (toList ts))
