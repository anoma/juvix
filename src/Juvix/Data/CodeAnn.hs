module Juvix.Data.CodeAnn
  ( module Juvix.Data.CodeAnn,
    module Juvix.Prelude.Pretty,
  )
where

import Juvix.Compiler.Concrete.Data
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty hiding (braces, parens)
import Prettyprinter.Render.Terminal (Color (..), bold, colorDull)

type Ann = CodeAnn

data CodeAnn
  = AnnKind NameKind
  | AnnKeyword
  | AnnCode
  | AnnComment
  | AnnImportant
  | AnnDelimiter
  | AnnLiteralString
  | AnnLiteralInteger
  | AnnUnkindedSym
  | AnnDef TopModulePath NameId
  | AnnRef TopModulePath NameId

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
  AnnDelimiter -> colorDull White
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
  AnnDef {} -> mempty
  AnnRef {} -> mempty

keyword :: Text -> Doc Ann
keyword = annotate AnnKeyword . pretty

kwLambda :: Doc Ann
kwLambda = keyword Str.lambdaUnicode

kwInclude :: Doc Ann
kwInclude = keyword Str.include

kwArrow :: Doc Ann
kwArrow = keyword Str.toUnicode

kwMapsto :: Doc Ann
kwMapsto = keyword Str.mapstoUnicode

kwForeign :: Doc Ann
kwForeign = keyword Str.foreign_

kwCompile :: Doc Ann
kwCompile = keyword Str.compile

kwC :: Doc Ann
kwC = keyword Str.cBackend

kwGhc :: Doc Ann
kwGhc = keyword Str.ghc

kwColon :: Doc Ann
kwColon = keyword Str.colon

kwData :: Doc Ann
kwData = keyword Str.data_

kwAssign :: Doc Ann
kwAssign = keyword Str.assignUnicode

kwEquals :: Doc Ann
kwEquals = keyword Str.equal

kwColonColon :: Doc Ann
kwColonColon = keyword (Str.colon <> Str.colon)

kwPipe :: Doc Ann
kwPipe = keyword Str.pipe

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

kwNatural :: Doc Ann
kwNatural = keyword Str.natural

kwInductive :: Doc Ann
kwInductive = keyword Str.inductive

kwArrowR :: Doc Ann
kwArrowR = keyword Str.toUnicode

kwLet :: Doc Ann
kwLet = keyword Str.let_

kwIn :: Doc Ann
kwIn = keyword Str.in_

kwPublic :: Doc Ann
kwPublic = keyword Str.public

kwPostfix :: Doc Ann
kwPostfix = keyword Str.postfix

kwInfixr :: Doc Ann
kwInfixr = keyword Str.infixr_

kwInfixl :: Doc Ann
kwInfixl = keyword Str.infixl_

kwInfix :: Doc Ann
kwInfix = keyword Str.infix_

kwAssignment :: Doc Ann
kwAssignment = keyword Str.assignUnicode

kwColonZero :: Doc Ann
kwColonZero = keyword Str.colonZero

kwColonOne :: Doc Ann
kwColonOne = keyword Str.colonOne

kwColonOmega :: Doc Ann
kwColonOmega = keyword Str.colonOmegaUnicode

kwOpen :: Doc Ann
kwOpen = keyword Str.open

kwUsing :: Doc Ann
kwUsing = keyword Str.using

kwHiding :: Doc Ann
kwHiding = keyword Str.hiding

kwImport :: Doc Ann
kwImport = keyword Str.import_

delimiter :: Text -> Doc Ann
delimiter = annotate AnnDelimiter . pretty

kwSemicolon :: Doc Ann
kwSemicolon = delimiter Str.semicolon

kwTerminating :: Doc Ann
kwTerminating = keyword Str.terminating

kwBraceL :: Doc Ann
kwBraceL = delimiter "{"

kwBraceR :: Doc Ann
kwBraceR = delimiter "}"

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

code :: Doc Ann -> Doc Ann
code = annotate AnnCode

braces :: Doc Ann -> Doc Ann
braces = enclose kwBraceL kwBraceR

parens :: Doc Ann -> Doc Ann
parens = enclose kwParenL kwParenR

bracesIf :: Bool -> Doc Ann -> Doc Ann
bracesIf t = if t then braces else id

implicitDelim :: IsImplicit -> Doc Ann -> Doc Ann
implicitDelim = \case
  Implicit -> braces
  Explicit -> parens

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
ppStringLit = annotate AnnLiteralString . doubleQuotes . escaped
  where
    showChar :: Char -> String
    showChar c = showLitChar c ("" :: String)
    escaped :: Text -> Doc a
    escaped = mconcatMap (pretty . showChar) . unpack

bracesIndent :: Doc Ann -> Doc Ann
bracesIndent d = braces (line <> indent' d <> line)
