module Juvix.Data.CodeAnn (
module Juvix.Data.CodeAnn ,
module Juvix.Prelude.Pretty,
                          ) where

import Juvix.Compiler.Concrete.Data
import Juvix.Prelude
import Juvix.Prelude.Pretty hiding (parens, braces)
import Prettyprinter.Render.Terminal
import Juvix.Extra.Strings qualified as Str

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

kwParenL :: Doc Ann
kwParenL = delimiter "("

kwParenR :: Doc Ann
kwParenR = delimiter ")"

kwDQuote :: Doc Ann
kwDQuote = pretty ("\"" :: Text)

kwDot :: Doc Ann
kwDot = delimiter "."

braces :: Doc Ann -> Doc Ann
braces = enclose kwBraceL kwBraceR

parens :: Doc Ann -> Doc Ann
parens = enclose kwParenL kwParenR

implicitDelim :: IsImplicit -> Doc Ann -> Doc Ann
implicitDelim = \case
  Implicit -> braces
  Explicit -> parens

doubleQuotes :: Doc Ann -> Doc Ann
doubleQuotes = enclose kwDQuote kwDQuote

annotateKind :: NameKind -> Doc Ann -> Doc Ann
annotateKind = annotate . AnnKind
