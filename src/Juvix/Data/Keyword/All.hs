module Juvix.Data.Keyword.All
  ( module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Extra.Strings qualified as Str

kwAs :: Keyword
kwAs = asciiKw Str.as

kwBuiltin :: Keyword
kwBuiltin = asciiKw Str.builtin

kwBottom :: Keyword
kwBottom = unicodeKw Str.bottomAscii Str.bottom

kwAny :: Keyword
kwAny = asciiKw Str.any

kwAssign :: Keyword
kwAssign = asciiKw Str.assignAscii

kwExclamation :: Keyword
kwExclamation = asciiKw Str.exclamation

kwAt :: Keyword
kwAt = asciiKw Str.at_

kwAxiom :: Keyword
kwAxiom = asciiKw Str.axiom

kwColon :: Keyword
kwColon = asciiKw Str.colon

kwEnd :: Keyword
kwEnd = asciiKw Str.end

kwHiding :: Keyword
kwHiding = asciiKw Str.hiding

kwImport :: Keyword
kwImport = asciiKw Str.import_

kwIn :: Keyword
kwIn = asciiKw Str.in_

kwInductive :: Keyword
kwInductive = asciiKw Str.inductive

kwInfix :: Keyword
kwInfix = asciiKw Str.infix_

kwInfixl :: Keyword
kwInfixl = asciiKw Str.infixl_

kwInfixr :: Keyword
kwInfixr = asciiKw Str.infixr_

kwLambda :: Keyword
kwLambda = unicodeKw Str.lambdaAscii Str.lambdaUnicode

kwPi :: Keyword
kwPi = unicodeKw Str.piAscii Str.piUnicode

kwLet :: Keyword
kwLet = asciiKw Str.let_

kwMapsTo :: Keyword
kwMapsTo = unicodeKw Str.mapstoAscii Str.mapstoUnicode

kwModule :: Keyword
kwModule = asciiKw Str.module_

kwOpen :: Keyword
kwOpen = asciiKw Str.open

kwPostfix :: Keyword
kwPostfix = asciiKw Str.postfix

kwPublic :: Keyword
kwPublic = asciiKw Str.public

kwRightArrow :: Keyword
kwRightArrow = unicodeKw Str.toAscii Str.toUnicode

kwSyntax :: Keyword
kwSyntax = asciiKw Str.syntax

kwIterator :: Keyword
kwIterator = asciiKw Str.iterator

kwPipe :: Keyword
kwPipe = asciiKw Str.pipe

kwType :: Keyword
kwType = asciiKw Str.type_

kwTerminating :: Keyword
kwTerminating = asciiKw Str.terminating

kwPositive :: Keyword
kwPositive = asciiKw Str.positive

kwUsing :: Keyword
kwUsing = asciiKw Str.using

kwWhere :: Keyword
kwWhere = asciiKw Str.where_

kwHole :: Keyword
kwHole = asciiKw Str.underscore

kwWildcard :: Keyword
kwWildcard = asciiKw Str.underscore

kwLetRec :: Keyword
kwLetRec = asciiKw Str.letrec_

kwCase :: Keyword
kwCase = asciiKw Str.case_

kwOf :: Keyword
kwOf = asciiKw Str.of_

kwMatch :: Keyword
kwMatch = asciiKw Str.match_

kwWith :: Keyword
kwWith = asciiKw Str.with_

kwIf :: Keyword
kwIf = asciiKw Str.if_

kwThen :: Keyword
kwThen = asciiKw Str.then_

kwElse :: Keyword
kwElse = asciiKw Str.else_

kwDef :: Keyword
kwDef = asciiKw Str.def

kwComma :: Keyword
kwComma = asciiKw Str.comma

kwPlus :: Keyword
kwPlus = asciiKw Str.plus

kwMinus :: Keyword
kwMinus = asciiKw Str.minus

kwMul :: Keyword
kwMul = asciiKw Str.mul

kwDiv :: Keyword
kwDiv = asciiKw Str.div

kwMod :: Keyword
kwMod = asciiKw Str.mod

kwEq :: Keyword
kwEq = asciiKw Str.equal

kwLt :: Keyword
kwLt = asciiKw Str.less

kwLe :: Keyword
kwLe = asciiKw Str.lessEqual

kwGt :: Keyword
kwGt = asciiKw Str.greater

kwGe :: Keyword
kwGe = asciiKw Str.greaterEqual

kwShow :: Keyword
kwShow = asciiKw Str.show_

kwStrConcat :: Keyword
kwStrConcat = asciiKw Str.strConcat

kwStrToInt :: Keyword
kwStrToInt = asciiKw Str.strToInt

kwBind :: Keyword
kwBind = asciiKw Str.bind

kwSeq :: Keyword
kwSeq = asciiKw Str.seq_

kwSeqq :: Keyword
kwSeqq = asciiKw Str.seqq_

kwTrace :: Keyword
kwTrace = asciiKw Str.trace_

kwFail :: Keyword
kwFail = asciiKw Str.fail_

kwErr :: Keyword
kwErr = asciiKw Str.err

kwList :: Keyword
kwList = asciiKw Str.list

kwFun :: Keyword
kwFun = asciiKw Str.fun_

kwStar :: Keyword
kwStar = asciiKw Str.mul

kwTrue :: Keyword
kwTrue = asciiKw Str.true_

kwFalse :: Keyword
kwFalse = asciiKw Str.false_

kwArg :: Keyword
kwArg = asciiKw Str.arg

kwTmp :: Keyword
kwTmp = asciiKw Str.tmp

kwUnit :: Keyword
kwUnit = asciiKw Str.unit

kwVoid :: Keyword
kwVoid = asciiKw Str.void

kwDollar :: Keyword
kwDollar = asciiKw Str.dollar

kwMutual :: Keyword
kwMutual = asciiKw Str.mutual

kwBracketL :: Keyword
kwBracketL = asciiKw Str.bracketL

kwBracketR :: Keyword
kwBracketR = asciiKw Str.bracketR

kwSP :: Keyword
kwSP = asciiKw Str.sp

kwHP :: Keyword
kwHP = asciiKw Str.hp

delimBraceL :: Keyword
delimBraceL = mkDelim Str.braceL

delimBraceR :: Keyword
delimBraceR = mkDelim Str.braceR

delimParenL :: Keyword
delimParenL = mkDelim Str.parenL

delimParenR :: Keyword
delimParenR = mkDelim Str.parenR

delimJudocExample :: Keyword
delimJudocExample = mkJudocDelim Str.judocExample

delimJudocStart :: Keyword
delimJudocStart = mkJudocDelim Str.judocStart

delimJudocBlockStart :: Keyword
delimJudocBlockStart = mkJudocDelim Str.judocBlockStart

delimJudocBlockEnd :: Keyword
delimJudocBlockEnd = mkJudocDelim Str.judocBlockEnd

delimSemicolon :: Keyword
delimSemicolon = mkDelim Str.semicolon
