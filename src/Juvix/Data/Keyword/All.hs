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

kwAtQuestion :: Keyword
kwAtQuestion = asciiKw Str.atQuestion

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

kwOperator :: Keyword
kwOperator = asciiKw Str.operator

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

kwPublic :: Keyword
kwPublic = asciiKw Str.public

kwRightArrow :: Keyword
kwRightArrow = unicodeKw Str.toAscii Str.toUnicode

kwSyntax :: Keyword
kwSyntax = asciiKw Str.syntax

kwInit :: Keyword
kwInit = asciiKw Str.init

kwRange :: Keyword
kwRange = asciiKw Str.range

kwAssoc :: Keyword
kwAssoc = asciiKw Str.assoc

kwNone :: Keyword
kwNone = asciiKw Str.none

kwNop :: Keyword
kwNop = asciiKw Str.nop

kwRight :: Keyword
kwRight = asciiKw Str.right

kwLeft :: Keyword
kwLeft = asciiKw Str.left

kwUnary :: Keyword
kwUnary = asciiKw Str.unary

kwBinary :: Keyword
kwBinary = asciiKw Str.binary

kwSame :: Keyword
kwSame = asciiKw Str.same

kwBelow :: Keyword
kwBelow = asciiKw Str.below

kwAbove :: Keyword
kwAbove = asciiKw Str.above

kwAlias :: Keyword
kwAlias = asciiKw Str.alias

kwFixity :: Keyword
kwFixity = asciiKw Str.fixity

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

kwTrait :: Keyword
kwTrait = asciiKw Str.trait

kwInstance :: Keyword
kwInstance = asciiKw Str.instance_

kwCoercion :: Keyword
kwCoercion = asciiKw Str.coercion_

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

kwNotEq :: Keyword
kwNotEq = asciiKw Str.notequal

kwPlusEq :: Keyword
kwPlusEq = asciiKw Str.plusequal

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

kwAtoi :: Keyword
kwAtoi = asciiKw Str.instrStrToInt

kwStrcat :: Keyword
kwStrcat = asciiKw Str.instrStrConcat

kwBind :: Keyword
kwBind = asciiKw Str.bind

kwSeq :: Keyword
kwSeq = asciiKw Str.seq_

kwSeqq :: Keyword
kwSeqq = asciiKw Str.seqq_

kwSSeq :: Keyword
kwSSeq = asciiKw Str.sseq_

kwTrace :: Keyword
kwTrace = asciiKw Str.trace_

kwFail :: Keyword
kwFail = asciiKw Str.fail_

kwDump :: Keyword
kwDump = asciiKw Str.dump

kwPrealloc :: Keyword
kwPrealloc = asciiKw Str.prealloc

kwArgsNum :: Keyword
kwArgsNum = asciiKw Str.instrArgsNum

kwPoseidon :: Keyword
kwPoseidon = asciiKw Str.instrPoseidon

kwEcOp :: Keyword
kwEcOp = asciiKw Str.instrEcOp

kwRandomEcPoint :: Keyword
kwRandomEcPoint = asciiKw Str.cairoRandomEcPoint

kwAlloc :: Keyword
kwAlloc = asciiKw Str.instrAlloc

kwCAlloc :: Keyword
kwCAlloc = asciiKw Str.instrCalloc

kwCExtend :: Keyword
kwCExtend = asciiKw Str.instrCextend

kwCCall :: Keyword
kwCCall = asciiKw Str.instrCcall

kwCCallTail :: Keyword
kwCCallTail = asciiKw Str.instrTccall

kwBr :: Keyword
kwBr = asciiKw Str.instrBr

kwSave :: Keyword
kwSave = asciiKw Str.save

kwDefault :: Keyword
kwDefault = asciiKw Str.default_

kwIntAdd :: Keyword
kwIntAdd = asciiKw Str.iadd

kwIntSub :: Keyword
kwIntSub = asciiKw Str.isub

kwIntMul :: Keyword
kwIntMul = asciiKw Str.imul

kwIntDiv :: Keyword
kwIntDiv = asciiKw Str.idiv

kwIntMod :: Keyword
kwIntMod = asciiKw Str.imod

kwIntLt :: Keyword
kwIntLt = asciiKw Str.ilt

kwIntLe :: Keyword
kwIntLe = asciiKw Str.ile

kwAdd_ :: Keyword
kwAdd_ = asciiKw Str.add_

kwSub_ :: Keyword
kwSub_ = asciiKw Str.sub_

kwMul_ :: Keyword
kwMul_ = asciiKw Str.mul_

kwDiv_ :: Keyword
kwDiv_ = asciiKw Str.div_

kwMod_ :: Keyword
kwMod_ = asciiKw Str.mod_

kwLt_ :: Keyword
kwLt_ = asciiKw Str.lt_

kwLe_ :: Keyword
kwLe_ = asciiKw Str.le_

kwFieldAdd :: Keyword
kwFieldAdd = asciiKw Str.fadd

kwFieldSub :: Keyword
kwFieldSub = asciiKw Str.fsub

kwFieldMul :: Keyword
kwFieldMul = asciiKw Str.fmul

kwFieldDiv :: Keyword
kwFieldDiv = asciiKw Str.fdiv

kwSeq_ :: Keyword
kwSeq_ = asciiKw Str.sseq_

kwEq_ :: Keyword
kwEq_ = asciiKw Str.eq

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

kwAp :: Keyword
kwAp = asciiKw Str.ap

kwFp :: Keyword
kwFp = asciiKw Str.fp

kwApPlusPlus :: Keyword
kwApPlusPlus = asciiKw Str.apPlusPlus

kwRel :: Keyword
kwRel = asciiKw Str.rel

kwAbs :: Keyword
kwAbs = asciiKw Str.abs

kwJmp :: Keyword
kwJmp = asciiKw Str.jmp

kwCall :: Keyword
kwCall = asciiKw Str.call

kwCallTail :: Keyword
kwCallTail = asciiKw Str.tcall

kwRet :: Keyword
kwRet = asciiKw Str.ret

kwLive :: Keyword
kwLive = asciiKw Str.live

kwAnomaGet :: Keyword
kwAnomaGet = asciiKw Str.anomaGet

kwAnomaDecode :: Keyword
kwAnomaDecode = asciiKw Str.anomaDecode

kwAnomaEncode :: Keyword
kwAnomaEncode = asciiKw Str.anomaEncode

kwAnomaVerifyDetached :: Keyword
kwAnomaVerifyDetached = asciiKw Str.anomaVerifyDetached

kwAnomaSign :: Keyword
kwAnomaSign = asciiKw Str.anomaSign

delimBraceL :: Keyword
delimBraceL = mkDelim Str.braceL

delimBraceR :: Keyword
delimBraceR = mkDelim Str.braceR

delimDoubleBraceL :: Keyword
delimDoubleBraceL = mkDelim Str.doubleBraceL

delimDoubleBraceR :: Keyword
delimDoubleBraceR = mkDelim Str.doubleBraceR

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
