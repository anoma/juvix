module Juvix.Data.Keyword.All
  ( module Juvix.Data.Keyword,
    module Juvix.Data.Keyword.All,
  )
where

import Juvix.Data.Keyword
import Juvix.Extra.Strings qualified as Str

kwBuiltin :: Keyword
kwBuiltin = asciiKw Str.builtin

kwAny :: Keyword
kwAny = asciiKw Str.any

kwAssign :: Keyword
kwAssign = asciiKw Str.assignAscii

kwAt :: Keyword
kwAt = asciiKw Str.at_

kwAxiom :: Keyword
kwAxiom = asciiKw Str.axiom

kwColon :: Keyword
kwColon = asciiKw Str.colon

kwColonOmega :: Keyword
kwColonOmega = unicodeKw Str.colonOmegaAscii Str.colonOmegaUnicode

kwColonOne :: Keyword
kwColonOne = asciiKw Str.colonOne

kwColonZero :: Keyword
kwColonZero = asciiKw Str.colonZero

kwCompile :: Keyword
kwCompile = asciiKw Str.compile

kwEnd :: Keyword
kwEnd = asciiKw Str.end

kwHiding :: Keyword
kwHiding = asciiKw Str.hiding

kwImport :: Keyword
kwImport = asciiKw Str.import_

kwForeign :: Keyword
kwForeign = asciiKw Str.foreign_

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

kwSemicolon :: Keyword
kwSemicolon = asciiKw Str.semicolon

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

ghc :: Keyword
ghc = asciiKw Str.ghc

cBackend :: Keyword
cBackend = asciiKw Str.cBackend

kwLetRec :: Keyword
kwLetRec = asciiKw Str.letrec_

kwConstr :: Keyword
kwConstr = asciiKw Str.constr

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

kwBind :: Keyword
kwBind = asciiKw Str.bind

kwSeq :: Keyword
kwSeq = asciiKw Str.seq_

kwTrace :: Keyword
kwTrace = asciiKw Str.trace_

kwFail :: Keyword
kwFail = asciiKw Str.fail_

kwFun :: Keyword
kwFun = asciiKw Str.fun_

kwStar :: Keyword
kwStar = asciiKw Str.mul

kwTrue :: Keyword
kwTrue = asciiKw Str.true_

kwFalse :: Keyword
kwFalse = asciiKw Str.false_

kwArg :: Keyword
kwArg = asciiKw Str.arg_

kwTmp :: Keyword
kwTmp = asciiKw Str.tmp_

kwUnit :: Keyword
kwUnit = asciiKw Str.unit

kwVoid :: Keyword
kwVoid = asciiKw Str.void

kwDollar :: Keyword
kwDollar = asciiKw Str.dollar
