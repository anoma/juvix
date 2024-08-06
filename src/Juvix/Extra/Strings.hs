module Juvix.Extra.Strings where

import Juvix.Prelude.Base

auto :: (IsString s) => s
auto = "auto"

binaryPrefix :: (IsString s) => s
binaryPrefix = "0b"

hexadecimalPrefix :: (IsString s) => s
hexadecimalPrefix = "0x"

octalPrefix :: (IsString s) => s
octalPrefix = "0o"

module_ :: (IsString s) => s
module_ = "module"

axiom :: (IsString s) => s
axiom = "axiom"

commentLineStart :: (IsString s) => s
commentLineStart = "--"

commentBlockStart :: (IsString s) => s
commentBlockStart = "{-"

commentBlockEnd :: (IsString s) => s
commentBlockEnd = "-}"

judocBlockStart :: (IsString s) => s
judocBlockStart = "{--"

judocBlockEnd :: (IsString s) => s
judocBlockEnd = "--}"

judocStart :: (IsString s) => s
judocStart = "---"

judocExample :: (IsString s) => s
judocExample = ">>>"

pragmasStart :: (IsString s) => s
pragmasStart = "{-#"

pragmasEnd :: (IsString s) => s
pragmasEnd = "#-}"

bracketL :: (IsString s) => s
bracketL = "["

bracketR :: (IsString s) => s
bracketR = "]"

braceL :: (IsString s) => s
braceL = "{"

braceR :: (IsString s) => s
braceR = "}"

doubleBraceL :: (IsString s) => s
doubleBraceL = "{{"

doubleBraceR :: (IsString s) => s
doubleBraceR = "}}"

parenL :: (IsString s) => s
parenL = "("

parenR :: (IsString s) => s
parenR = ")"

end :: (IsString s) => s
end = "end"

eval :: (IsString s) => s
eval = "eval"

hiding :: (IsString s) => s
hiding = "hiding"

include :: (IsString s) => s
include = "include"

alias :: (IsString s) => s
alias = "alias"

pragma :: (IsString s) => s
pragma = "pragma"

import_ :: (IsString s) => s
import_ = "import"

in_ :: (IsString s) => s
in_ = "in"

inductive :: (IsString s) => s
inductive = "type"

function :: (IsString s) => s
function = "function"

variable :: (IsString s) => s
variable = "variable"

constructor :: (IsString s) => s
constructor = "constructor"

topModule :: (IsString s) => s
topModule = "top module"

localModule :: (IsString s) => s
localModule = "local module"

infix_ :: (IsString s) => s
infix_ = "infix"

infixl_ :: (IsString s) => s
infixl_ = "infixl"

infixr_ :: (IsString s) => s
infixr_ = "infixr"

operator :: (IsString s) => s
operator = "operator"

open :: (IsString s) => s
open = "open"

syntax :: (IsString s) => s
syntax = "syntax"

below :: (IsString s) => s
below = "below"

above :: (IsString s) => s
above = "above"

assoc :: (IsString s) => s
assoc = "assoc"

fixity :: (IsString s) => s
fixity = "fixity"

iterator :: (IsString s) => s
iterator = "iterator"

postfix :: (IsString s) => s
postfix = "postfix"

let_ :: (IsString s) => s
let_ = "let"

letrec_ :: (IsString s) => s
letrec_ = "letrec"

public :: (IsString s) => s
public = "public"

comment :: (IsString s) => s
comment = "comment"

judoc :: (IsString s) => s
judoc = "judoc"

number :: (IsString s) => s
number = "number"

error :: (IsString s) => s
error = "error"

string :: (IsString s) => s
string = "string"

byte_ :: (IsString s) => s
byte_ = "byte"

byteEq :: (IsString s) => s
byteEq = "byte-eq"

byteToNat :: (IsString s) => s
byteToNat = "byte-to-nat"

byteFromNat :: (IsString s) => s
byteFromNat = "byte-from-nat"

byteArray :: (IsString s) => s
byteArray = "bytearray"

byteArrayFromListByte :: (IsString s) => s
byteArrayFromListByte = "bytearray-from-list-byte"

byteArraySize :: (IsString s) => s
byteArraySize = "bytearray-size"

nat :: (IsString s) => s
nat = "nat"

stringPrint :: (IsString s) => s
stringPrint = "string-print"

stringConcat :: (IsString s) => s
stringConcat = "string-concat"

stringEq :: (IsString s) => s
stringEq = "string-eq"

stringToNat :: (IsString s) => s
stringToNat = "string-to-nat"

bool_ :: (IsString s) => s
bool_ = "bool"

list :: (IsString s) => s
list = "list"

maybe_ :: (IsString s) => s
maybe_ = "maybe"

int_ :: (IsString s) => s
int_ = "int"

boolPrint :: (IsString s) => s
boolPrint = "bool-print"

fieldEq :: (IsString s) => s
fieldEq = "field-eq"

fieldAdd :: (IsString s) => s
fieldAdd = "field-add"

fieldSub :: (IsString s) => s
fieldSub = "field-sub"

fieldMul :: (IsString s) => s
fieldMul = "field-mul"

fieldDiv :: (IsString s) => s
fieldDiv = "field-div"

field :: (IsString s) => s
field = "field"

fieldFromInt :: (IsString s) => s
fieldFromInt = "field-from-int"

fieldToNat :: (IsString s) => s
fieldToNat = "field-to-nat"

io :: (IsString s) => s
io = "IO"

ioSequence :: (IsString s) => s
ioSequence = "IO-sequence"

ioReadline :: (IsString s) => s
ioReadline = "IO-readline"

natPrint :: (IsString s) => s
natPrint = "nat-print"

natToString :: (IsString s) => s
natToString = "nat-to-string"

natPlus :: (IsString s) => s
natPlus = "nat-plus"

natSub :: (IsString s) => s
natSub = "nat-sub"

natMul :: (IsString s) => s
natMul = "nat-mul"

natUDiv :: (IsString s) => s
natUDiv = "nat-udiv"

natDiv :: (IsString s) => s
natDiv = "nat-div"

natMod :: (IsString s) => s
natMod = "nat-mod"

natLe :: (IsString s) => s
natLe = "nat-le"

natLt :: (IsString s) => s
natLt = "nat-lt"

natEq :: (IsString s) => s
natEq = "nat-eq"

natGt :: (IsString s) => s
natGt = "nat-gt"

natGe :: (IsString s) => s
natGe = "nat-ge"

boolIf :: (IsString s) => s
boolIf = "bool-if"

boolOr :: (IsString s) => s
boolOr = "bool-or"

boolAnd :: (IsString s) => s
boolAnd = "bool-and"

intToString :: (IsString s) => s
intToString = "int-to-string"

intEq :: (IsString s) => s
intEq = "int-eq"

intPlus :: (IsString s) => s
intPlus = "int-plus"

intSubNat :: (IsString s) => s
intSubNat = "int-sub-nat"

intNegNat :: (IsString s) => s
intNegNat = "int-neg-nat"

intNeg :: (IsString s) => s
intNeg = "int-neg"

intMul :: (IsString s) => s
intMul = "int-mul"

intDiv :: (IsString s) => s
intDiv = "int-div"

intMod :: (IsString s) => s
intMod = "int-mod"

intSub :: (IsString s) => s
intSub = "int-sub"

intNonNeg :: (IsString s) => s
intNonNeg = "int-nonneg"

intLe :: (IsString s) => s
intLe = "int-le"

intLt :: (IsString s) => s
intLt = "int-lt"

fromNat :: (IsString s) => s
fromNat = "from-nat"

fromInt :: (IsString s) => s
fromInt = "from-int"

intPrint :: (IsString s) => s
intPrint = "int-print"

anomaGet :: (IsString s) => s
anomaGet = "anoma-get"

anomaEncode :: (IsString s) => s
anomaEncode = "anoma-encode"

anomaDecode :: (IsString s) => s
anomaDecode = "anoma-decode"

anomaVerifyDetached :: (IsString s) => s
anomaVerifyDetached = "anoma-verify-detached"

anomaSign :: (IsString s) => s
anomaSign = "anoma-sign"

anomaSignDetached :: (IsString s) => s
anomaSignDetached = "anoma-sign-detached"

anomaVerifyWithMessage :: (IsString s) => s
anomaVerifyWithMessage = "anoma-verify-with-message"

builtinSeq :: (IsString s) => s
builtinSeq = "seq"

as :: (IsString s) => s
as = "as"

builtin :: (IsString s) => s
builtin = "builtin"

type_ :: (IsString s) => s
type_ = "Type"

any :: (IsString s) => s
any = "Any"

questionMark :: (IsString s) => s
questionMark = "?"

fadd :: (IsString s) => s
fadd = "fadd"

fsub :: (IsString s) => s
fsub = "fsub"

fmul :: (IsString s) => s
fmul = "fmul"

fdiv :: (IsString s) => s
fdiv = "fdiv"

ftoi :: (IsString s) => s
ftoi = "ftoi"

itof :: (IsString s) => s
itof = "itof"

u8toi :: (IsString s) => s
u8toi = "u8toi"

itou8 :: (IsString s) => s
itou8 = "itou8"

delimiter :: (IsString s) => s
delimiter = "delimiter"

keyword :: (IsString s) => s
keyword = "keyword"

using :: (IsString s) => s
using = "using"

where_ :: (IsString s) => s
where_ = "where"

assignAscii :: (IsString s) => s
assignAscii = ":="

pipe :: (IsString s) => s
pipe = "|"

equal :: (IsString s) => s
equal = "="

notequal :: (IsString s) => s
notequal = "!="

plusequal :: (IsString s) => s
plusequal = "+="

less :: (IsString s) => s
less = "<"

lessEqual :: (IsString s) => s
lessEqual = "<="

greater :: (IsString s) => s
greater = ">"

greaterEqual :: (IsString s) => s
greaterEqual = ">="

bind :: (IsString s) => s
bind = ">>="

seq_ :: (IsString s) => s
seq_ = ">>"

seqq_ :: (IsString s) => s
seqq_ = ">>>"

sseq_ :: (IsString s) => s
sseq_ = "seq"

eq :: (IsString s) => s
eq = "eq"

trace_ :: (IsString s) => s
trace_ = "trace"

fail_ :: (IsString s) => s
fail_ = "fail"

dump :: (IsString s) => s
dump = "dump"

prealloc :: (IsString s) => s
prealloc = "prealloc"

argsnum :: (IsString s) => s
argsnum = "argsnum"

err :: (IsString s) => s
err = "err"

jmp :: (IsString s) => s
jmp = "jmp"

call :: (IsString s) => s
call = "call"

tcall :: (IsString s) => s
tcall = "tcall"

rel :: (IsString s) => s
rel = "rel"

abs :: (IsString s) => s
abs = "abs"

show_ :: (IsString s) => s
show_ = "show"

strConcat :: (IsString s) => s
strConcat = "strConcat"

strToInt :: (IsString s) => s
strToInt = "strToInt"

data_ :: (IsString s) => s
data_ = "data"

deBruijnVar :: (IsString s) => s
deBruijnVar = "$"

exclamation :: (IsString s) => s
exclamation = "!"

lambdaUnicode :: (IsString s) => s
lambdaUnicode = "λ"

lambdaAscii :: (IsString s) => s
lambdaAscii = "\\"

toUnicode :: (IsString s) => s
toUnicode = "→"

toAscii :: (IsString s) => s
toAscii = "->"

mapstoUnicode :: (IsString s) => s
mapstoUnicode = "↦"

mapstoAscii :: (IsString s) => s
mapstoAscii = "->"

comma :: (IsString s) => s
comma = ","

semicolon :: (IsString s) => s
semicolon = ";"

underscore :: (IsString s) => s
underscore = "_"

at_ :: (IsString s) => s
at_ = "@"

atQuestion :: (IsString s) => s
atQuestion = "@?"

dot :: (IsString s) => s
dot = "."

colon :: (IsString s) => s
colon = ":"

terminating :: (IsString s) => s
terminating = "terminating"

positive :: (IsString s) => s
positive = "positive"

trait :: (IsString s) => s
trait = "trait"

instance_ :: (IsString s) => s
instance_ = "instance"

coercion_ :: (IsString s) => s
coercion_ = "coercion"

waveArrow :: (IsString s) => s
waveArrow = "↝"

main :: (IsString s) => s
main = "main"

true_ :: (IsString s) => s
true_ = "true"

false_ :: (IsString s) => s
false_ = "false"

default_ :: (IsString s) => s
default_ = "default"

plus :: (IsString s) => s
plus = "+"

minus :: (IsString s) => s
minus = "-"

mul :: (IsString s) => s
mul = "*"

div :: (IsString s) => s
div = "/"

mod :: (IsString s) => s
mod = "%"

ap :: (IsString s) => s
ap = "ap"

fp :: (IsString s) => s
fp = "fp"

apPlusPlus :: (IsString s) => s
apPlusPlus = "ap++"

iadd :: (IsString s) => s
iadd = "iadd"

isub :: (IsString s) => s
isub = "isub"

imul :: (IsString s) => s
imul = "imul"

idiv :: (IsString s) => s
idiv = "idiv"

imod :: (IsString s) => s
imod = "imod"

ilt :: (IsString s) => s
ilt = "ilt"

ile :: (IsString s) => s
ile = "ile"

add_ :: (IsString s) => s
add_ = "add"

sub_ :: (IsString s) => s
sub_ = "sub"

mul_ :: (IsString s) => s
mul_ = "mul"

div_ :: (IsString s) => s
div_ = "div"

mod_ :: (IsString s) => s
mod_ = "mod"

lt_ :: (IsString s) => s
lt_ = "lt"

le_ :: (IsString s) => s
le_ = "le"

ret :: (IsString s) => s
ret = "ret"

live :: (IsString s) => s
live = "live"

dollar :: (IsString s) => s
dollar = "$"

notMutual :: (IsString s) => s
notMutual = "not-mutual"

mutual :: (IsString s) => s
mutual = "mutual"

if_ :: (IsString s) => s
if_ = "if"

then_ :: (IsString s) => s
then_ = "then"

else_ :: (IsString s) => s
else_ = "else"

piUnicode :: (IsString s) => s
piUnicode = "Π"

piAscii :: (IsString s) => s
piAscii = "Pi"

def :: (IsString s) => s
def = "def"

unit :: (IsString s) => s
unit = "unit"

void :: (IsString s) => s
void = "void"

case_ :: (IsString s) => s
case_ = "case"

save :: (IsString s) => s
save = "save"

tsave :: (IsString s) => s
tsave = "tsave"

caseOn :: (IsString s) => s
caseOn = "case-on"

alloc :: (IsString s) => s
alloc = "alloc"

calloc :: (IsString s) => s
calloc = "calloc"

cextend :: (IsString s) => s
cextend = "cextend"

ccall :: (IsString s) => s
ccall = "ccall"

br :: (IsString s) => s
br = "br"

of_ :: (IsString s) => s
of_ = "of"

match_ :: (IsString s) => s
match_ = "match"

with_ :: (IsString s) => s
with_ = "with"

fun_ :: (IsString s) => s
fun_ = "function"

integer :: (IsString s) => s
integer = "integer"

uint8 :: (IsString s) => s
uint8 = "uint8"

bool :: (IsString s) => s
bool = "bool"

bottomAscii :: (IsString s) => s
bottomAscii = "bottom"

bottom :: (IsString s) => s
bottom = "⊥"

arg :: (IsString s) => s
arg = "arg"

tmp :: (IsString s) => s
tmp = "tmp"

instrAdd :: (IsString s) => s
instrAdd = "add"

argsTag :: (IsString s) => s
argsTag = "args@"

tagTag :: (IsString s) => s
tagTag = "tag@"

stdlibTag :: (IsString s) => s
stdlibTag = "stdlib@"

instrSub :: (IsString s) => s
instrSub = "sub"

instrMul :: (IsString s) => s
instrMul = "mul"

instrDiv :: (IsString s) => s
instrDiv = "div"

instrMod :: (IsString s) => s
instrMod = "mod"

instrLt :: (IsString s) => s
instrLt = "lt"

instrLe :: (IsString s) => s
instrLe = "le"

instrEq :: (IsString s) => s
instrEq = "eq"

instrStrConcat :: (IsString s) => s
instrStrConcat = "strcat"

instrStrToInt :: (IsString s) => s
instrStrToInt = "atoi"

instrFieldToInt :: (IsString s) => s
instrFieldToInt = "ftoi"

instrIntToField :: (IsString s) => s
instrIntToField = "itof"

instrUInt8ToInt :: (IsString s) => s
instrUInt8ToInt = "u8toi"

instrIntToUInt8 :: (IsString s) => s
instrIntToUInt8 = "itou8"

instrByteArrayFromListUInt8 :: (IsString s) => s
instrByteArrayFromListUInt8 = "bytearray-from-list-uint8"

instrByteArraySize :: (IsString s) => s
instrByteArraySize = "bytearray-size"

instrShow :: (IsString s) => s
instrShow = "show"

instrPush :: (IsString s) => s
instrPush = "push"

instrPop :: (IsString s) => s
instrPop = "pop"

instrPusht :: (IsString s) => s
instrPusht = "pusht"

instrPopt :: (IsString s) => s
instrPopt = "popt"

instrTrace :: (IsString s) => s
instrTrace = "trace"

instrDump :: (IsString s) => s
instrDump = "dump"

instrFailure :: (IsString s) => s
instrFailure = "fail"

instrArgsNum :: (IsString s) => s
instrArgsNum = "argsnum"

instrPoseidon :: (IsString s) => s
instrPoseidon = "poseidon"

instrEcOp :: (IsString s) => s
instrEcOp = "ec_op"

instrPrealloc :: (IsString s) => s
instrPrealloc = "prealloc"

instrAlloc :: (IsString s) => s
instrAlloc = "alloc"

instrCalloc :: (IsString s) => s
instrCalloc = "calloc"

instrCextend :: (IsString s) => s
instrCextend = "cextend"

instrCall :: (IsString s) => s
instrCall = "call"

instrTcall :: (IsString s) => s
instrTcall = "tcall"

instrCcall :: (IsString s) => s
instrCcall = "ccall"

instrTccall :: (IsString s) => s
instrTccall = "tccall"

instrReturn :: (IsString s) => s
instrReturn = "ret"

instrBr :: (IsString s) => s
instrBr = "br"

juvixDotOrg :: (IsString s) => s
juvixDotOrg = "https://juvix.org"

vampirDef :: (IsString s) => s
vampirDef = "def"

vampirEq :: (IsString s) => s
vampirEq = "="

vampirNumber :: (IsString s) => s
vampirNumber = "number"

vampirFail :: (IsString s) => s
vampirFail = "fail"

vampirAdd :: (IsString s) => s
vampirAdd = "add"

vampirSub :: (IsString s) => s
vampirSub = "sub"

vampirMul :: (IsString s) => s
vampirMul = "mul"

vampirDiv :: (IsString s) => s
vampirDiv = "div"

vampirMod :: (IsString s) => s
vampirMod = "rem"

vampirEqual :: (IsString s) => s
vampirEqual = "equal"

vampirLessThan :: (IsString s) => s
vampirLessThan = "lessThan"

vampirLessOrEqual :: (IsString s) => s
vampirLessOrEqual = "lessOrEqual"

vampirIf :: (IsString s) => s
vampirIf = "if"

zero :: (IsString s) => s
zero = "zero"

suc :: (IsString s) => s
suc = "suc"

true :: (IsString s) => s
true = "true"

false :: (IsString s) => s
false = "false"

ofNat :: (IsString s) => s
ofNat = "ofNat"

negSuc :: (IsString s) => s
negSuc = "negSuc"

nil :: (IsString s) => s
nil = "nil"

cons :: (IsString s) => s
cons = "cons"

nothing :: (IsString s) => s
nothing = "nothing"

just :: (IsString s) => s
just = "just"

pair :: (IsString s) => s
pair = "pair"

unary :: (IsString s) => s
unary = "unary"

binary :: (IsString s) => s
binary = "binary"

left :: (IsString s) => s
left = "left"

right :: (IsString s) => s
right = "right"

same :: (IsString s) => s
same = "same"

none :: (IsString s) => s
none = "none"

nop :: (IsString s) => s
nop = "nop"

init :: (IsString s) => s
init = "init"

range :: (IsString s) => s
range = "range"

git :: (IsString s) => s
git = "git"

dependencies :: (IsString s) => s
dependencies = "dependencies"

path_ :: (IsString s) => s
path_ = "path"

package :: (IsString s) => s
package = "package"

version :: (IsString s) => s
version = "version"

functionsPlaceholder :: (IsString s) => s
functionsPlaceholder = "functionsLibrary_placeholder"

theFunctionsLibrary :: (IsString s) => s
theFunctionsLibrary = "the_functionsLibrary"

cairoRangeCheck :: (IsString s) => s
cairoRangeCheck = "range_check"

cairoPoseidon :: (IsString s) => s
cairoPoseidon = "poseidon"

cairoEcOp :: (IsString s) => s
cairoEcOp = "ec_op"

cairoRandomEcPoint :: (IsString s) => s
cairoRandomEcPoint = "random_ec_point"

cairoPoseidonState :: (IsString s) => s
cairoPoseidonState = "poseidon_state"

cairoMkPoseidonState :: (IsString s) => s
cairoMkPoseidonState = "mkPoseidonState"

cairoEcPoint :: (IsString s) => s
cairoEcPoint = "ec_point"

cairoMkEcPoint :: (IsString s) => s
cairoMkEcPoint = "mkEcPoint"

rustFn :: (IsString s) => s
rustFn = "fn"

rustIf :: (IsString s) => s
rustIf = "if"

rustElse :: (IsString s) => s
rustElse = "else"

rustMatch :: (IsString s) => s
rustMatch = "match"

rustLoop :: (IsString s) => s
rustLoop = "loop"

rustLet :: (IsString s) => s
rustLet = "let"

rustConst :: (IsString s) => s
rustConst = "const"

rustMut :: (IsString s) => s
rustMut = "mut"

rustVec :: (IsString s) => s
rustVec = "vec!"

rustVector :: (IsString s) => s
rustVector = "Vec"

rustWord :: (IsString s) => s
rustWord = "Word"

rustMemory :: (IsString s) => s
rustMemory = "Memory"

rustContinue :: (IsString s) => s
rustContinue = "continue"

rustReturn :: (IsString s) => s
rustReturn = "return"
