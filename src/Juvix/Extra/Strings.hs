module Juvix.Extra.Strings where

import Juvix.Prelude.Base

module_ :: (IsString s) => s
module_ = "module"

axiom :: (IsString s) => s
axiom = "axiom"

judocStart :: (IsString s) => s
judocStart = "---"

judocExample :: (IsString s) => s
judocExample = ">>>"

end :: (IsString s) => s
end = "end"

eval :: (IsString s) => s
eval = "eval"

hiding :: (IsString s) => s
hiding = "hiding"

include :: (IsString s) => s
include = "include"

import_ :: (IsString s) => s
import_ = "import"

in_ :: (IsString s) => s
in_ = "in"

inductive :: (IsString s) => s
inductive = "type"

function :: (IsString s) => s
function = "function"

variable :: IsString s => s
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

open :: (IsString s) => s
open = "open"

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

number :: (IsString s) => s
number = "number"

error :: (IsString s) => s
error = "error"

string :: (IsString s) => s
string = "string"

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

int_ :: (IsString s) => s
int_ = "int"

boolPrint :: (IsString s) => s
boolPrint = "bool-print"

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

boolOr :: IsString s => s
boolOr = "bool-or"

boolAnd :: IsString s => s
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

builtin :: IsString s => s
builtin = "builtin"

type_ :: (IsString s) => s
type_ = "Type"

any :: (IsString s) => s
any = "Any"

questionMark :: (IsString s) => s
questionMark = "?"

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

trace_ :: (IsString s) => s
trace_ = "trace"

fail_ :: (IsString s) => s
fail_ = "fail"

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

dot :: (IsString s) => s
dot = "."

colon :: (IsString s) => s
colon = ":"

terminating :: (IsString s) => s
terminating = "terminating"

positive :: (IsString s) => s
positive = "positive"

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

dollar :: (IsString s) => s
dollar = "$"

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

caseOn :: (IsString s) => s
caseOn = "case-on"

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

bool :: (IsString s) => s
bool = "bool"

arg :: (IsString s) => s
arg = "arg"

tmp :: (IsString s) => s
tmp = "tmp"

instrAdd :: (IsString s) => s
instrAdd = "add"

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

gebAbsurd :: IsString s => s
gebAbsurd = "absurd"

gebUnit :: IsString s => s
gebUnit = "unit"

gebLeft :: IsString s => s
gebLeft = "left"

gebRight :: IsString s => s
gebRight = "right"

gebCase :: IsString s => s
gebCase = "case-on"

gebPair :: IsString s => s
gebPair = "pair"

gebFst :: IsString s => s
gebFst = "fst"

gebSnd :: IsString s => s
gebSnd = "snd"

gebLamb :: IsString s => s
gebLamb = "lamb"

gebValueClosure :: IsString s => s
gebValueClosure = "cls"

gebValueClosureEnv :: IsString s => s
gebValueClosureEnv = "env"

lispNil :: IsString s => s
lispNil = "nil"

gebApp :: IsString s => s
gebApp = "app"

gebVar :: IsString s => s
gebVar = "index"

gebAdd :: IsString s => s
gebAdd = "add"

gebSub :: IsString s => s
gebSub = "sub"

gebMul :: IsString s => s
gebMul = "mul"

gebDiv :: IsString s => s
gebDiv = "div"

gebMod :: IsString s => s
gebMod = "mod"

gebFail :: IsString s => s
gebFail = "fail"

gebEq :: IsString s => s
gebEq = "eq"

gebLt :: IsString s => s
gebLt = "lt"

gebInitial :: IsString s => s
gebInitial = "so0"

gebTerminal :: IsString s => s
gebTerminal = "so1"

gebProd :: IsString s => s
gebProd = "prod"

gebCoprod :: IsString s => s
gebCoprod = "coprod"

gebHom :: IsString s => s
gebHom = "!->"

gebInteger :: IsString s => s
gebInteger = "int"

gebTyped :: IsString s => s
gebTyped = "typed"

juvixDotOrg :: (IsString s) => s
juvixDotOrg = "https://juvix.org"
