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

constructor :: (IsString s) => s
constructor = "constructor"

constr :: (IsString s) => s
constr = "constr"

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

print :: (IsString s) => s
print = "print"

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

boolean_ :: (IsString s) => s
boolean_ = "boolean"

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

boolIf :: (IsString s) => s
boolIf = "bool-if"

boolOr :: IsString s => s
boolOr = "bool-or"

boolAnd :: IsString s => s
boolAnd = "bool-and"

builtin :: IsString s => s
builtin = "builtin"

type_ :: (IsString s) => s
type_ = "Type"

any :: (IsString s) => s
any = "any"

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

foreign_ :: (IsString s) => s
foreign_ = "foreign"

compile :: (IsString s) => s
compile = "compile"

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

colonSpace :: (IsString s) => s
colonSpace = ": "

colonOmegaUnicode :: (IsString s) => s
colonOmegaUnicode = ":ω"

colonOmegaAscii :: (IsString s) => s
colonOmegaAscii = ":any"

colonOne :: (IsString s) => s
colonOne = ":1"

colonZero :: (IsString s) => s
colonZero = ":0"

ghc :: (IsString s) => s
ghc = "ghc"

cBackend :: (IsString s) => s
cBackend = "c"

terminating :: (IsString s) => s
terminating = "terminating"

positive :: (IsString s) => s
positive = "positive"

waveArrow :: (IsString s) => s
waveArrow = "↝"

stdlib :: (IsString s) => s
stdlib = "stdlib.h"

stdbool :: (IsString s) => s
stdbool = "stdbool.h"

stdio :: (IsString s) => s
stdio = "stdio.h"

main :: (IsString s) => s
main = "main"

fprintf :: (IsString s) => s
fprintf = "fprintf"

stderr_ :: (IsString s) => s
stderr_ = "stderr"

exit :: (IsString s) => s
exit = "exit"

exitFailure_ :: (IsString s) => s
exitFailure_ = "EXIT_FAILURE"

malloc :: (IsString s) => s
malloc = "malloc"

sizeof :: (IsString s) => s
sizeof = "sizeof"

true_ :: (IsString s) => s
true_ = "true"

false_ :: (IsString s) => s
false_ = "false"

default_ :: (IsString s) => s
default_ = "default"

arg_ :: (IsString s) => s
arg_ = "arg"

tmp_ :: (IsString s) => s
tmp_ = "tmp"

tag :: (IsString s) => s
tag = "tag"

main_ :: (IsString s) => s
main_ = "main"

minicRuntime :: (IsString s) => s
minicRuntime = "c-runtime.h"

putStrLn_ :: (IsString s) => s
putStrLn_ = "putStrLn"

debug_ :: (IsString s) => s
debug_ = "debug"

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

pi_ :: (IsString s) => s
pi_ = "pi"

def :: (IsString s) => s
def = "def"

zero :: (IsString s) => s
zero = "0"

succ :: (IsString s) => s
succ = "S"

unit :: (IsString s) => s
unit = "unit"

void :: (IsString s) => s
void = "void"

nil :: (IsString s) => s
nil = "nil"

cons :: (IsString s) => s
cons = "cons"

pair :: (IsString s) => s
pair = "pair"

case_ :: (IsString s) => s
case_ = "case"

of_ :: (IsString s) => s
of_ = "of"

match_ :: (IsString s) => s
match_ = "match"

with_ :: (IsString s) => s
with_ = "with"

fun_ :: (IsString s) => s
fun_ = "function"

fwd_ :: (IsString s) => s
fwd_ = "forward"

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

juvixFunctionT :: (IsString s) => s
juvixFunctionT = "juvix_function_t"

juvixDotOrg :: (IsString s) => s
juvixDotOrg = "https://juvix.org"
