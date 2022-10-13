module Juvix.Extra.Strings where

import Juvix.Prelude.Base

module_ :: IsString s => s
module_ = "module"

axiom :: IsString s => s
axiom = "axiom"

judocStart :: IsString s => s
judocStart = "---"

judocExample :: IsString s => s
judocExample = ">>>"

end :: IsString s => s
end = "end"

eval :: IsString s => s
eval = "eval"

hiding :: IsString s => s
hiding = "hiding"

include :: IsString s => s
include = "include"

import_ :: IsString s => s
import_ = "import"

in_ :: IsString s => s
in_ = "in"

inductive :: IsString s => s
inductive = "inductive"

function :: IsString s => s
function = "function"

constructor :: IsString s => s
constructor = "constructor"

constr :: IsString s => s
constr = "constr"

topModule :: IsString s => s
topModule = "top module"

localModule :: IsString s => s
localModule = "local module"

infix_ :: IsString s => s
infix_ = "infix"

infixl_ :: IsString s => s
infixl_ = "infixl"

infixr_ :: IsString s => s
infixr_ = "infixr"

open :: IsString s => s
open = "open"

postfix :: IsString s => s
postfix = "postfix"

print :: IsString s => s
print = "print"

let_ :: IsString s => s
let_ = "let"

letrec_ :: IsString s => s
letrec_ = "letrec"

public :: IsString s => s
public = "public"

comment :: IsString s => s
comment = "comment"

number :: IsString s => s
number = "number"

error :: IsString s => s
error = "error"

string :: IsString s => s
string = "string"

natural :: IsString s => s
natural = "natural"

boolean_ :: IsString s => s
boolean_ = "boolean"

io :: IsString s => s
io = "IO"

ioSequence :: IsString s => s
ioSequence = "IO-sequence"

naturalPrint :: IsString s => s
naturalPrint = "natural-print"

naturalPlus :: IsString s => s
naturalPlus = "natural-plus"

builtin :: IsString s => s
builtin = "builtin"

type_ :: IsString s => s
type_ = "Type"

questionMark :: IsString s => s
questionMark = "?"

keyword :: IsString s => s
keyword = "keyword"

using :: IsString s => s
using = "using"

where_ :: IsString s => s
where_ = "where"

assignAscii :: IsString s => s
assignAscii = ":="

pipe :: IsString s => s
pipe = "|"

equal :: IsString s => s
equal = "="

less :: IsString s => s
less = "<"

lessEqual :: IsString s => s
lessEqual = "<="

greater :: IsString s => s
greater = ">"

greaterEqual :: IsString s => s
greaterEqual = ">="

bind :: IsString s => s
bind = ">>="

seq_ :: IsString s => s
seq_ = ">>"

trace_ :: IsString s => s
trace_ = "trace"

fail_ :: IsString s => s
fail_ = "fail"

data_ :: IsString s => s
data_ = "data"

deBruijnVar :: IsString s => s
deBruijnVar = "$"

exclamation :: IsString s => s
exclamation = "!"

lambdaUnicode :: IsString s => s
lambdaUnicode = "λ"

lambdaAscii :: IsString s => s
lambdaAscii = "\\"

toUnicode :: IsString s => s
toUnicode = "→"

toAscii :: IsString s => s
toAscii = "->"

mapstoUnicode :: IsString s => s
mapstoUnicode = "↦"

mapstoAscii :: IsString s => s
mapstoAscii = "->"

foreign_ :: IsString s => s
foreign_ = "foreign"

compile :: IsString s => s
compile = "compile"

comma :: IsString s => s
comma = ","

semicolon :: IsString s => s
semicolon = ";"

underscore :: IsString s => s
underscore = "_"

at_ :: IsString s => s
at_ = "@"

dot :: IsString s => s
dot = "."

colon :: IsString s => s
colon = ":"

colonSpace :: IsString s => s
colonSpace = ": "

colonOmegaUnicode :: IsString s => s
colonOmegaUnicode = ":ω"

colonOmegaAscii :: IsString s => s
colonOmegaAscii = ":any"

colonOne :: IsString s => s
colonOne = ":1"

colonZero :: IsString s => s
colonZero = ":0"

ghc :: IsString s => s
ghc = "ghc"

cBackend :: IsString s => s
cBackend = "c"

terminating :: IsString s => s
terminating = "terminating"

positive :: IsString s => s
positive = "positive"

waveArrow :: IsString s => s
waveArrow = "↝"

stdlib :: IsString s => s
stdlib = "stdlib.h"

stdbool :: IsString s => s
stdbool = "stdbool.h"

stdio :: IsString s => s
stdio = "stdio.h"

main :: IsString s => s
main = "main"

fprintf :: IsString s => s
fprintf = "fprintf"

stderr_ :: IsString s => s
stderr_ = "stderr"

exit :: IsString s => s
exit = "exit"

exitFailure_ :: IsString s => s
exitFailure_ = "EXIT_FAILURE"

malloc :: IsString s => s
malloc = "malloc"

sizeof :: IsString s => s
sizeof = "sizeof"

true_ :: IsString s => s
true_ = "true"

false_ :: IsString s => s
false_ = "false"

arg_ :: IsString s => s
arg_ = "arg"

tmp_ :: IsString s => s
tmp_ = "tmp"

tag :: IsString s => s
tag = "tag"

main_ :: IsString s => s
main_ = "main"

minicRuntime :: IsString s => s
minicRuntime = "c-runtime.h"

putStrLn_ :: IsString s => s
putStrLn_ = "putStrLn"

debug_ :: IsString s => s
debug_ = "debug"

plus :: IsString s => s
plus = "+"

minus :: IsString s => s
minus = "-"

mul :: IsString s => s
mul = "*"

div :: IsString s => s
div = "/"

mod :: IsString s => s
mod = "%"

dollar :: IsString s => s
dollar = "$"

if_ :: IsString s => s
if_ = "if"

then_ :: IsString s => s
then_ = "then"

else_ :: IsString s => s
else_ = "else"

pi_ :: IsString s => s
pi_ = "pi"

def :: IsString s => s
def = "def"

zero :: IsString s => s
zero = "0"

succ :: IsString s => s
succ = "S"

unit :: IsString s => s
unit = "unit"

void :: IsString s => s
void = "void"

nil :: IsString s => s
nil = "nil"

cons :: IsString s => s
cons = "cons"

pair :: IsString s => s
pair = "pair"

case_ :: IsString s => s
case_ = "case"

of_ :: IsString s => s
of_ = "of"

match_ :: IsString s => s
match_ = "match"

with_ :: IsString s => s
with_ = "with"

fun_ :: IsString s => s
fun_ = "function"

fwd_ :: IsString s => s
fwd_ = "forward"

juvixFunctionT :: IsString s => s
juvixFunctionT = "juvix_function_t"

juvixDotOrg :: IsString s => s
juvixDotOrg = "https://juvix.org"
