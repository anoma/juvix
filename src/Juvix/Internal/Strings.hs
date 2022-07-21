module Juvix.Internal.Strings where

import Juvix.Prelude

module_ :: IsString s => s
module_ = "module"

axiom :: IsString s => s
axiom = "axiom"

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

assignUnicode :: IsString s => s
assignUnicode = "≔"

assignAscii :: IsString s => s
assignAscii = ":="

pipe :: IsString s => s
pipe = "|"

equal :: IsString s => s
equal = "="

data_ :: IsString s => s
data_ = "data"

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

semicolon :: IsString s => s
semicolon = ";"

underscore :: IsString s => s
underscore = "_"

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

tag :: IsString s => s
tag = "tag"

main_ :: IsString s => s
main_ = "main"

minicRuntime :: IsString s => s
minicRuntime = "minic-runtime.h"

putStrLn_ :: IsString s => s
putStrLn_ = "putStrLn"

juvixFunctionT :: IsString s => s
juvixFunctionT = "juvix_function_t"
