module MiniJuvix.Internal.Strings where

import MiniJuvix.Prelude

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

match :: IsString s => s
match = "match"

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

any :: IsString s => s
any = "Any"

type_ :: IsString s => s
type_ = "Type"

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

agda :: IsString s => s
agda = "agda"
