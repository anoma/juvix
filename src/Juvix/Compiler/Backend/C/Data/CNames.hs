module Juvix.Compiler.Backend.C.Data.CNames where

import Juvix.Prelude

primPrefix :: Text -> Text
primPrefix = ("prim_" <>)

zero :: Text
zero = primPrefix "zero"

suc :: Text
suc = primPrefix "suc"

printNat :: Text
printNat = primPrefix "printNat"

natToString :: Text
natToString = primPrefix "natToString"

printString :: Text
printString = primPrefix "printString"

stringConcat :: Text
stringConcat = primPrefix "stringConcat"

stringToNat :: Text
stringToNat = primPrefix "stringToNat"

io :: Text
io = primPrefix "io"

string_ :: Text
string_ = primPrefix "string"

nat :: Text
nat = primPrefix "nat"

bool_ :: Text
bool_ = primPrefix "bool"

true_ :: Text
true_ = primPrefix "true"

false_ :: Text
false_ = primPrefix "false"

printBool :: Text
printBool = primPrefix "printBool"

int_ :: Text
int_ = "int"

ioseq :: Text
ioseq = primPrefix "sequence"

ioreadline :: Text
ioreadline = primPrefix "readline"

natplus :: Text
natplus = primPrefix "natplus"

natsub :: Text
natsub = primPrefix "natsub"

natmul :: Text
natmul = primPrefix "natmul"

natudiv :: Text
natudiv = primPrefix "natudiv"

natdiv :: Text
natdiv = primPrefix "natdiv"

natmod :: Text
natmod = primPrefix "natmod"

natle :: Text
natle = primPrefix "natle"

natlt :: Text
natlt = primPrefix "natlt"

nateq :: Text
nateq = primPrefix "nateq"

boolif :: Text
boolif = primPrefix "if"

boolor :: Text
boolor = primPrefix "or"

booland :: Text
booland = primPrefix "and"

trace_ :: Text
trace_ = "trace"

fail_ :: Text
fail_ = "fail"

funField :: Text
funField = "fun"

asStruct :: Text -> Text
asStruct n = n <> "_s"

asTypeDef :: Text -> Text
asTypeDef n = n <> "_t"

asTag :: Text -> Text
asTag n = n <> "_tag"

asField :: Text -> Text
asField n = n <> "_field"

asNullary :: Text -> Text
asNullary n = n <> "_nullary"

asCast :: Text -> Text
asCast n = "as_" <> n

asProjName :: Text -> Text -> Text
asProjName argName n = "proj_" <> argName <> "_" <> n

asProj :: Int -> Text -> Text
asProj argIdx n = asProjName (asCtorArg (show argIdx)) n

asIs :: Text -> Text
asIs n = "is_" <> n

asNew :: Text -> Text
asNew n = "new_" <> n

asFun :: Text -> Text
asFun n = n <> "_fun"

asEval :: Text -> Text
asEval n = n <> "_eval"

asApply :: Text -> Text
asApply n = n <> "_apply"

asEnv :: Text -> Text
asEnv n = n <> "_env"

asFunArg :: Text -> Text
asFunArg n = "fa" <> n

asCtorArg :: Text -> Text
asCtorArg n = "ca" <> n

asEnvArg :: Text -> Text
asEnvArg n = "ea" <> n

mkArgs :: (Text -> Text) -> [Text]
mkArgs f = map (f . show) [0 :: Integer ..]

funArgs :: [Text]
funArgs = mkArgs asFunArg

ctorArgs :: [Text]
ctorArgs = mkArgs asCtorArg

envArgs :: [Text]
envArgs = mkArgs asEnvArg
