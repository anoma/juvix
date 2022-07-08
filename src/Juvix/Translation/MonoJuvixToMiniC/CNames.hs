module Juvix.Translation.MonoJuvixToMiniC.CNames where

import Juvix.Prelude

primPrefix :: Text -> Text
primPrefix = ("prim_" <>)

zero :: Text
zero = primPrefix "zero"

suc :: Text
suc = primPrefix "suc"

printNat :: Text
printNat = primPrefix "printNat"

io :: Text
io = primPrefix "io"

nat :: Text
nat = primPrefix "nat"

int_ :: Text
int_ = "int"

ioseq :: Text
ioseq = primPrefix "sequence"

natplus :: Text
natplus = primPrefix "natplus"

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
