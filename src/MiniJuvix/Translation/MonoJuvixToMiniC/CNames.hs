module MiniJuvix.Translation.MonoJuvixToMiniC.CNames where

import MiniJuvix.Prelude

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
