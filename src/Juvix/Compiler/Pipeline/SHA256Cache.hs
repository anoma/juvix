module Juvix.Compiler.Pipeline.SHA256Cache where

import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Data.SHA256 qualified as SHA256
import Juvix.Prelude

type SHA256Cache = Cache ImportNode Text

evalSHA256Cache :: (Member Files r) => Sem (SHA256Cache ': r) a -> Sem r a
evalSHA256Cache = evalCacheEmpty computeSHA256

computeSHA256 :: (Member Files r) => ImportNode -> Sem r Text
computeSHA256 = SHA256.digestFile . (^. importNodeAbsFile)

getSHA256 :: (Member SHA256Cache r) => ImportNode -> Sem r Text
getSHA256 = cacheGet
