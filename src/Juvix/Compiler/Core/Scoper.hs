module Juvix.Compiler.Core.Scoper where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

type ScopeError = Text

scopeCheck :: InfoTable -> Maybe ScopeError
scopeCheck = either Just (const Nothing) . run . runError . walkT goTopNode

goTopNode :: (Members '[Error ScopeError] r) => Symbol -> Node -> Sem r ()
goTopNode sym = runReader sym . walkN check

check :: (Members '[Reader Symbol, Error ScopeError] r) => Index -> Node -> Sem r ()
check k = \case
  NVar v
    | v ^. varIndex < k -> return ()
    | otherwise -> scopeErr ("variable " <> ppTrace (NVar v) <> " is out of scope")
  _ -> return ()

scopeErr :: (Members '[Reader Symbol, Error ScopeError] r) => Text -> Sem r a
scopeErr msg = do
  sym <- ask @Symbol
  throw @ScopeError ("Scope error in the definition of " <> show sym <> "\n" <> msg)

-- | prints the scope error without exiting
scopeTrace :: InfoTable -> IO ()
scopeTrace i = whenJust (scopeCheck i) putStrLn
