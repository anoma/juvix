module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.LocalVars where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types
import Juvix.Prelude

newtype LocalVars = LocalVars
  { _localArities :: HashMap VarName Arity
  }

makeLenses ''LocalVars

addArity :: Member (State LocalVars) r => VarName -> Arity -> Sem r ()
addArity v t = modify (over localArities (HashMap.insert v t))

getLocalArity :: Member (Reader LocalVars) r => VarName -> Sem r Arity
getLocalArity v = fromJust <$> asks (^. localArities . at v)

emptyLocalVars :: LocalVars
emptyLocalVars =
  LocalVars
    { _localArities = mempty
    }
