module MiniJuvix.Syntax.Abstract.Language.Extra (
  module MiniJuvix.Syntax.Abstract.Language,
  module MiniJuvix.Syntax.Abstract.Language.Extra
                                       ) where

import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Prelude

patternVariables :: Pattern -> [VarName]
patternVariables p = case p of
  PatternVariable v -> [v]
  PatternWildcard {} -> []
  PatternEmpty {} -> []
  PatternConstructorApp app -> appVariables app
  where
  appVariables :: ConstructorApp -> [VarName]
  appVariables (ConstructorApp _ ps) = concatMap patternVariables ps
