module MiniJuvix.Syntax.Abstract.Language.Extra (
  module MiniJuvix.Syntax.Abstract.Language,
  module MiniJuvix.Syntax.Abstract.Language.Extra
                                       ) where

import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Prelude

smallerPatternVariables :: Pattern -> [VarName]
smallerPatternVariables p = case p of
  PatternVariable {} -> []
  PatternWildcard {} -> []
  PatternEmpty {} -> []
  PatternConstructorApp app -> appVariables app
  where
  appVariables :: ConstructorApp -> [VarName]
  appVariables (ConstructorApp _ ps) = concatMap patternVariables ps

  patternVariables :: Pattern -> [VarName]
  patternVariables pat = case pat of
    PatternVariable v -> [v]
    PatternWildcard {} -> []
    PatternEmpty {} -> []
    PatternConstructorApp app -> appVariables app
