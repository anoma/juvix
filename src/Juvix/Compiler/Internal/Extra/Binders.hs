module Juvix.Compiler.Internal.Extra.Binders where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

class HasBinders a where
  bindersTraversal :: Traversal' a VarName

instance HasBinders PatternArg where
  bindersTraversal f (PatternArg i n p) = PatternArg i <$> traverse f n <*> bindersTraversal f p

instance HasBinders Pattern where
  bindersTraversal f p = case p of
    PatternVariable v -> PatternVariable <$> f v
    PatternWildcardConstructor {} -> pure p
    PatternConstructorApp a -> PatternConstructorApp <$> goApp f a
    where
      goApp :: Traversal' ConstructorApp VarName
      goApp g = traverseOf constrAppParameters (traverse (bindersTraversal g))

instance HasBinders FunctionParameter where
  bindersTraversal = paramName . _Just

instance HasBinders InductiveParameter where
  bindersTraversal = inductiveParamName

instance HasBinders SimpleBinder where
  bindersTraversal = sbinderVar

instance HasBinders FunctionDef where
  bindersTraversal = funDefName

instance HasBinders MutualBlockLet where
  bindersTraversal = mutualLet . each . bindersTraversal

instance HasBinders LetClause where
  bindersTraversal f = \case
    LetFunDef fun -> LetFunDef <$> bindersTraversal f fun
    LetMutualBlock b -> LetMutualBlock <$> bindersTraversal f b
