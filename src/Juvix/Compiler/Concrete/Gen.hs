module Juvix.Compiler.Concrete.Gen
  ( module Juvix.Compiler.Concrete.Gen,
    module Juvix.Compiler.Concrete.Keywords,
  )
where

import Juvix.Compiler.Concrete.Keywords
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

kw :: (Members '[Reader Interval] r) => Keyword -> Sem r KeywordRef
kw k = do
  loc <- ask
  return
    KeywordRef
      { _keywordRefKeyword = k,
        _keywordRefUnicode = Ascii,
        _keywordRefInterval = loc
      }

smallUniverseExpression :: forall s r. (SingI s) => (Members '[Reader Interval] r) => Sem r (ExpressionType s)
smallUniverseExpression = do
  loc <- ask @Interval
  return $ case sing :: SStage s of
    SScoped -> ExpressionUniverse (smallUniverse loc)
    SParsed ->
      ExpressionAtoms
        { _expressionAtomsLoc = Irrelevant loc,
          _expressionAtoms = pure (AtomUniverse (smallUniverse loc))
        }
