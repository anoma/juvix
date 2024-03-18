module Juvix.Compiler.Nockma.Anoma where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Prelude

-- | Call a function at the head of the subject using the Anoma calling convention
anomaCall :: Term Natural -> [Term Natural] -> Term Natural
anomaCall functionsLib args = anomaCallTuple functionsLib (foldTerms <$> nonEmpty args)

anomaCallTuple :: Term Natural -> Maybe (Term Natural) -> Term Natural
anomaCallTuple functionsLib = \case
  Just args -> helper (Just (opReplace "anomaCall-args" (closurePath ArgsTuple) args))
  Nothing -> helper Nothing
  where
    helper replaceArgs =
      opCall
        "anomaCall"
        (closurePath WrapperCode)
        ( opReplace
            "anomaCall-functionsLibrary"
            (closurePath FunctionsLibrary)
            (OpQuote # functionsLib)
            (repArgs (OpAddress # emptyPath))
        )
      where
        repArgs x = case replaceArgs of
          Nothing -> x
          Just r -> r x
