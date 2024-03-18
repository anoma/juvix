module Juvix.Compiler.Nockma.Anoma where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Prelude

-- | Call a function at the head of the subject using the Anoma calling convention
anomaCall :: Term Natural -> [Term Natural] -> Term Natural
anomaCall functionsLib args = case nonEmpty args of
  Just args' -> helper (Just (opReplace "anomaCall-args" (closurePath ArgsTuple) (foldTerms args')))
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
