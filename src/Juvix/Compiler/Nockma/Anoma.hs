module Juvix.Compiler.Nockma.Anoma where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromTree
import Juvix.Prelude

-- | Call a function at the head of the subject using the Anoma calling convention
anomaCall :: [Term Natural] -> Term Natural
anomaCall args = anomaCallTuple (foldTerms <$> nonEmpty args)

anomaCallTuple :: Maybe (Term Natural) -> Term Natural
anomaCallTuple = \case
  Just args -> helper (Just (opReplace "anomaCall-args" (closurePath ArgsTuple) args))
  Nothing -> helper Nothing
  where
    helper replaceArgs =
      opCall
        "anomaCall"
        (closurePath WrapperCode)
        (repArgs (OpAddress # emptyPath))
      where
        repArgs x = case replaceArgs of
          Nothing -> x
          Just r -> r x
