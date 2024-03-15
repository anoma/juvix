module Nockma.Base where

import Base
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromTree

-- | Call a function at the head of the subject using the Anoma calling convention
anomaCall :: [Term Natural] -> Term Natural
anomaCall args = case nonEmpty args of
  Just args' -> helper (Just (opReplace "anomaCall-args" (closurePath ArgsTuple) (foldTerms args')))
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
