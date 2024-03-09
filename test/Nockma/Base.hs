module Nockma.Base where

import Base
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromTree

-- | Call a function at the head of the subject using the Anoma calling convention
anomaCall :: Term Natural -> [Term Natural] -> Term Natural
anomaCall env args = case nonEmpty args of
  Just args' -> helper (Just (OpReplace # (closurePath ArgsTuple # foldTerms args')))
  Nothing -> helper Nothing
  where
    helper replaceArgs =
      opCall
        "anomaCall"
        (closurePath WrapperCode)
        ( OpReplace
            # (closurePath FunctionsLibrary # OpQuote # env)
            # (repArgs (OpAddress # emptyPath))
        )
      where
        repArgs x = case replaceArgs of
          Nothing -> x
          Just r -> r # x
