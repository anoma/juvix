module Nockma.Base where

import Base
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Translation.FromTree

-- | Call a function at the head of the subject using the Anoma calling convention
anomaCall :: [Term Natural] -> Term Natural
anomaCall args = case nonEmpty args of
  Just args' -> OpCall # [L] # OpReplace # ([R, L] # foldTerms args') # (OpAddress # emptyPath)
  Nothing -> OpCall # [L] # (OpAddress # emptyPath)
