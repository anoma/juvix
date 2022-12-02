module Juvix.Compiler.Core.Transformation.MatchToCase where

import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra.Recursors.Map.Named
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Transformation.Base

matchToCase :: InfoTable -> InfoTable
matchToCase = run . mapT' (const (dmapM matchToCaseNode))

matchToCaseNode :: forall r. Member InfoTableBuilder r => Node -> Sem r Node
matchToCaseNode n = undefined
