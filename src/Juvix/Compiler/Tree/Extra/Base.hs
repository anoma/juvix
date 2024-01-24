module Juvix.Compiler.Tree.Extra.Base where

import Juvix.Compiler.Tree.Language

mkBinop :: BinaryOpcode -> Node -> Node -> Node
mkBinop op arg1 arg2 = Binop (NodeBinop op arg1 arg2)

mkUnop :: UnaryOpcode -> Node -> Node
mkUnop op arg = Unop (NodeUnop op arg)
