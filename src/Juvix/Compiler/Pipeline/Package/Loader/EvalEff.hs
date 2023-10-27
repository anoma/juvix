module Juvix.Compiler.Pipeline.Package.Loader.EvalEff where

import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Value

data TypeSpec = TypeSpec
  { _typeSpecName :: Text,
    _typeSpecFile :: Path Abs File
  }

makeLenses ''TypeSpec

data EvalEff m a where
  Eval' :: Node -> EvalEff m Value
  LookupIdentifier :: Text -> EvalEff m Node
  -- | Assert that the Node has a type given by one of the ;TypeSpec;s
  AssertNodeType :: (Foldable f) => Node -> f TypeSpec -> EvalEff m ()

makeSem ''EvalEff

type EvalFileEff = Scoped (Path Abs File) EvalEff
