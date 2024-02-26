module Juvix.Compiler.Pipeline.Package.Loader.EvalEff where

import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Language.Value
import Juvix.Compiler.Pipeline.Package.Loader.Versions

data TypeSpec = TypeSpec
  { _typeSpecName :: Text,
    _typeSpecFile :: Path Abs File,
    _typeSpecVersion :: PackageVersion
  }

makeLenses ''TypeSpec

data EvalEff :: Effect where
  Eval' :: Node -> EvalEff m Value
  LookupIdentifier :: Text -> EvalEff m Node
  -- | Assert that the Node has a type given by one of the 'TypeSpec's
  AssertNodeType :: (Foldable f) => Node -> f TypeSpec -> EvalEff m TypeSpec

makeSem ''EvalEff

type EvalFileEff = Provider EvalEff (Path Abs File)
