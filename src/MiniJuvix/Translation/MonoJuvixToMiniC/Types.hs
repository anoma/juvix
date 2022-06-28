module MiniJuvix.Translation.MonoJuvixToMiniC.Types where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MiniC.Language
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Translation.MonoJuvixToMiniC.BuiltinTable

newtype MiniCResult = MiniCResult
  { _resultCCode :: Text
  }

data BindingInfo = BindingInfo
  { _bindingInfoExpr :: Expression,
    _bindingInfoType :: CFunType
  }

newtype PatternInfoTable = PatternInfoTable
  {_patternBindings :: HashMap Text BindingInfo}

type CArity = Int

data ClosureInfo = ClosureInfo
  { _closureNameId :: Mono.NameId,
    _closureRootName :: Text,
    _closureBuiltin :: Maybe Mono.BuiltinPrim,
    _closureMembers :: [CDeclType],
    _closureFunType :: CFunType,
    _closureCArity :: CArity
  }
  deriving stock (Show, Eq)

closureNamedId :: ClosureInfo -> Text
closureNamedId ClosureInfo {..} = _closureRootName <> "_" <> show (length _closureMembers)

makeLenses ''ClosureInfo
makeLenses ''MiniCResult
makeLenses ''PatternInfoTable
makeLenses ''BindingInfo

closureRootFunction :: ClosureInfo -> Text
closureRootFunction c = case c ^. closureBuiltin of
  Just b -> fromMaybe unsup (builtinName b)
    where
      unsup :: a
      unsup = error ("unsupported builtin " <> show b)
  Nothing -> c ^. closureRootName
