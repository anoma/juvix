module Juvix.Compiler.Asm.Data.InfoTable
  ( module Juvix.Compiler.Asm.Data.InfoTable,
    module Juvix.Compiler.Asm.Language.Rep,
    module Juvix.Compiler.Asm.Language.Type,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Rep
import Juvix.Compiler.Asm.Language.Type

data InfoTable = InfoTable
  { _infoFunctions :: HashMap Symbol FunctionInfo,
    _infoConstrs :: HashMap Tag ConstructorInfo,
    _infoInductives :: HashMap Symbol InductiveInfo,
    _infoMainFunction :: Maybe Symbol
  }

data FunctionInfo = FunctionInfo
  { _functionName :: Text,
    _functionLocation :: Maybe Location,
    _functionSymbol :: Symbol,
    _functionArgNames :: [Maybe Text],
    -- | `_functionArgsNum` may be different from `length (typeArgs
    -- (_functionType))` only if it is 0 (the "function" takes zero arguments)
    -- and the result is a function.
    _functionArgsNum :: Int,
    _functionType :: Type,
    _functionMaxValueStackHeight :: Int,
    _functionMaxTempStackHeight :: Int,
    _functionCode :: Code
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Text,
    _constructorLocation :: Maybe Location,
    _constructorTag :: Tag,
    -- | `_constructorArgsNum` should always be equal to `length (typeArgs
    -- (_constructorType))`. It is stored separately mainly for the benefit of
    -- the interpreter (so it does not have to recompute it every time).
    _constructorArgsNum :: Int,
    -- | Constructor types are assumed to be fully uncurried, i.e., `uncurryType
    -- _constructorType == _constructorType`
    _constructorType :: Type,
    _constructorInductive :: Symbol,
    _constructorRepresentation :: MemRep,
    _constructorFixity :: Maybe Fixity
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [Tag],
    _inductiveRepresentation :: IndRep
  }

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''InductiveInfo

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoFunctions = mempty,
      _infoConstrs = mempty,
      _infoInductives = mempty,
      _infoMainFunction = Nothing
    }

lookupFunInfo :: InfoTable -> Symbol -> FunctionInfo
lookupFunInfo infoTable sym = fromMaybe (error "invalid function symbol") (HashMap.lookup sym (infoTable ^. infoFunctions))

lookupConstrInfo :: InfoTable -> Tag -> ConstructorInfo
lookupConstrInfo infoTable tag = fromMaybe (error "invalid constructor tag") (HashMap.lookup tag (infoTable ^. infoConstrs))

lookupInductiveInfo :: InfoTable -> Symbol -> InductiveInfo
lookupInductiveInfo infoTable sym = fromMaybe (error "invalid inductive symbol") (HashMap.lookup sym (infoTable ^. infoInductives))
