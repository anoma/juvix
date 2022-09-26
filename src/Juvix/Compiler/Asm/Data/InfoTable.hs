module Juvix.Compiler.Asm.Data.InfoTable where

import Juvix.Compiler.Asm.Language
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
    -- | `_functionArgsNum` may be different from `length (typeArgs
    -- (_functionType))` only if it is 0 (the "function" takes zero arguments)
    -- and the result is a function.
    _functionArgsNum :: Int,
    _functionType :: Type,
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
    _constructorRepresentation :: MemRep
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [ConstructorInfo]
  }

-- | Memory representation of a constructor.
data MemRep
  = -- | Standard representation of a constructor: [tag, field 0, .., field n]
    MemRepConstr
  | -- | A constructor with zero fields (arguments) can be represented as an
    -- unboxed tag.
    MemRepTag
  | -- | If a constructor is the only non-zero-field constructor in its inductive
    -- type, then it can be represented as a tagless tuple (the tag is not needed
    -- to distinguish from other unboxed tag constructors)
    MemRepTuple
  | -- | If a zero-field constructor is the only constructor in its inductive
    -- type, then it's a unit and can be omitted altogether.
    MemRepUnit
  | -- | If a constructor has exactly one field and there are no other
    -- constructors in its inductive type (in Haskell, such types are
    -- "newtypes"), then it can be unpacked and represented by the value of its
    -- field. If the tags are globally unique, this can be applied even if there
    -- are other constructors, as long as no two unpacked fields have the same
    -- type or an untagged representation (we can't distinguish between tuples
    -- representing constructors of different inductive types because they have
    -- no tag).
    MemRepUnpacked

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
