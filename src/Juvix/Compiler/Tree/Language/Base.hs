module Juvix.Compiler.Tree.Language.Base
  ( module Juvix.Compiler.Tree.Language.Base,
    module Juvix.Compiler.Core.Language.Base,
  )
where

import Juvix.Compiler.Core.Language.Base
import Juvix.Data.Field

-- | Offset of a data field or an argument
type Offset = Int

-- | Constant values. Void is an unprintable unit.
data Constant
  = ConstInt Integer
  | ConstBool Bool
  | ConstString Text
  | ConstField FField
  | ConstUnit
  | ConstVoid
  | ConstUInt8 Word8
  | ConstByteArray ByteString
  deriving stock (Eq, Generic)

instance (Hashable Constant)

-- | MemRefs are references to values stored in memory.
data MemRef
  = -- | A direct memory reference.
    DRef DirectRef
  | -- | ConstrRef is an indirect reference to a field (argument) of
    --  a constructor: field k holds the (k+1)th argument.
    ConstrRef Field
  deriving stock (Eq)

data OffsetRef = OffsetRef
  { _offsetRefOffset :: Offset,
    _offsetRefName :: Maybe Text
  }
  deriving stock (Eq)

-- | DirectRef is a direct memory reference.
data DirectRef
  = -- | ArgRef references an argument in the argument area (0-based offsets).
    --   JVT/JVA code: 'arg[<offset>]'.
    ArgRef OffsetRef
  | -- | TempRef references a value in the temporary stack (0-based offsets,
    --   counted from the _bottom_ of the temporary stack). JVT/JVA code:
    --   'tmp[<offset>]'.
    TempRef RefTemp
  deriving stock (Eq)

mkTempRef :: OffsetRef -> DirectRef
mkTempRef o = TempRef (RefTemp o Nothing)

mkTempRef' :: Int -> Int -> DirectRef
mkTempRef' height idx =
  TempRef
    ( RefTemp
        { _refTempOffsetRef = OffsetRef {_offsetRefOffset = idx, _offsetRefName = Nothing},
          _refTempTempHeight = Just height
        }
    )

data RefTemp = RefTemp
  { _refTempOffsetRef :: OffsetRef,
    _refTempTempHeight :: Maybe Int
  }
  deriving stock (Eq)

-- | Constructor field reference. JVT/JVA code: '<dref>.<tag>[<offset>]'
data Field = Field
  { _fieldName :: Maybe Text,
    -- | tag of the constructor being referenced
    _fieldTag :: Tag,
    -- | location where the data is stored
    _fieldRef :: DirectRef,
    _fieldOffset :: Offset
  }
  deriving stock (Eq)

makeLenses ''Field
makeLenses ''OffsetRef
makeLenses ''DirectRef
makeLenses ''RefTemp
