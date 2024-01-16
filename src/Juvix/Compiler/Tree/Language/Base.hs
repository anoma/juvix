module Juvix.Compiler.Tree.Language.Base
  ( module Juvix.Compiler.Tree.Language.Base,
    module Juvix.Compiler.Core.Language.Base,
  )
where

import Juvix.Compiler.Core.Language.Base

-- | Offset of a data field or an argument
type Offset = Int

-- | Constant values. Void is an unprintable unit.
data Constant
  = ConstInt Integer
  | ConstBool Bool
  | ConstString Text
  | ConstUnit
  | ConstVoid

-- | MemRefs are references to values stored in memory.
data MemRef
  = -- | A direct memory reference.
    DRef DirectRef
  | -- | ConstrRef is an indirect reference to a field (argument) of
    --  a constructor: field k holds the (k+1)th argument.
    ConstrRef Field

data OffsetRef = OffsetRef
  { _offsetRefOffset :: Offset,
    _offsetRefName :: Maybe Text
  }

-- | DirectRef is a direct memory reference.
data DirectRef
  = -- | ArgRef references an argument in the argument area (0-based offsets).
    --   JVT/JVA code: 'arg[<offset>]'.
    ArgRef OffsetRef
  | -- | TempRef references a value in the temporary stack (0-based offsets,
    --   counted from the _bottom_ of the temporary stack). JVT/JVA code:
    --   'tmp[<offset>]'.
    TempRef OffsetRef

-- | Constructor field reference. JVT/JVA code: '<dref>.<tag>[<offset>]'
data Field = Field
  { _fieldName :: Maybe Text,
    -- | tag of the constructor being referenced
    _fieldTag :: Tag,
    -- | location where the data is stored
    _fieldRef :: DirectRef,
    _fieldOffset :: Offset
  }

makeLenses ''Field
makeLenses ''OffsetRef
makeLenses ''DirectRef
