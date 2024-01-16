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
data MemRef' r
  = -- | A direct memory reference.
    DRef r
  | -- | ConstrRef is an indirect reference to a field (argument) of
    --  a constructor: field k holds the (k+1)th argument.
    ConstrRef (Field' r)

data OffsetRef = OffsetRef
  { _offsetRefOffset :: Offset,
    _offsetRefName :: Maybe Text
  }

-- | Constructor field reference.
data Field' r = Field
  { _fieldName :: Maybe Text,
    -- | tag of the constructor being referenced
    _fieldTag :: Tag,
    -- | location where the data is stored
    _fieldRef :: r,
    _fieldOffset :: Offset
  }
  deriving stock (Functor)

makeLenses ''Field'
makeLenses ''OffsetRef
