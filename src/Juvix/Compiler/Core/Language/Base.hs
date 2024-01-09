module Juvix.Compiler.Core.Language.Base
  ( Info,
    Key,
    IsInfo,
    module Juvix.Compiler.Core.Language.Builtins,
    module Juvix.Prelude,
    module Juvix.Compiler.Core.Language.Base,
  )
where

import GHC.Show qualified as Show
import Juvix.Compiler.Core.Info (Info, IsInfo, Key)
import Juvix.Compiler.Core.Language.Builtins
import Juvix.Extra.Serialize
import Juvix.Prelude
import Prettyprinter

type Location = Interval

-- | Consecutive symbol IDs for reachable user functions.
data Symbol = Symbol
  { _symbolModuleId :: ModuleId,
    _symbolId :: Word
  }
  deriving stock (Ord, Eq, Generic)

instance Serialize Symbol

instance Hashable Symbol

instance Pretty Symbol where
  pretty Symbol {..} = pretty _symbolId <> "@" <> pretty _symbolModuleId

instance Show Symbol where
  show = show . pretty

defaultSymbol :: Word -> Symbol
defaultSymbol = Symbol defaultModuleId

uniqueName :: Text -> Symbol -> Text
uniqueName txt sym = txt <> "_" <> show sym

data TagUser = TagUser
  { _tagUserModuleId :: ModuleId,
    _tagUserWord :: Word
  }
  deriving stock (Eq, Generic, Ord, Show)

instance Hashable TagUser

instance Serialize TagUser

-- | Tag of a constructor, uniquely identifying it. Tag values are consecutive
-- and separate from symbol IDs. We might need fixed special tags in Core for
-- common "builtin" constructors, e.g., unit, nat, so that the code generator
-- can treat them specially.
data Tag
  = BuiltinTag BuiltinDataTag
  | UserTag TagUser
  deriving stock (Eq, Generic, Ord, Show)

instance Hashable Tag

instance Serialize Tag

isBuiltinTag :: Tag -> Bool
isBuiltinTag = \case
  BuiltinTag {} -> True
  UserTag {} -> False

-- | de Bruijn index
type Index = Int

-- | de Bruijn level (reverse de Bruijn index)
type Level = Int

getUserTagId :: Tag -> Maybe Word
getUserTagId = \case
  UserTag TagUser {..} -> Just _tagUserWord
  BuiltinTag {} -> Nothing

-- | The first argument `bl` is the current binder level (the number of binders
-- upward).
getBinderLevel :: Level -> Index -> Level
getBinderLevel bl idx = bl - idx - 1

-- | The first argument `bl` is the current binder level (the number of binders
-- upward).
getBinderIndex :: Level -> Level -> Index
getBinderIndex bl lvl = bl - lvl - 1

makeLenses ''Symbol
makeLenses ''TagUser
