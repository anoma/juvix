module Juvix.Compiler.Nockma.Evaluator.Storage where

import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base

newtype Storage a = Storage
  {_storageKeyValueData :: HashMap (Term a) (Term a)}

emptyStorage :: (Hashable a) => Storage a
emptyStorage = Storage {_storageKeyValueData = mempty}

makeLenses ''Storage
