module MiniJuvix.Syntax.Eval where

--------------------------------------------------------------------------------

open import Haskell.Prelude
open import Agda.Builtin.Equality

open import MiniJuvix.Syntax.Core

-- Language extensions
{-# FOREIGN AGDA2HS
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
#-}

{-# FOREIGN AGDA2HS
import qualified MiniJuvix.Syntax.Core as Core
#-}

-- What is a value? it's simply a term that contains an answer, i.e. a
-- term that does no longer reduce.

data Value : Set where
  Universe : Value

{-# COMPILE AGDA2HS Value #-}
