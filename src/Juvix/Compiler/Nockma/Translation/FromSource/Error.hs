module Juvix.Compiler.Nockma.Translation.FromSource.Error where

import Juvix.Compiler.Nockma.Encoding.Cue qualified as Cue
import Juvix.Compiler.Nockma.Language (NockNaturalNaturalError)
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data CueDecodingError
  = ErrDecodingError Cue.DecodingError
  | ErrNockNaturalError NockNaturalNaturalError
  deriving stock (Show)

instance Pretty CueDecodingError where
  pretty = \case
    ErrDecodingError e -> undefined
