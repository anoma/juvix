module Juvix.Compiler.Backend.Geb.Translation
  ( module Juvix.Compiler.Backend.Geb.Translation,
    module Juvix.Compiler.Backend.Geb.Translation.FromCore,
  )
where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty
import Juvix.Compiler.Backend.Geb.Translation.FromCore

newtype Result = Result
  { _resultCode :: Text
  }

toResult :: Morphism -> Result
toResult geb = Result (ppPrint geb <> "\n")
