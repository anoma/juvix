module Juvix.Compiler.Backend.VampIR.Translation
  ( module Juvix.Compiler.Backend.VampIR.Translation,
    module Juvix.Compiler.Backend.VampIR.Translation.FromCore,
  )
where

import Juvix.Compiler.Backend.VampIR.Language
import Juvix.Compiler.Backend.VampIR.Pretty
import Juvix.Compiler.Backend.VampIR.Translation.FromCore

newtype Result = Result
  { _resultCode :: Text
  }

toResult :: Program -> Result
toResult p = Result $ ppPrint p

makeLenses ''Result
