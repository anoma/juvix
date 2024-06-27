module Juvix.Compiler.Backend.VampIR.Translation
  ( module Juvix.Compiler.Backend.VampIR.Translation,
  )
where

import Juvix.Compiler.Backend.VampIR.Language
import Juvix.Compiler.Backend.VampIR.Pretty
import Juvix.Compiler.Backend.VampIR.Translation.FromCore qualified as VampIR
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Pipeline.EntryPoint

newtype Result = Result
  { _resultCode :: Text
  }

makeLenses ''Result

toResult :: Bool -> Program -> Result
toResult unsafe p = Result $ show $ ppOut opts p
  where
    opts = defaultOptions {_optUnsafe = unsafe}

fromCore :: (Member (Reader EntryPoint) r) => InfoTable -> Sem r Result
fromCore tab = do
  unsafe <- asks (^. entryPointUnsafe)
  return $ toResult unsafe $ VampIR.fromCore tab
