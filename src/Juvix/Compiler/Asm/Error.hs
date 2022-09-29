module Juvix.Compiler.Asm.Error where

import Juvix.Compiler.Asm.Language
import Juvix.Data.PPOutput
import Text.Megaparsec.Pos qualified as M

data AsmError = AsmError
  { _asmErrorLoc :: Maybe Location,
    _asmErrorMsg :: Text
  }

makeLenses ''AsmError

instance ToGenericError AsmError where
  genericError :: Member (Reader GenericOptions) r => AsmError -> Sem r GenericError
  genericError e = ask >>= generr
    where
      generr :: GenericOptions -> Sem r GenericError
      generr _ =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          i = getLoc e
          msg = pretty (e ^. asmErrorMsg)

instance Pretty AsmError where
  pretty (AsmError {..}) = pretty _asmErrorMsg

instance HasLoc AsmError where
  getLoc (AsmError {..}) = fromMaybe defaultLoc _asmErrorLoc
    where
      defaultLoc = singletonInterval (mkLoc "" 0 (M.initialPos ""))
