module Juvix.Compiler.Asm.Extra
  ( module Juvix.Compiler.Asm.Extra,
    module Juvix.Compiler.Asm.Extra.Base,
    module Juvix.Compiler.Asm.Extra.Type,
    module Juvix.Compiler.Asm.Extra.Recursors,
    module Juvix.Compiler.Asm.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Recursors
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language

validateCode :: forall r. Member (Error AsmError) r => InfoTable -> Arguments -> Code -> Sem r ()
validateCode tab args = void . recurse sig args
  where
    sig :: RecursorSig r ()
    sig =
      RecursorSig
        { _recursorInfoTable = tab,
          _recurseInstr = \_ _ -> return (),
          _recurseBranch = \_ _ _ _ -> return (),
          _recurseCase = \_ _ _ _ -> return ()
        }

validateFunction :: Member (Error AsmError) r => InfoTable -> FunctionInfo -> Sem r ()
validateFunction tab fi = validateCode tab args (fi ^. functionCode)
  where
    args = HashMap.fromList $ zip [0..] (typeArgs (fi ^. functionType))

validateInfoTable :: Member (Error AsmError) r => InfoTable -> Sem r ()
validateInfoTable tab = mapM_ (validateFunction tab) (HashMap.elems (tab ^. infoFunctions))

validate :: InfoTable -> Maybe AsmError
validate tab =
  case run $ runError $ validateInfoTable tab of
    Left err -> Just err
    _ -> Nothing
