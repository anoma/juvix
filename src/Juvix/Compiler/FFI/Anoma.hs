module Juvix.Compiler.FFI.Anoma where

import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Prelude

data Anoma = Anoma
  { _anomaStdlibCall :: Nockma.Term Natural
  }
  deriving stock (Generic, Show)

instance NFData Anoma

instance Serialize Anoma

makeLenses ''Anoma

nockmaTermFromCoreNode :: Core.Node -> Maybe (Nockma.Term Natural)
nockmaTermFromCoreNode node =
  case node of
    Core.NCtr Core.Constr {..} ->
      case _constrArgs of
        [Core.NCst Core.Constant {..}] ->
          case _constantValue of
            Core.ConstInteger v
              | v >= 0 ->
                  return $
                    Nockma.TAtom (fromIntegral v)
            _ -> Nothing
        [arg1, arg2] -> do
          arg1' <- nockmaTermFromCoreNode arg1
          arg2' <- nockmaTermFromCoreNode arg2
          return $ Nockma.TCell arg1' arg2'
        _ -> Nothing
    _ -> Nothing

resolveAnomaFFI :: Core.Node -> Maybe (Core.Symbol, Anoma)
resolveAnomaFFI node =
  case node of
    Core.NCtr Core.Constr {..}
      | [Core.NIdt Core.Ident {..}, stdlibCall] <- _constrArgs -> do
          stdlibCall' <- nockmaTermFromCoreNode stdlibCall
          return (_identSymbol, Anoma {_anomaStdlibCall = stdlibCall'})
    _ -> Nothing
