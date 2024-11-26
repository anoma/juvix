module Anoma.Effect.AddTransaction where

import Anoma.Effect.Base
import Anoma.Rpc.AddTransaction
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Prelude
import Juvix.Prelude.Aeson qualified as Aeson

newtype AddTransactionInput = AddTransactionInput
  { _mempoolAddTransactionInputCandidate :: Nockma.Term Natural
  }

makeLenses ''AddTransactionInput

addTransaction ::
  forall r.
  (Members '[Anoma, Error SimpleError, Logger] r) =>
  AddTransactionInput ->
  Sem r ()
addTransaction i = do
  nodeInfo <- getNodeInfo
  let msg =
        AddTransaction
          { _addTransactionNodeInfo = nodeInfo,
            _addTransactionTransaction = encodeJam64 (i ^. mempoolAddTransactionInputCandidate)
          }
  logMessageValue "Request payload" msg
  -- addTransaction always returns an empty response
  anomaRpc addTransactionGrpcUrl (Aeson.toJSON msg) >>= fromJSONErr
