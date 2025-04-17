module Anoma.Effect.AddTransaction
  ( module Anoma.Effect.AddTransaction,
  )
where

import Anoma.Effect.Base
import Anoma.Http.AddTransaction
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Prelude
import Juvix.Prelude.Aeson qualified as Aeson

newtype AddTransactionInput = AddTransactionInput
  { _addTransactionInputCandidate :: Nockma.Term Natural
  }

makeLenses ''AddTransactionInput

addTransaction ::
  forall r.
  (Members '[Anoma, Error SimpleError, Logger] r) =>
  AddTransactionInput ->
  Sem r ()
addTransaction i = do
  let msg =
        AddTransaction
          { _addTransactionTransaction = encodeJam64 (i ^. addTransactionInputCandidate),
            _addTransactionType = TransactionTransparent
          }
  logMessageValue "Request payload" msg
  resVal <- anomaPost addTransactionUrl (Aeson.toJSON msg)
  logMessageValue "Response payload" resVal
  res :: Response <- case Aeson.fromJSON resVal of
    Aeson.Success r ->
      return $ ResponseSuccess r
    _ -> do
      r <- fromJSONErr resVal
      return $ ResponseError r
  case res of
    ResponseSuccess AddTransactionSuccess {..} ->
      logVerbose ("Transaction added successfully: " <> show _successMessage)
    ResponseError AddTransactionError {..} -> do
      logError ("Failed to add transaction: " <> show _errorError)
      throw (SimpleError (("Failed to add transaction: " <> show _errorError)))
