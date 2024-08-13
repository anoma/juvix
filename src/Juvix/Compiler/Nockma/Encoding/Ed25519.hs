module Juvix.Compiler.Nockma.Encoding.Ed25519 where

import Data.ByteString qualified as BS
import Juvix.Prelude.Base

signatureLength :: Int
signatureLength = 64

publicKeyLength :: Int
publicKeyLength = 32

privateKeyLength :: Int
privateKeyLength = 64

-- | Remove the Ed25519 signature from a signed message.
-- The signaure of an Ed25519 message is the first 64 bytes of the signed message.
removeSignature :: ByteString -> ByteString
removeSignature = BS.drop signatureLength
