module Juvix.Data.IntegerWithBase where

import Juvix.Data.Fixity
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data IntegerWithBase = IntegerWithBase
  { _integerWithBaseBase :: IntegerBase,
    _integerWithBaseValue :: Integer
  }
  deriving stock (Show, Eq, Ord, Generic, Data)

data IntegerBase
  = IntegerBaseBinary
  | IntegerBaseOctal
  | IntegerBaseDecimal
  | IntegerBaseHexadecimal
  deriving stock (Bounded, Enum, Show, Eq, Ord, Generic, Data)

instance Hashable IntegerWithBase

instance Serialize IntegerWithBase

instance NFData IntegerWithBase

instance Hashable IntegerBase

instance Serialize IntegerBase

instance NFData IntegerBase

makeLenses ''IntegerWithBase

integerBasePrefix :: IntegerBase -> Text
integerBasePrefix = \case
  IntegerBaseBinary -> Str.binaryPrefix
  IntegerBaseOctal -> Str.octalPrefix
  IntegerBaseDecimal -> ""
  IntegerBaseHexadecimal -> Str.hexadecimalPrefix

instance Pretty IntegerWithBase where
  pretty IntegerWithBase {..} =
    let sign
          | _integerWithBaseValue < 0 = "-"
          | otherwise = ""
     in sign
          <> pretty (integerBasePrefix _integerWithBaseBase)
          <> pretty (showNum (fromIntegral (abs _integerWithBaseValue)))
    where
      showNum :: Natural -> String
      showNum n = case _integerWithBaseBase of
        IntegerBaseBinary -> showBin n ""
        IntegerBaseOctal -> showOct n ""
        IntegerBaseDecimal -> showInt n ""
        IntegerBaseHexadecimal -> showHex n ""

instance HasAtomicity IntegerWithBase where
  atomicity = const Atom
