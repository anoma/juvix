module Juvix.Compiler.Concrete.Data.Literal where

import Juvix.Data.Fixity
import Juvix.Extra.Serialize
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Prettyprinter

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

makeLenses ''IntegerWithBase

data Literal
  = LitString Text
  | LitIntegerWithBase IntegerWithBase
  deriving stock (Show, Eq, Ord, Generic, Data)

type LiteralLoc = WithLoc Literal

instance Hashable IntegerBase

instance Serialize IntegerBase

instance NFData IntegerBase

instance Hashable IntegerWithBase

instance Serialize IntegerWithBase

instance NFData IntegerWithBase

instance Hashable Literal

instance Serialize Literal

instance NFData Literal

instance HasAtomicity IntegerWithBase where
  atomicity = const Atom

instance HasAtomicity Literal where
  atomicity = \case
    LitString {} -> Atom
    LitIntegerWithBase h -> atomicity h

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

instance Pretty Literal where
  pretty = \case
    LitIntegerWithBase n -> pretty n
    LitString s -> ppStringLit s
      where
        ppStringLit :: Text -> Doc a
        ppStringLit = dquotes . pretty
