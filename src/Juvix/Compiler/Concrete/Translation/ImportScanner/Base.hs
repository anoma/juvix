module Juvix.Compiler.Concrete.Translation.ImportScanner.Base where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

newtype ImportScan = ImportScan
  { _importNames :: NonEmpty String
  }
  deriving stock (Show, Eq, Generic)

instance Hashable ImportScan

instance Pretty ImportScan where
  pretty (ImportScan l) =
    Str.import_ <+> pretty (mconcat (intersperse "." (toList l)))

-- | The relative path does not have a file extension
importScanToRelPath :: ImportScan -> Path Rel File
importScanToRelPath (ImportScan l) = relFile (joinFilePaths l)

data ParseError = ParseError

data Token
  = TokenString
  | TokenImport ImportScan
  | TokenReserved
  | TokenCode
