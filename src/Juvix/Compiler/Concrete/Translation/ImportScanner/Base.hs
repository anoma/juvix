module Juvix.Compiler.Concrete.Translation.ImportScanner.Base where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

data ImportScan = ImportScan
  { _importNames :: NonEmpty String
  }
  deriving stock (Show, Eq, Generic)

instance Hashable ImportScan

instance Pretty ImportScan where
  pretty (ImportScan l) =
    Str.import_ <+> pretty (mconcat (intersperse "." (toList l)))

importScanToRelPath :: ImportScan -> FileExt -> Path Rel File
importScanToRelPath (ImportScan l) ext = relFile (joinFilePaths l <.> fileExtToString ext)

data ParseError = ParseError

data Token
  = TokenString
  | TokenImport ImportScan
  | TokenReserved
  | TokenCode
