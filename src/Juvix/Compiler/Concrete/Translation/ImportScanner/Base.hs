module Juvix.Compiler.Concrete.Translation.ImportScanner.Base where

import FlatParse.Basic
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

data ImportScan' a = ImportScan
  { _importNames :: NonEmpty String,
    _importLoc :: a
  }
  deriving stock (Show, Eq, Generic)

makeLenses ''ImportScan'

type ImportScanParsed = ImportScan' Span

type ImportScan = ImportScan' Interval

instance (Hashable a) => Hashable (ImportScan' a)

instance HasLoc ImportScan where
  getLoc = (^. importLoc)

instance Pretty (ImportScan' a) where
  pretty ImportScan {..} =
    Str.import_ <+> pretty (mconcat (intersperse "." (toList _importNames)))

-- | The relative path does not have a file extension
importScanToRelPath :: ImportScan' a -> Path Rel File
importScanToRelPath ImportScan {..} = relFile (joinFilePaths _importNames)

data ParseError = ParseError

data Token
  = TokenString
  | TokenImport ImportScanParsed
  | TokenReserved
  | TokenCode
