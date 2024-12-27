module Juvix.Compiler.Concrete.Translation.ImportScanner.Base where

import FlatParse.Basic
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

data ImportScan' a = ImportScan
  { _importNames :: NonEmpty String,
    _importLoc :: a
  }
  deriving stock (Show, Eq, Generic)

type ImportScanParsed = ImportScan' Span

type ImportScan = ImportScan' Interval

newtype ScanResult = ScanResult
  { _scanResultImports :: HashSet ImportScan
  }
  deriving stock (Eq)

makeLenses ''ImportScan'
makeLenses ''ScanResult

instance (Hashable a) => Hashable (ImportScan' a)

instance HasLoc ImportScan where
  getLoc = (^. importLoc)

instance Pretty (ImportScan' a) where
  pretty :: ImportScan' a -> Doc ann
  pretty s =
    Str.import_ <+> unAnnotate (importScanPrettyName s)

importScanPretty :: ImportScan' a -> Doc CodeAnn
importScanPretty s = kwImport <+> importScanPrettyName s

importScanPrettyName :: ImportScan' a -> Doc CodeAnn
importScanPrettyName ImportScan {..} =
  annotate
    (AnnKind KNameTopModule)
    (pretty (mconcat (intersperse "." (toList _importNames))))

-- | The relative path does not have a file extension
importScanToRelPath :: ImportScan' a -> Path Rel File
importScanToRelPath ImportScan {..} = relFile (joinFilePaths _importNames)

data ParseError = ParseError

data Token
  = TokenString
  | TokenImport ImportScanParsed
  | TokenReserved
  | TokenCode

makePrisms ''Token
