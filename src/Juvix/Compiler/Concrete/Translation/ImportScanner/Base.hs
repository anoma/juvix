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

data ScanResult = ScanResult
  { _scanResultModule :: ScannedTopModuleName,
    _scanResultImports :: HashSet ImportScan
  }

data ScannedTopModuleName = ScannedTopModuleName
  { _scannedTopModuleNameParts :: NonEmpty String,
    _scannedTopModuleLoc :: Interval
  }
  deriving stock (Show, Eq)

makeLenses ''ImportScan'
makeLenses ''ScanResult
makeLenses ''ScannedTopModuleName

scannedTopModuleNameToRelPath :: ScannedTopModuleName -> Path Rel File
scannedTopModuleNameToRelPath = relFile . scannedTopModuleNameToFilePath

scannedTopModuleNameToFilePath :: ScannedTopModuleName -> FilePath
scannedTopModuleNameToFilePath = joinFilePaths . (^. scannedTopModuleNameParts)

scannedTopModuleNameToDottedString :: ScannedTopModuleName -> String
scannedTopModuleNameToDottedString = intercalate "." . toList . (^. scannedTopModuleNameParts)

instance Pretty ScannedTopModuleName where
  pretty = pretty . scannedTopModuleNameToDottedString

instance (Hashable a) => Hashable (ImportScan' a)

instance HasLoc ScannedTopModuleName where
  getLoc = (^. scannedTopModuleLoc)

instance HasLoc ImportScan where
  getLoc = (^. importLoc)

instance Pretty (ImportScan' a) where
  pretty s =
    Str.import_ <+> unAnnotate (importScanPrettyName s)

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
