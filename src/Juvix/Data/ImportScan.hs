module Juvix.Data.ImportScan where

import FlatParse.Basic
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Data.CodeAnn
import Juvix.Data.Loc
import Juvix.Data.TopModulePathKey
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude.Base

data ImportScan' a = ImportScan
  { _importScanKey :: TopModulePathKey,
    _importScanLoc :: a
  }
  deriving stock (Show, Eq, Generic)

instance (NFData a) => NFData (ImportScan' a)

type ImportScanParsed = ImportScan' Span

type ImportScan = ImportScan' Interval

newtype ScanResult = ScanResult
  { _scanResultImports :: HashSet ImportScan
  }
  deriving stock (Eq)

makeLenses ''ImportScan'
makeLenses ''ScanResult

instance (Hashable a) => Hashable (ImportScan' a)

instance (Serialize a) => Serialize (ImportScan' a)

instance HasLoc ImportScan where
  getLoc = (^. importScanLoc)

instance Pretty (ImportScan' a) where
  pretty :: ImportScan' a -> Doc ann
  pretty s =
    Str.import_ <+> unAnnotate (importScanPrettyName s)

instance PrettyCodeAnn (ImportScan' a) where
  ppCodeAnn s = kwImport <+> importScanPrettyName s

importScanPrettyName :: ImportScan' a -> Doc CodeAnn
importScanPrettyName ImportScan {..} =
  annotate
    (AnnKind KNameTopModule)
    (pretty _importScanKey)

-- | The relative path does not have a file extension
importScanToRelPath :: ImportScan' a -> Path Rel File
importScanToRelPath ImportScan {..} = topModulePathKeyToRelativePathNoExt _importScanKey

topModulePathImportScan :: TopModulePath -> ImportScan
topModulePathImportScan t =
  ImportScan
    { _importScanKey = topModulePathKey t,
      _importScanLoc = getLoc t
    }
