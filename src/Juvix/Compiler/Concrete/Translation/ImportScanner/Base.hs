module Juvix.Compiler.Concrete.Translation.ImportScanner.Base where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

data ImportScan = ImportScan
  { _importNames :: NonEmpty String,
    _importLoc :: Interval
  }
  deriving stock (Show, Eq, Generic)

makeLenses ''ImportScan

instance Hashable ImportScan

instance HasLoc ImportScan where
  getLoc = (^. importLoc)

instance Pretty ImportScan where
  pretty ImportScan {..} =
    Str.import_ <+> pretty (mconcat (intersperse "." (toList _importNames)))

-- | The relative path does not have a file extension
importScanToRelPath :: ImportScan -> Path Rel File
importScanToRelPath ImportScan {..} = relFile (joinFilePaths _importNames)

data ParseError = ParseError

data Token
  = TokenString
  | TokenImport ImportScan
  | TokenReserved
  | TokenCode
