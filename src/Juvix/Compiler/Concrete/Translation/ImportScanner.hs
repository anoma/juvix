module Juvix.Compiler.Concrete.Translation.ImportScanner
  ( module Juvix.Compiler.Concrete.Translation.ImportScanner,
    module Juvix.Parser.Error,
  )
where

import Juvix.Compiler.Concrete.Translation.ImportScanner.FlatParse qualified as FlatParse
import Juvix.Compiler.Concrete.Translation.ImportScanner.Megaparsec qualified as Megaparsec
import Juvix.Parser.Error
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Prelude (show)

data ImportScanStrategy
  = -- | use FlatParse first and fallback to megaparsec if it fails
    ImportScanStrategyFallback
  | ImportScanStrategyFlatParse
  | ImportScanStrategyMegaparsec
  deriving stock (Eq, Data, Ord, Enum, Bounded)

instance Show ImportScanStrategy where
  show :: ImportScanStrategy -> String
  show = \case
    ImportScanStrategyFallback -> "flatparse-megaparsec"
    ImportScanStrategyFlatParse -> "flatparse"
    ImportScanStrategyMegaparsec -> "megaparsec"

defaultImportScanStrategy :: ImportScanStrategy
defaultImportScanStrategy = ImportScanStrategyFallback

scanFileImports ::
  (Members '[Reader ImportScanStrategy, Logger, Files, Error ParserError] r) =>
  Path Abs File ->
  Sem r ScanResult
scanFileImports file = readFileBS' file >>= scanBSImports file

scanBSImports ::
  (Members '[Reader ImportScanStrategy, Logger, Error ParserError] r) =>
  Path Abs File ->
  ByteString ->
  Sem r ScanResult
scanBSImports fp inputBS = do
  strat <- adaptStrategy <$> ask
  case strat of
    ImportScanStrategyFallback ->
      case FlatParse.scanBSImports fp inputBS of
        Just x -> return x
        Nothing -> do
          logWarn (mkAnsiText ("The FlatParse parser failed to scan the file " <> toFilePath @Text fp <> ". Falling back to MegaParsec."))
          Megaparsec.scanBSImports fp inputBS
    ImportScanStrategyFlatParse -> case FlatParse.scanBSImports fp inputBS of
      Nothing ->
        throw $
          ErrFlatParseError
            FlatParseError
              { _flatParseErrorLoc = fileLoc
              }
      Just r -> return r
    ImportScanStrategyMegaparsec -> Megaparsec.scanBSImports fp inputBS
  where
    adaptStrategy :: ImportScanStrategy -> ImportScanStrategy
    adaptStrategy = \case
      ImportScanStrategyFallback
        | not (isJuvixFile fp) -> ImportScanStrategyMegaparsec
      s -> s

    fileLoc :: Interval
    fileLoc =
      Interval
        { _intervalFile = fp,
          _intervalStart = tmpFileLoc,
          _intervalEnd = tmpFileLoc
        }

    tmpFileLoc :: FileLoc
    tmpFileLoc =
      FileLoc
        { _locLine = mempty,
          _locCol = mempty,
          _locOffset = mempty
        }
