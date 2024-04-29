module Juvix.Compiler.Concrete.Translation.ImportScanner
  ( module Juvix.Compiler.Concrete.Translation.ImportScanner,
    module Juvix.Parser.Error,
  )
where

import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Compiler.Concrete.Translation.ImportScanner.FlatParse qualified as FlatParse
import Juvix.Compiler.Concrete.Translation.ImportScanner.Megaparsec qualified as Megaparsec
import Juvix.Parser.Error
import Juvix.Prelude
import Prelude (show)

data ScanImportStrategy
  = -- | use FlatParse first and fallback to megaparsec if it fails
    ScanImportStrategyFallback
  | ScanImportStrategyFlatParse
  | ScanImportStrategyMegaparsec
  deriving stock (Eq)

instance Show ScanImportStrategy where
  show :: ScanImportStrategy -> String
  show = \case
    ScanImportStrategyFallback -> "flatparse-megaparsec"
    ScanImportStrategyFlatParse -> "flatparse"
    ScanImportStrategyMegaparsec -> "megaparsec"

defaultScanImportStrategy :: ScanImportStrategy
defaultScanImportStrategy = ScanImportStrategyFallback

scanFileImports ::
  (Members '[Files, Error ParserError] r) =>
  ScanImportStrategy ->
  Path Abs File ->
  Sem r (HashSet ImportScan)
scanFileImports strat file = readFileBS' file >>= scanBSImports strat file

scanBSImports ::
  (Members '[Error ParserError] r) =>
  ScanImportStrategy ->
  Path Abs File ->
  ByteString ->
  Sem r (HashSet ImportScan)
scanBSImports strat fp inputBS =
  case strat of
    ScanImportStrategyFallback ->
      case FlatParse.scanBSImports fp inputBS of
        Just x -> return x
        Nothing -> Megaparsec.scanBSImports fp inputBS
    ScanImportStrategyFlatParse -> case FlatParse.scanBSImports fp inputBS of
      Nothing -> error "Flatparse parser error"
      Just r -> return r
    ScanImportStrategyMegaparsec -> Megaparsec.scanBSImports fp inputBS
