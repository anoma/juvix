module Juvix.Compiler.Concrete.Translation.ImportScanner
  ( module Juvix.Compiler.Concrete.Translation.ImportScanner,
    module Juvix.Parser.Error,
  )
where

import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Compiler.Concrete.Translation.ImportScanner.FlatParse qualified as FlatParse
import Juvix.Parser.Error
import Juvix.Prelude

scanFileImports ::
  (Members '[Files, Error ParserError] r) =>
  Path Abs File ->
  Sem r (HashSet ImportScan)
scanFileImports file = readFileBS' file >>= scanBSImports file

scanBSImports ::
  (Members '[Error ParserError] r) =>
  Path Abs File ->
  ByteString ->
  Sem r (HashSet ImportScan)
scanBSImports fp inputBS = case FlatParse.scanBSImports fp inputBS of
  Just x -> return x
  Nothing -> undefined
