module Juvix.Compiler.Concrete.Translation.ImportScanner.Megaparsec
  ( module Juvix.Compiler.Concrete.Translation.ImportScanner.Base,
    scanBSImports,
  )
where

import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Parser.Error
import Juvix.Prelude
import Juvix.Prelude.Parsing

scanBSImports ::
  (Members '[Error MegaparsecError] r) =>
  Path Abs File ->
  ByteString ->
  Sem r (HashSet ImportScan)
scanBSImports fp inputBS = undefined
