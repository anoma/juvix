module Juvix.Compiler.Verification.Dumper
  ( Dumper,
    dump,
    DumpInfo (..),
    runDumper,
    ignoreDumper,
    module Juvix.Compiler.Verification.Data.Lang,
  )
where

import Data.Text qualified as Text
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Verification.Data.Lang
import Juvix.Prelude

data DumpInfo = DumpInfo
  { _dumpInfoLang :: Lang,
    _dumpInfoPhase :: Text,
    _dumpInfoExpr :: Text
  }

makeLenses ''DumpInfo

data Dumper :: Effect where
  Dump :: DumpInfo -> Dumper m ()

makeSem ''Dumper

data DumperState = DumperState
  { _dumperStateDumps :: [DumpInfo]
  }

makeLenses ''DumperState

ppImport :: Lang -> Text
ppImport = \case
  LangCore -> "import Juvix.Core.Main\nopen Juvix.Core.Main"

ppEquiv :: DumpInfo -> DumpInfo -> Text
ppEquiv di1 di2 =
  case (di1 ^. dumpInfoLang, di2 ^. dumpInfoLang) of
    (LangCore, LangCore) -> di1 ^. dumpInfoExpr <> " ≈ " <> di2 ^. dumpInfoExpr

ppDumps :: [DumpInfo] -> Text
ppDumps dumps = Text.unlines imports <> "\n" <> go 0 dumps
  where
    imports = map ppImport . nubSort . map (^. dumpInfoLang) $ dumps

    go :: Int -> [DumpInfo] -> Text
    go n = \case
      [] -> ""
      [_] -> ""
      d1 : d2 : rest ->
        "lemma step_"
          <> show n
          <> "_"
          <> d2 ^. dumpInfoPhase
          <> " : "
          <> ppEquiv d1 d2
          <> " := by\n  sorry\n\n"
          <> go (n + 1) (d2 : rest)

dumperEnabled :: EntryPoint -> Bool
dumperEnabled entry = entry ^. entryPointMainFile == entry ^. entryPointModulePath

runDumper :: forall r a. (Members '[Files, Reader EntryPoint] r) => Sem (Dumper ': r) a -> Sem r a
runDumper a = do
  entry <- ask
  case entry ^. entryPointMainFile of
    Just sourcePath
      | dumperEnabled entry ->
          runDumper' (replaceExtension' ".lean" sourcePath) a
    _ ->
      ignoreDumper a

runDumper' :: forall r a. (Member Files r) => Path Abs File -> Sem (Dumper ': r) a -> Sem r a
runDumper' path a = do
  (st, res) <- reinterpret (runState (DumperState [])) interp a
  when (not . null $ st ^. dumperStateDumps) $
    writeFileEnsureLn' path (ppDumps (reverse (st ^. dumperStateDumps)))
  return res
  where
    interp :: forall m b. Dumper m b -> Sem (State DumperState ': r) b
    interp = \case
      Dump di -> modify (over dumperStateDumps (di :))

ignoreDumper :: Sem (Dumper ': r) a -> Sem r a
ignoreDumper = interpret $ \case
  Dump _ -> pure ()
