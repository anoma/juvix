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
import Path qualified
import System.FilePath

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
          <> d2
          ^. dumpInfoPhase
          <> " : "
          <> ppEquiv d1 d2
          <> " := by\n  sorry\n\n"
          <> go (n + 1) (d2 : rest)

writeLeanProjectFiles :: (Member Files r) => Path Abs Dir -> Text -> Sem r ()
writeLeanProjectFiles dirPath name = do
  let lakeFile = dirPath Path.</> $(mkRelFile "lakefile.toml")
      toolchainFile = dirPath Path.</> $(mkRelFile "lean-toolchain")
      gitignoreFile = dirPath Path.</> $(mkRelFile ".gitignore")
  writeFileEnsureLn'
    lakeFile
    $ "name = \""
    <> name
    <> "_lean\"\n"
    <> "defaultTargets = [\""
    <> name
    <> "\"]\n\n"
    <> "[[require]]\n"
    <> "name = \"juvix-lean\"\n"
    <> "git = \"https://github.com/anoma/juvix-lean.git\"\n"
    <> "rev = \"main\"\n\n"
    <> "[[lean_lib]]\n"
    <> "name = \""
    <> name
    <> "\"\n"
  writeFileEnsureLn' toolchainFile "leanprover/lean4:v4.18.0-rc1\n"
  writeFileEnsureLn' gitignoreFile ".lake\n"

runDumper :: forall r a. (Members '[Files, Reader EntryPoint] r) => Sem (Dumper ': r) a -> Sem r a
runDumper a = do
  entry <- ask
  case entry ^. entryPointMainFile of
    Just sourcePath
      | entryPointVerificationEnabled entry -> do
          let name = dropExtension (toFilePath (filename sourcePath))
          subdir <- parseRelDir (name <> "_lean")
          let dirPath = parent sourcePath Path.</> subdir
          file <- parseRelFile (name <> ".lean")
          let filePath = dirPath Path.</> file
          ensureDir' dirPath
          writeLeanProjectFiles dirPath (fromString name)
          runDumper' filePath a
    _ ->
      ignoreDumper a

runDumper' :: forall r a. (Member Files r) => Path Abs File -> Sem (Dumper ': r) a -> Sem r a
runDumper' path a = do
  (st, res) <- reinterpret (runState (DumperState [])) interp a
  when (not . null $ st ^. dumperStateDumps)
    $ writeFileEnsureLn' path (ppDumps (reverse (st ^. dumperStateDumps)))
  return res
  where
    interp :: forall m b. Dumper m b -> Sem (State DumperState ': r) b
    interp = \case
      Dump di -> modify (over dumperStateDumps (di :))

ignoreDumper :: Sem (Dumper ': r) a -> Sem r a
ignoreDumper = interpret $ \case
  Dump _ -> pure ()
