module Juvix.Compiler.Verification.Dumper
  ( Dumper,
    dump,
    runDumper,
    module Juvix.Compiler.Verification.Data.Lang,
  )
where

import Data.Text qualified as Text
import Juvix.Compiler.Verification.Data.Lang
import Juvix.Prelude

data Dumper :: Effect where
  Dump :: Lang -> Text -> Dumper m ()

makeSem ''Dumper

data DumperState = DumperState
  { _dumperStateDumps :: [(Lang, Text)]
  }

makeLenses ''DumperState

ppImport :: Lang -> Text
ppImport = \case
  LangCore -> "import Juvix.Core.Main"

ppEquiv :: (Lang, Text) -> (Lang, Text) -> Text
ppEquiv (lang1, text1) (lang2, text2) =
  case (lang1, lang2) of
    (LangCore, LangCore) -> text1 <> " ≈ " <> text2

ppDumps :: [(Lang, Text)] -> Text
ppDumps dumps = Text.unlines imports <> "\n" <> go 0 dumps
  where
    imports = map (ppImport . fst) dumps

    go :: Int -> [(Lang, Text)] -> Text
    go n = \case
      [] -> ""
      [_] -> ""
      d1 : d2 : rest ->
        "lemma step_equiv_" <> show n <> " : " <> ppEquiv d1 d2 <> " := by sorry\n" <> go (n + 1) (d2 : rest)

runDumper :: forall r a. (Member Files r) => Path Abs File -> Sem (Dumper ': r) a -> Sem r a
runDumper path a = do
  (st, res) <- reinterpret (runState (DumperState [])) interp a
  writeFileEnsureLn' path (ppDumps (st ^. dumperStateDumps))
  return res
  where
    interp :: forall m b. Dumper m b -> Sem (State DumperState ': r) b
    interp = \case
      Dump lang text -> modify (over dumperStateDumps ((lang, text) :))
