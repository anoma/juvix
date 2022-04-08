module MiniJuvix.Prelude.Pretty where

import MiniJuvix.Prelude.Base
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text qualified as Text

class HasAnsiBackend a where
  toAnsi :: a -> SimpleDocStream Ansi.AnsiStyle

class HasTextBackend a where
  toText :: a -> SimpleDocStream b

toAnsiText :: (HasAnsiBackend a, HasTextBackend a) => Bool -> a -> Text
toAnsiText noColors
  | noColors = Ansi.renderStrict . toText
  | otherwise = Text.renderStrict . toAnsi
