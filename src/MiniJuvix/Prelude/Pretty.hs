module MiniJuvix.Prelude.Pretty
  ( module MiniJuvix.Prelude.Pretty,
    module Prettyprinter,
  )
where

import MiniJuvix.Prelude.Base
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text qualified as Text

class HasAnsiBackend a where
  toAnsi :: a -> SimpleDocStream Ansi.AnsiStyle

class HasTextBackend a where
  toText :: a -> SimpleDocStream b

renderIO :: (HasAnsiBackend a, HasTextBackend a) => Bool -> a -> IO ()
renderIO useColors = hRenderIO useColors stdout

hRenderIO :: (HasAnsiBackend a, HasTextBackend a) => Bool -> Handle -> a -> IO ()
hRenderIO useColors h
  | useColors = Ansi.renderIO h . toAnsi
  | otherwise = Text.renderIO h . toText

toAnsiText :: (HasAnsiBackend a, HasTextBackend a) => Bool -> a -> Text
toAnsiText useColors
  | useColors = Ansi.renderStrict . toAnsi
  | otherwise = Text.renderStrict . toText

prettyText :: Pretty a => a -> Text
prettyText = Text.renderStrict . layoutPretty defaultLayoutOptions . pretty
