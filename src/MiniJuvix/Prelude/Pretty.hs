module MiniJuvix.Prelude.Pretty
  ( module MiniJuvix.Prelude.Pretty,
    module Prettyprinter,
  )
where

import MiniJuvix.Prelude.Base
import Prettyprinter hiding (hsep, vsep)
import Prettyprinter qualified as PP
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

vsep :: Foldable f => f (Doc a) -> Doc a
vsep = PP.vsep . toList

vsep2 :: [Doc a] -> Doc a
vsep2 = concatWith (\a b -> a <> line <> line <> b)

hsep :: Foldable f => f (Doc a) -> Doc a
hsep = PP.hsep . toList

vsepMaybe :: Foldable f => f (Doc a) -> Maybe (Doc a)
vsepMaybe l
  | null l = Nothing
  | otherwise = Just (vsep l)

hsepMaybe :: Foldable f => f (Doc a) -> Maybe (Doc a)
hsepMaybe l
  | null l = Nothing
  | otherwise = Just (hsep l)
