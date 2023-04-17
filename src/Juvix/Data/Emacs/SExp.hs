module Juvix.Data.Emacs.SExp where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Prettyprinter.Render.Text

class ToSExp a where
  toSExp :: a -> SExp

data SExp
  = Symbol Text
  | App [SExp]
  | Pair SExp SExp
  | Quote SExp
  | Backquote SExp
  | Int Word64
  | String Text

progn :: [SExp] -> SExp
progn l = App (Symbol "progn" : l)

renderSExp :: SExp -> Text
renderSExp =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . pretty

instance Pretty SExp where
  pretty = \case
    Symbol s -> pretty s
    Int s -> pretty s
    App l -> parens (sep (map pretty l))
    Pair l r -> parens (pretty l <+> dot <+> pretty r)
    Backquote l -> pretty '`' <> pretty l
    Quote l -> pretty '\'' <> pretty l
    String s -> dquotes (pretty s)
