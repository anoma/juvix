module Juvix.Emacs.SExp where

import Juvix.Prelude
import Juvix.Prelude.Pretty

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

nil :: SExp
nil = Symbol "nil"

progn :: [SExp] -> SExp
progn l = App (Symbol "progn" : l)

putHash :: Text -> SExp -> SExp -> SExp
putHash tblName key val = App [(Symbol "puthash"), key, val, (Symbol tblName)]

makeHashTable :: SExp
makeHashTable = App [Symbol "make-hash-table"]

setq :: Text -> SExp -> SExp
setq varName val = App [Symbol "setq", Symbol varName, val]

mkHashTable :: Text -> [(SExp, SExp)] -> SExp
mkHashTable tblName items =
  progn
    $ setq tblName makeHashTable
    : [putHash tblName key val | (key, val) <- items]

withLocalHashTable :: Text -> [(SExp, SExp)] -> SExp -> SExp
withLocalHashTable tblName items body =
  let1
    (tblName, makeHashTable)
    . progn
    $ [putHash tblName key val | (key, val) <- items]
    ++ [body]

let_ :: [(Text, SExp)] -> SExp -> SExp
let_ items body =
  App
    [ Symbol "let",
      App [App [(Symbol x), xdef] | (x, xdef) <- items],
      body
    ]

let1 :: (Text, SExp) -> SExp -> SExp
let1 (x, xdef) body = let_ [(x, xdef)] body

mkList :: [SExp] -> SExp
mkList = Quote . App

instance HasTextBackend SExp where
  toTextDoc = pretty

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
