module Juvix.Compiler.Backend.Rust.Pretty.Keywords where

import Juvix.Compiler.Backend.Rust.Language
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str

kwFn :: Doc Ann
kwFn = keyword Str.rustFn
