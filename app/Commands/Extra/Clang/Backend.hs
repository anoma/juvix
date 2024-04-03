module Commands.Extra.Clang.Backend where

import CommonOptions

data ClangBackend
  = ClangNative
  | ClangWasi
  deriving stock (Eq, Bounded, Enum)
