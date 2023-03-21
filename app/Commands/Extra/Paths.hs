module Commands.Extra.Paths where

import Juvix.Prelude

-- | imaginary file path for error messages in the repl.
replPath :: Path Abs File
replPath = $(mkAbsFile "/<repl>")

gebReplPath :: Path Abs File
gebReplPath = $(mkAbsFile "/repl.geb")
