module Commands.Extra.Paths where

import Juvix.Prelude

gebReplPath :: Path Abs File
gebReplPath = $(mkAbsFile "/repl.geb")
