module Juvix.Compiler.Nockma.StdlibFunction where

import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Prelude hiding (Path)

-- | The stdlib paths are obtained from the Urbit dojo
-- * Load the stdlib file into the Urbit dojo
-- * Run: `=>  anoma  !=(s)` where s is a stdlib symbol
-- eg:
--      =>  anoma  !=(add)
--      [9 20 0 15]
stdlibPath :: StdlibFunction -> Term Natural
stdlibPath = \case
  StdlibDec -> [nock| [9 342 0 15] |]
  StdlibAdd -> [nock| [9 20 0 15] |]
  StdlibSub -> [nock| [9 47 0 15] |]
  StdlibMul -> [nock| [9 4 0 15] |]
  StdlibDiv -> [nock| [9 170 0 15] |]
  StdlibMod -> [nock| [9 46 0 15] |]
  StdlibLe -> [nock| [9 84 0 15] |]
  StdlibLt -> [nock| [9 343 0 15] |]

oldstdlibPath :: StdlibFunction -> Term Natural
oldstdlibPath = \case
  StdlibDec -> [nock| [9 342 0 1] |]
  StdlibAdd -> [nock| [9 20 0 1] |]
  StdlibSub -> [nock| [9 47 0 1] |]
  StdlibMul -> [nock| [9 4 0 1] |]
  StdlibDiv -> [nock| [9 170 0 1] |]
  StdlibMod -> [nock| [9 46 0 1] |]
  StdlibLe -> [nock| [9 84 0 1] |]
  StdlibLt -> [nock| [9 343 0 1] |]

reallyoldstdlibPath :: StdlibFunction -> Path
reallyoldstdlibPath =
  decodePath' . EncodedPath . \case
    StdlibDec -> 342
    StdlibAdd -> 20
    StdlibSub -> 47
    StdlibMul -> 4
    StdlibDiv -> 170
    StdlibMod -> 46
    StdlibLe -> 84
    StdlibLt -> 343
