module Juvix.Compiler.Nockma.StdlibFunction where

import Juvix.Compiler.Nockma.Translation.FromSource.QQ
import Juvix.Prelude.Base

-- | The stdlib paths are obtained from the Urbit dojo
-- * Load the stdlib file into the Urbit dojo
-- * Run: `=>  anoma  !=(s)` where s is a stdlib symbol
-- eg:
--      =>  anoma  !=(add)
--      [9 20 0 15]
stdlibPath :: StdlibFunction -> Term Natural
stdlibPath = \case
  StdlibDec -> [nock| [9 342 0 63] |]
  StdlibAdd -> [nock| [9 20 0 63] |]
  StdlibSub -> [nock| [9 47 0 63] |]
  StdlibMul -> [nock| [9 4 0 63] |]
  StdlibDiv -> [nock| [9 170 0 63] |]
  StdlibMod -> [nock| [9 46 0 63] |]
  StdlibLe -> [nock| [9 84 0 63] |]
  StdlibLt -> [nock| [9 343 0 63] |]
  StdlibPow2 -> [nock| [9 4 0 7] |]
  StdlibEncode -> [nock| [9 22 0 3] |]
  StdlibDecode -> [nock| [9 94 0 3] |]
