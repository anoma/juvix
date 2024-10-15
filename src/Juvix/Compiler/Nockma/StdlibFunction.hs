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
  StdlibDec -> [nock| [9 342 0 255] |]
  StdlibAdd -> [nock| [9 20 0 255] |]
  StdlibSub -> [nock| [9 47 0 255] |]
  StdlibMul -> [nock| [9 4 0 255] |]
  StdlibDiv -> [nock| [9 170 0 255] |]
  StdlibMod -> [nock| [9 46 0 255] |]
  StdlibLe -> [nock| [9 84 0 255] |]
  StdlibLt -> [nock| [9 343 0 255] |]
  -- pow2 is called bex in hoon
  StdlibPow2 -> [nock| [9 4 0 31] |]
  -- encode is called jam in hoon
  StdlibEncode -> [nock| [9 22 0 15] |]
  -- decode is called cue in hoon
  StdlibDecode -> [nock| [9 94 0 15] |]
  -- verifyDetached is called verify-detatched in hoon
  StdlibVerifyDetached -> [nock| [9 22 0 7] |]
  StdlibSign -> [nock| [9 10 0 7] |]
  StdlibSignDetached -> [nock| [9 23 0 7] |]
  StdlibVerify -> [nock| [9 4 0 7] |]
  StdlibLengthList -> [nock| [9 1.406 0 127] |]
  StdlibCurry -> [nock| [9 4 0 127] |]
  -- Obtained from the urbit dojo using:
  --
  -- =>  anoma  !=(~(met block 3))
  --
  -- The `3` here is because we want to treat each atom as sequences of 2^3
  -- bits, i.e bytes.
  StdlibLengthBytes -> [nock| [8 [9 10 0 31] 9 190 10 [6 7 [0 3] 1 3] 0 2] |]
  -- Obtained from the urbit dojo using:
  --
  -- =>  anoma  !=(~(cat block 3))
  --
  -- The `3` here is because we want to treat each atom as sequences of 2^3
  -- bits, i.e bytes.
  StdlibCatBytes -> [nock| [8 [9 10 0 31] 9 4 10 [6 7 [0 3] 1 3] 0 2] |]
  -- Obtained from the urbit dojo using:
  --
  -- =>(anoma !=(|=([l=(list @)] (foldr l |=([fst=@ snd=@] (add (~(lsh block 3) 1 snd) fst))))))
  --
  -- The `3` here is because we want to shift left in byte = 2^3 bit steps.
  StdlibFoldBytes ->
    [nock|
            [ 8
              [1 0]
              [ 1
                8
                [9 46 0 511]
                9
                2
                10
                [ 6
                  [0 14]
                  7
                  [0 3]
                  8
                  [1 0 0]
                  [ 1
                    8
                    [9 20 0 4.095]
                    9
                    2
                    10
                    [6 [7 [0 3] 8 [8 [9 10 0 511] 9 90 10 [6 7 [0 3] 1 3] 0 2] 9 2 10 [6 [7 [0 3] 1 1] 0 29] 0 2] 0 28]
                    0
                    2
                  ]
                  0
                  1
                ]
                0
                2
              ]
              0
              1
            ]
     |]
