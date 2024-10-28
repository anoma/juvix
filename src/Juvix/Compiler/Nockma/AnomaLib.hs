module Juvix.Compiler.Nockma.AnomaLib where

import Data.FileEmbed qualified as FE
import Juvix.Compiler.Nockma.Translation.FromSource
import Juvix.Prelude.Base

anomaLib :: Term Natural
anomaLib =
  fromRight impossible $
    parseText $
      decodeUtf8 $(FE.makeRelativeToProject "runtime/nockma/anomalib.nockma" >>= FE.embedFile)

-- | The anoma lib paths are obtained from the Urbit dojo
-- * Load the anoma lib files into the Urbit dojo
--
--      =anoma -build-file /=base=/lib/anoma/hoon
--      =rm -build-file /=base=/lib/resource-machine/hoon
--
-- * Run: `=>  rm  !=(s)` where s is an anoma lib symbol
-- eg:
--      =>  rm  !=(add)
--      [9 20 0 15]
anomaLibPath :: AnomaLib -> Term Natural
anomaLibPath = \case
  AnomaLibFunction (AnomaStdlibFunction f) -> case f of
    StdlibDec -> [nock| [9 342 0 511] |]
    StdlibAdd -> [nock| [9 20 0 511] |]
    StdlibSub -> [nock| [9 47 0 511] |]
    StdlibMul -> [nock| [9 4 0 511] |]
    StdlibDiv -> [nock| [9 170 0 511] |]
    StdlibMod -> [nock| [9 46 0 511] |]
    StdlibLe -> [nock| [9 84 0 511] |]
    StdlibLt -> [nock| [9 343 0 511] |]
    -- pow2 is called bex in hoon
    StdlibPow2 -> [nock| [9 4 0 63] |]
    -- encode is called jam in hoon
    StdlibEncode -> [nock| [9 22 0 31] |]
    -- decode is called cue in hoon
    StdlibDecode -> [nock| [9 94 0 31] |]
    -- verifyDetached is called verify-detatched in hoon
    StdlibVerifyDetached -> [nock| [9 22 0 15] |]
    StdlibSign -> [nock| [9 10 0 15] |]
    StdlibSignDetached -> [nock| [9 23 0 15] |]
    StdlibVerify -> [nock| [9 4 0 15] |]
    StdlibLengthList -> [nock| [9 1.406 0 255] |]
    StdlibCurry -> [nock| [9 4 0 255] |]
    -- sha256 is called shax in hoon
    StdlibSha256 -> [nock| [9 22 0 7] |]
    -- Obtained from the urbit dojo using:
    --
    -- =>  rm  !=(~(met block 3))
    --
    -- The `3` here is because we want to treat each atom as sequences of 2^3
    -- bits, i.e bytes.
    StdlibLengthBytes -> [nock| [8 [9 10 0 63] 9 190 10 [6 7 [0 3] 1 3] 0 2] |]
    -- Obtained from the urbit dojo using:
    --
    -- =>  rm  !=(~(cat block 3))
    --
    -- The `3` here is because we want to treat each atom as sequences of 2^3
    -- bits, i.e bytes.
    StdlibCatBytes -> [nock| [8 [9 10 0 63] 9 4 10 [6 7 [0 3] 1 3] 0 2] |]
    -- Obtained from the urbit dojo using:
    --
    -- =>(rm !=(|=([l=(list @)] (foldr l |=([fst=@ snd=@] (add (~(lsh block 3) 1 snd) fst))))))
    --
    -- The `3` here is because we want to shift left in byte = 2^3 bit steps.
    StdlibFoldBytes ->
      [nock|
              [ 8
                [1 0]
                [ 1
                  8
                  [9 46 0 1.023]
                  9
                  2
                  10
                  [ 6
                    [0 14]
                    7
                    [0 3]
                    8
                    [1 0 0]
                    [1 8 [9 20 0 8.191] 9 2 10 [6 [7 [0 3] 8 [8 [9 10 0 1.023] 9 90 10 [6 7 [0 3] 1 3] 0 2] 9 2 10 [6 [7 [0 3] 1 1] 0 29] 0 2] 0 28] 0 2]
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
    -- Obtained from the urbit dojo using:
    --
    -- =>  rm  !=  |=  [seed=@]  ~(. og seed)
    StdlibRandomInitGen -> [nock| [8 [1 0] [1 8 [9 47 0 31] 10 [6 0 14] 0 2] 0 1] |]
    -- obtained from the urbit dojo using:
    --
    -- =>  rm  !=  |=  [rng=* width=@]   (raws:`_og`rng width)
    StdlibRandomNextBytes -> [nock| [8 [1 0 0] [1 8 [7 [0 12] 9 4 0 1] 9 2 10 [6 0 29] 0 2] 0 1] |]
    -- obtained from the urbit dojo using:
    --
    -- =>  rm  !=  |=  [rng=*]  split:`_og`rng
    StdlibRandomSplit -> [nock| [8 [1 0] [1 7 [0 6] 9 21 0 1] 0 1] |]
  AnomaLibFunction (AnomaRmFunction f) -> case f of
    RmCommit -> [nock| [9 94 0 1] |]
    RmNullify -> [nock| [9 350 0 1] |]
    RmKind -> [nock| [9 1.492 0 1] |]
    RmProveLogic -> [nock| [9 342 0 1] |]
    RmProveAction -> [nock| [9 22 0 1] |]
    RmDeltaAdd -> [nock| [9 92 0 1] |]
    RmDeltaSub -> [nock| [9 763 0 1] |]
    RmResourceDelta -> [nock| [9 343 0 1] |]
    RmActionDelta -> [nock| [9 4 0 1] |]
    RmMakeDelta -> [nock| [9 372 0 1] |]
    RmProveDelta -> [nock| [9 1.535 0 1] |]
  AnomaLibValue (AnomaRmValue v) -> case v of
    RmZeroDelta -> [nock| [9 20 0 1] |]
