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
    -- [callOp numWithinLayer @ layerNum]
    StdlibDec -> [nock| [9 342 0 8191] |]
    StdlibAdd -> [nock| [9 20 0 8191] |]
    StdlibSub -> [nock| [9 47 0 8191] |]
    StdlibMul -> [nock| [9 4 0 8191] |]
    StdlibDiv -> [nock| [9 170 0 8191] |]
    StdlibMod -> [nock| [9 46 0 8191] |]
    StdlibLe -> [nock| [9 84 0 8191] |]
    StdlibLt -> [nock| [9 343 0 8191] |]
    -- pow2 is called bex in hoon
    StdlibPow2 -> [nock| [9 4 0 1023] |]
    -- encode is called jam in hoon
    StdlibEncode -> [nock| [9 22 0 511] |]
    -- decode is called cue in hoon
    StdlibDecode -> [nock| [9 94 0 511] |]
    -- verifyDetached is called verify-detatched in hoon
    StdlibVerifyDetached -> [nock| [9 22 0 255] |]
    StdlibSign -> [nock| [9 10 0 255] |]
    StdlibSignDetached -> [nock| [9 23 0 255] |]
    StdlibVerify -> [nock| [9 4 0 255] |]
    StdlibLengthList -> [nock| [9 1.406 0 4095] |]
    StdlibCurry -> [nock| [9 4 0 4095] |]
    -- sha256 is called shax in hoon
    StdlibSha256 -> [nock| [9 22 0 127] |]
    -- Obtained from the urbit dojo using:
    --
    -- =>  rm  !=(~(met block 3))
    --
    -- The `3` here is because we want to treat each atom as sequences of 2^3
    -- bits, i.e bytes.
    StdlibLengthBytes -> [nock| [8 [9 10 0 1023] 9 190 10 [6 7 [0 3] 1 3] 0 2] |]
    -- Obtained from the urbit dojo using:
    --
    -- =>  rm  !=(~(cat block 3))
    --
    -- The `3` here is because we want to treat each atom as sequences of 2^3
    -- bits, i.e bytes.
    StdlibCatBytes -> [nock| [8 [9 10 0 1023] 9 4 10 [6 7 [0 3] 1 3] 0 2] |]
    -- Obtained from the urbit dojo using:
    --
    -- =>(rm !=(|=([l=(list @)] (foldr l |=([fst=@ snd=@] (add (~(lsh block 3) 1 snd) fst))))))
    --
    -- The `3` here is because we want to shift left in byte = 2^3 bit steps.
    StdlibFoldBytes ->
      [nock|
        [
          8
          [1 0]
          [
            1
            8
            [9 46 0 16383]
            9
            2
            10
            [
              6
              [0 14]
              7
              [0 3]
              8
              [1 0 0]
              [
                1
                8
                [9 20 0 131071]
                9
                2
                10
                [
                  6
                  [
                    7
                    [0 3]
                    8
                    [8 [9 10 0 16383] 9 90 10 [6 7 [0 3] 1 3] 0 2]
                    9
                    2
                    10
                    [6 [7 [0 3] 1 1] 0 29]
                    0
                    2
                  ]
                  0
                  28
                ]
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
    -- Obtained from the urbit dojo using:
    --
    -- =>  rm  !=  |=  [seed=@]  ~(. og seed)
    StdlibRandomInitGen -> [nock| [8 [1 0] [1 8 [9 47 0 511] 10 [6 0 14] 0 2] 0 1] |]
    -- obtained from the urbit dojo using:
    --
    -- =>  rm  !=  |=  [rng=* width=@]   (raws:`_og`rng width)
    StdlibRandomNextBits -> [nock| [8 [1 0 0] [1 8 [7 [0 12] 9 4 0 1] 9 2 10 [6 0 29] 0 2] 0 1] |]
    -- obtained from the urbit dojo using:
    --
    -- =>  rm  !=  |=  [rng=*]  split:`_og`rng
    StdlibRandomSplit -> [nock| [8 [1 0] [1 7 [0 6] 9 21 0 1] 0 1] |]
    -- obtained from the urbit dojo using:
    --
    -- =>  rm  !=  |=  a=(set)  ~(tap in a)
    StdlibAnomaSetToList -> [nock| [8 [1 0] [1 8 [9 21 0 63] 9 186 10 [6 0 14] 0 2] 0 1] |]
    -- called silt in hoon
    StdlibAnomaSetFromList -> [nock| [9 22 0 15] |]
  AnomaLibFunction (AnomaRmFunction f) -> case f of
    RmCommit -> [nock| [9 3002 0 1] |]
    RmNullify -> [nock| [9 2815 0 1] |]
    RmKind -> [nock| [9 5972 0 1] |]
    RmDeltaAdd -> [nock| [9 372 0 1] |]
    RmDeltaSub -> [nock| [9 12013 0 1] |]
    RmResourceDelta -> [nock| [9 701 0 1] |]
    RmActionDelta -> [nock| [9 4 0 1] |]
    RmMakeDelta -> [nock| [9 11951 0 1] |]
    RmIsCommitment -> [nock| [9 12012 0 1] |]
    RmIsNullifier -> [nock| [9 5974 0 1] |]
    RmActionCreate -> [nock| [9 382 0 1] |]
    RmTransactionCompose -> [nock| [9 383 0 1] |]
    RmCreateFromComplianceInputs -> [nock| [9 1406 0 1] |]
    RmCairoProveDelta -> [nock| [9 1503 0 1] |]
  AnomaLibValue (AnomaRmValue v) -> case v of
    RmZeroDelta -> [nock| [9 174 0 1] |]
