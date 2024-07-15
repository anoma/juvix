module Juvix.Compiler.Tree.Extra.Apply where

import Data.FileEmbed qualified as FE
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Encoding
import Juvix.Compiler.Tree.Data.InfoTable
import Juvix.Compiler.Tree.Data.InfoTableBuilder
import Juvix.Compiler.Tree.Language
import Juvix.Compiler.Tree.Translation.FromSource

data ApplyBuiltins = ApplyBuiltins
  { -- | The number of `juvix_apply_n` functions.
    _applyBuiltinsNum :: Int,
    -- | Maps `n` to the function `juvix_apply_n`.
    _applyBuiltinsMap :: HashMap Int Symbol
  }

makeLenses ''ApplyBuiltins

addApplyBuiltins :: InfoTable -> (ApplyBuiltins, InfoTable)
addApplyBuiltins tab = (blts, bs' ^. stateInfoTable)
  where
    bs :: BuilderState
    bs = builderStateFromInfoTable tab

    bs' :: BuilderState
    bs' =
      fromRight impossible
        $ parseText' bs
        $ decodeUtf8 $(FE.makeRelativeToProject "runtime/tree/apply.jvt" >>= FE.embedFile)

    blts :: ApplyBuiltins
    blts =
      ApplyBuiltins
        { _applyBuiltinsNum = 4,
          _applyBuiltinsMap =
            HashMap.fromList $ map mkApply [1 .. 4]
        }

    mkApply :: Int -> (Int, Symbol)
    mkApply x = (x, f)
      where
        idt = "juvix_apply_" <> show x
        f = case fromJust $ HashMap.lookup idt (bs' ^. stateIdents) of
          IdentFun s -> s
          _ -> impossible
