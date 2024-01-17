module Juvix.Compiler.Asm.Extra.Apply where

import Data.FileEmbed qualified as FE
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Encoding
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Data.InfoTableBuilder
import Juvix.Compiler.Asm.Translation.FromSource

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
    nextSymbolId = maximum (0 : map (^. symbolId) (HashMap.keys (tab ^. infoFunctions) ++ HashMap.keys (tab ^. infoInductives))) + 1
    nextUserId = maximum (0 : mapMaybe getUserTagId (HashMap.keys (tab ^. infoConstrs))) + 1

    bs :: BuilderState
    bs =
      BuilderState
        { _stateNextSymbolId = nextSymbolId,
          _stateNextUserTag = nextUserId,
          _stateInfoTable = tab,
          _stateIdents = mempty
        }

    bs' :: BuilderState
    bs' =
      fromRight impossible $
        parseText' bs $
          decodeUtf8 $(FE.makeRelativeToProject "runtime/src/asm/apply.jva" >>= FE.embedFile)

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
