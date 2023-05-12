-- Moves al let expressions at the top, just after the top lambdas. This
-- transformation assumes:
-- - There are no LetRecs, Lambdas (other than the ones at the top), nor Match.
-- - Case nodes do not have binders.
-- - Let items and body do not reference variables bound in Pi nodes.
-- - All let items have type Int.
-- - Top lambdas do not have lets in the binders.
module Juvix.Compiler.Core.Transformation.LetHoisting
  ( module Juvix.Compiler.Core.Transformation.LetHoisting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra.Recursors.Map.Named
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

data LItem = LItem
  { _itemLet :: LetItem,
    _itemLevel :: Level,
    _itemSymbol :: Symbol
  }

makeLenses ''LItem

-- | `LItem` indexed by Symbol
type LetsTable = HashMap Symbol (Indexed LItem)

mkLetsTable :: [Indexed LItem] -> LetsTable
mkLetsTable l = HashMap.fromList [(i ^. indexedThing . itemSymbol, i) | i <- l]

letHoisting :: InfoTable -> InfoTable
letHoisting = run . mapT' (const letHoist)

letHoist :: forall r. Members '[InfoTableBuilder] r => Node -> Sem r Node
letHoist n = do
  (l, n') <- runOutputList @LItem (removeLets n)
  let (topLambdas, body') = unfoldLambdas n'
      nlam = length topLambdas
      il = indexFrom 0 l
      tbl = mkLetsTable il
      mkLetItem :: Indexed LItem -> LetItem
      mkLetItem i = shiftLetItem s (i ^. indexedThing . itemLet)
        where
          s = - i ^. indexedThing . itemLevel + (nlam + i ^. indexedIx)
          -- s = undefined
      letItems = map mkLetItem il
      body'' =
        substPlaceholders nlam tbl
        (mkLets letItems body')
  return (reLambdas topLambdas body'')

-- | Removes every Let node and replaces references to it with a unique symbol.
removeLets :: forall r. Members '[InfoTableBuilder, Output LItem] r => Node -> Sem r Node
removeLets = rmapNM' go
  where
    go ::
      (Level -> [BinderChange] -> Node -> Sem r Node) ->
      Level ->
      Node ->
      Sem r Node
    go recur _itemLevel = \case
      NLet l -> do
        _itemSymbol <- freshSymbol
        -- note that the binder does not need to be hoisted because it is
        -- assumed to have type Int
        let bi = l ^. letItem . letItemBinder
        value' <- go recur _itemLevel (l ^. letItem . letItemValue)
        let _itemLet = LetItem bi value'
        output LItem {..}
        -- [BCRemove (BinderRemove bi (mkIdent' _itemSymbol))]
        go (\lvl ls -> recur lvl (mkBCRemove bi (mkIdent' _itemSymbol) : ls)) _itemLevel (l ^. letBody)
      other -> recur _itemLevel [] other

-- | Replaces the placeholders with variables that point to the hoisted let.
substPlaceholders :: Int -> LetsTable -> Node -> Node
substPlaceholders nlam tbl = dmapN go
  where
    go :: Level -> Node -> Node
    go lvl = \case
      NIdt i
        | Just (l :: Indexed LItem) <- HashMap.lookup (i ^. identSymbol) tbl ->
            mkVar' (lvl - (nlam + l ^. indexedIx))
      n -> n

-- | True if it is of the form λ … λ let a₁ = b₁; … aₙ = bₙ in body;
-- where body does not contain any let.
isLetHoisted :: Node -> Bool
isLetHoisted =
    checkBody
    . snd
    . unfoldLambdas
  where
    checkBody :: Node -> Bool
    checkBody n = isJust . run . runFail $ do
      k <- peelLets n
      noLets k
    peelLets :: Members '[Fail] r => Node -> Sem r Node
    peelLets = \case
      NLet Let {..} -> do
        noLets (_letItem ^. letItemValue)
        peelLets _letBody
      n -> return n
    noLets :: forall r. Members '[Fail] r => Node -> Sem r ()
    noLets = walk go
     where
      go :: Node -> Sem r ()
      go = \case
        NLet {} -> fail
        _ -> return ()
