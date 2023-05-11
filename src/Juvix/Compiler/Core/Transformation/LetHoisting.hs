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
letHoisting = run . mapT' (\_ n -> letHoist n)

letHoist :: forall r. Members '[InfoTableBuilder] r => Node -> Sem r Node
letHoist n = do
  (l, body') <- runOutputList @LItem (removeLets body)
  let il = indexFrom 0 l
      tbl = mkLetsTable il
      mkLetItem :: Indexed LItem -> LetItem
      mkLetItem i = shiftLetItem s (i ^. indexedThing . itemLet)
        where
          s = i ^. indexedIx - i ^. indexedThing . itemLevel
      letItems = map mkLetItem il
      body'' = substPlaceholders tbl (mkLets letItems body')
  return (reLambdas topLambdas body'')
  where
    (topLambdas, body) = unfoldLambdas n

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
        let bi = l ^. letItem . letItemBinder
        binder' <- traverseOf binderType (go recur _itemLevel) bi
        value' <- go recur _itemLevel (l ^. letItem . letItemValue)
        let _itemLet = LetItem binder' value'
        _itemSymbol <- freshSymbol
        output LItem {..}
        recur _itemLevel [BCRemove (BinderRemove bi (mkIdent' _itemSymbol))] (l ^. letBody)
      other -> recur _itemLevel [] other

-- | Replaces the placeholders with variables that point to the hoisted let.
substPlaceholders :: LetsTable -> Node -> Node
substPlaceholders tbl = dmapN go
  where
    go :: Level -> Node -> Node
    go lvl = \case
      NIdt i
        | Just (l :: Indexed LItem) <- HashMap.lookup (i ^. identSymbol) tbl ->
            mkVar' (lvl - l ^. indexedIx)
      n -> n
