-- Moves al let expressions at the top, just after the top lambdas. This
-- transformation assumes:
-- - There are no LetRecs, Lambdas (other than the ones at the top), nor Match.
-- - Case nodes do not have binders.
-- - Except for the top lambdas, only Let nodes introduce binders.
-- - All let items have type Int.
-- - Top lambdas do not have lets in the binders.
module Juvix.Compiler.Core.Transformation.LetHoisting
  ( module Juvix.Compiler.Core.Transformation.LetHoisting,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Extra.Recursors.Map.Named
import Juvix.Compiler.Core.Extra.Utils
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base

data LItem = LItem
  { _itemLet :: LetItem,
    _itemLevel :: Level,
    _itemName :: Text,
    _itemSymbol :: Symbol
  }

makeLenses ''LItem

newtype LetInfo = LetInfo
  { _letSymbols :: [Symbol]
  }

makeLenses ''LetInfo

-- | `LItem` indexed by Symbol
type LetsTable = HashMap Symbol (Indexed LItem)

mkLetsTable :: [Indexed LItem] -> LetsTable
mkLetsTable l = HashMap.fromList [(i ^. indexedThing . itemSymbol, i) | i <- l]

letHoisting :: InfoTable -> InfoTable
letHoisting = run . mapT' (const letHoist)

letHoist :: forall r. Members '[InfoTableBuilder] r => Node -> Sem r Node
letHoist n = do
  let (topLambdas, body) = unfoldLambdas n
  (l, body') <- runReader @[Symbol] [] (runOutputList @LItem (removeLets body))
  let il = indexFrom 0 l
      tbl = mkLetsTable il
      nlets = length il
      mkLetItem :: Indexed LItem -> LetItem
      mkLetItem i = shiftLetItem (i ^. indexedIx) (i ^. indexedThing . itemLet)
      letItems = map mkLetItem il
      body'' = substPlaceholders tbl (mkLets letItems (shift nlets body'))
  return (reLambdas topLambdas body'')

-- | Removes every Let node and replaces references to it with a unique symbol.
-- NOTE It is assumed that all bound variables are bound by a let.
removeLets :: forall r. Members '[InfoTableBuilder, Output LItem, Reader [Symbol]] r => Node -> Sem r Node
removeLets = go mempty
  where
    go :: BinderList Binder -> Node -> Sem r Node
    go bl = dmapLRM' (bl, f)
    f ::
      BinderList Binder ->
      Node ->
      Sem r Recur
    f bl = \case
      NVar v
        | v ^. varIndex < length bl -> do
            End . mkIdent' . (!! (v ^. varIndex)) <$> ask
        | otherwise -> return (End (NVar (shiftVar (-length bl) v)))
      NLet l -> do
        let _itemLevel = length bl
        _itemSymbol <- freshSymbol
        -- note that the binder does not need to be hoisted because it is
        -- assumed to have type Int
        let bi = l ^. letItem . letItemBinder
        value' <- go bl (l ^. letItem . letItemValue)
        output
          LItem
            { _itemLet = LetItem bi value',
              _itemName = bi ^. binderName,
              _itemSymbol,
              _itemLevel
            }
        r <- local (_itemSymbol :) (go (BL.cons bi bl) (l ^. letBody))
        return (End r)
      other -> return (Recur other)

-- | Replaces the placeholders with variables that point to the hoisted let.
substPlaceholders :: LetsTable -> Node -> Node
substPlaceholders tbl = dmapN go
  where
    go :: Level -> Node -> Node
    go lvl = \case
      NIdt i
        | Just (t :: Indexed LItem) <- HashMap.lookup (i ^. identSymbol) tbl ->
            mkVarN (t ^. indexedThing . itemName) (lvl - t ^. indexedIx - 1)
      m -> m

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
