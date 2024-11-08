module Juvix.Compiler.Core.Extra.Recursors.Utils where

import Juvix.Compiler.Core.Extra.Recursors.Map.Named

-- shiftVar :: Index -> Var -> Var
-- shiftVar m v =
--   let v' = over varIndex (+ m) v
--    in if
--           | v' ^. varIndex < 0 -> error "shifting a variable resulted in a negative index"
--           | otherwise -> v'

shiftVar' :: Text -> Index -> Var -> Var
shiftVar' txt m v =
  let v' = over varIndex (+ m) v
   in if
          | v' ^. varIndex < 0 ->
              error $
                txt
                  <> ": Shifting a variable resulted in a negative index: "
                  <> prettyVar v
                  <> " "
                  <> show (v' ^. varIndex)
          | otherwise -> v'

-- | increase all free variable indices by a given value
shift :: Text -> Index -> Node -> Node
shift txt = \case
  0 -> id
  m -> umapN go
    where
      go k = \case
        NVar v
          | v ^. varIndex >= k -> NVar (shiftVar' txt m v)
        n -> n
