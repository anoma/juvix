module Mari.Library.Trace.Format where

import Control.Lens ((^.))
import qualified Data.Text as Text
import Mari.Library
import qualified Mari.Library.HashMap as HashMap
import qualified Mari.Library.NameSymbol as NameSymbol
import Mari.Library.Trace.Types
import Prelude (String)

--------------------------------------------------------------------------------
-- Main Functionality
--------------------------------------------------------------------------------

currentStackChain :: StackChain -> (Integer -> Integer) -> [String]
currentStackChain chain indentationIncrement =
  chain
    |> stackChainToList
    -- Let's start with the parent
    |> foldr f ([], 0)
    |> fst
  where
    f current (formattedString, indentation) =
      ( formattedString
          <> [ spacing indentation
                 <> functionCall (current ^. name) (current ^. start)
             ],
        indentationIncrement indentation
      )

fullTrace :: T -> (Integer -> Integer) -> [String]
fullTrace t indentationIncrement =
  traceLog t 0 indentationIncrement (t ^. traces)

traceLog ::
  T -> Integer -> (Integer -> Integer) -> [Stack] -> [String]
traceLog t level indentationIncrement stacks =
  (stacks >>= traceStack t level indentationIncrement)

traceStack :: T -> Integer -> (Integer -> Integer) -> Stack -> [String]
traceStack t currentLevel indentationIncrement stack =
  case HashMap.lookup (stack ^. name) (t ^. enabled) of
    Just metaInfo
      | (t ^. debugLevel) >= Just (metaInfo ^. level) ->
        callFull
    Just metaInfo ->
      case metaInfo ^. enable of
        Disabled ->
          callIgnoreCurrent
        Enabled ->
          callFull
        DisableRecursive ->
          []
    Nothing
      | (t ^. debugLevel) >= Just maxTrace ->
        callFull
    Nothing ->
      callIgnoreCurrent
  where
    callFull =
      -- Bad case, make it better as we may end up
      -- ·· (Prelude.add2 12.0)
      -- ·· (Prelude.add2 12.0) ↦ 14.0
      -- if we filter our between list
      let inc = indentationIncrement
          traceBetween = traceLog t (inc currentLevel) inc (stack ^. between)
       in case traceBetween of
            [] ->
              [spacing currentLevel <> returnResult stack]
            _ : _ ->
              [spacing currentLevel <> functionCall (stack ^. name) (stack ^. start)]
                <> traceBetween
                <> [spacing currentLevel <> returnResult stack]
    callIgnoreCurrent =
      traceLog t currentLevel indentationIncrement (stack ^. between)

--------------------------------------------------------------------------------
-- Formatting Helpers
--------------------------------------------------------------------------------

customSpacing :: Char -> Int -> [Char]
customSpacing = flip replicate

spacing :: Integer -> [Char]
spacing 0 = ""
spacing n = customSpacing '·' (fromInteger n) <> " "

functionCall :: NameSymbol.T -> [Text] -> [Char]
functionCall f args =
  "("
    <> unintern (NameSymbol.toSymbol f)
    <> space
    <> foldMap Text.unpack (intersperse " " args)
    <> ")"
  where
    space =
      case args of
        [] -> ""
        _ : _ -> " "

returnResult :: Stack -> [Char]
returnResult stack =
  let res = maybe "No Return" Text.unpack (stack ^. output)
   in functionCall (stack ^. name) (stack ^. start) <> " ↦ " <> res

--------------------------------------------------------------------------------
-- Utility Helpers
--------------------------------------------------------------------------------

stackChainToList :: StackChain -> [Stack]
stackChainToList Empty = []
stackChainToList StackChain {parent = cdr, currentStack = car} =
  car : stackChainToList cdr
