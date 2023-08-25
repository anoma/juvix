module Juvix.Compiler.Concrete.Data.Highlight.RenderEmacs where

import Data.Text qualified as Text
import Juvix.Compiler.Concrete.Data.Highlight.Properties
import Juvix.Compiler.Concrete.Data.ScopedName
import Juvix.Data.CodeAnn
import Juvix.Data.Emacs
import Juvix.Prelude

nameKindFace :: NameKind -> Maybe Face
nameKindFace = \case
  KNameConstructor -> Just FaceConstructor
  KNameInductive -> Just FaceInductive
  KNameFunction -> Just FaceFunction
  KNameTopModule -> Just FaceModule
  KNameLocalModule -> Just FaceModule
  KNameAxiom -> Just FaceAxiom
  KNameLocal -> Nothing
  KNameAlias -> Nothing
  KNameFixity -> Nothing

fromCodeAnn :: CodeAnn -> Maybe EmacsProperty
fromCodeAnn = \case
  AnnKind k -> do
    f <- nameKindFace k
    return (EPropertyFace (PropertyFace f))
  AnnKeyword -> Just (EPropertyFace (PropertyFace FaceKeyword))
  AnnDelimiter -> Just (EPropertyFace (PropertyFace FaceDelimiter))
  AnnComment -> Just (EPropertyFace (PropertyFace FaceComment))
  AnnJudoc -> Just (EPropertyFace (PropertyFace FaceJudoc))
  AnnLiteralString -> Just (EPropertyFace (PropertyFace FaceString))
  AnnLiteralInteger -> Just (EPropertyFace (PropertyFace FaceNumber))
  AnnCode -> Nothing
  AnnImportant -> Nothing
  AnnUnkindedSym -> Nothing
  AnnDef {} -> Nothing
  -- TODO goto property
  AnnRef {} -> Nothing

data RenderState = RenderState
  { _statePoint :: Point,
    _stateText :: Text,
    _stateStack :: [(Point, EmacsProperty)],
    _stateProperties :: [WithRange EmacsProperty]
  }

makeLenses ''RenderState

renderEmacs :: SimpleDocStream CodeAnn -> (Text, SExp)
renderEmacs s =
  let r = run . execState iniRenderState . go . alterAnnotationsS fromCodeAnn $ s
   in (r ^. stateText, progn (map putProperty (r ^. stateProperties)))
  where
    iniRenderState =
      RenderState
        { _statePoint = minBound,
          _stateStack = [],
          _stateText = mempty,
          _stateProperties = []
        }
    go :: (Members '[State RenderState] r) => SimpleDocStream EmacsProperty -> Sem r ()
    go = \case
      SFail -> error "when is this supposed to happen?"
      SEmpty -> do
        st <- gets (^. stateStack)
        case st of
          [] -> return ()
          _ -> error "non-empty stack at the end. Is this possible?"
        return ()
      SChar ch rest -> do
        modify (over stateText (<> Text.singleton ch))
        modify (over statePoint succ)
        go rest
      SLine len rest -> do
        let b = "\n" <> Text.replicate len " "
        modify (over stateText (<> b))
        modify (over statePoint (pointSuccN (fromIntegral (succ len))))
        go rest
      SText len txt rest -> do
        modify (over stateText (<> txt))
        modify (over statePoint (pointSuccN (fromIntegral len)))
        go rest
      SAnnPush an rest -> do
        pt <- gets (^. statePoint)
        modify (over stateStack ((pt, an) :))
        go rest
      SAnnPop rest -> do
        _pintervalEnd <- gets (^. statePoint)
        (_pintervalStart, an) :| st <- nonEmpty' <$> gets (^. stateStack)
        modify (set stateStack st)
        let p =
              WithRange
                { _withRange = PointInterval {..},
                  _withRangeParam = an
                }
        modify (over stateProperties (p :))
        go rest
