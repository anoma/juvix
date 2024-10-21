{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Compiler.Nockma.Pretty.Base
  ( module Juvix.Compiler.Nockma.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Nockma.Pretty.Options,
  )
where

import Juvix.Compiler.Nockma.Encoding.ByteString (atomToText)
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude hiding (Atom, Path)

docDefault :: (PrettyCode c) => c -> Doc Ann
docDefault = doc defaultOptions

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

runPrettyCode :: (PrettyCode c) => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

instance PrettyCodeAnn NockNaturalNaturalError where
  ppCodeAnn = \case
    NaturalInvalidPath a -> "Invalid path" <+> docDefault a
    NaturalInvalidOp a -> "Invalid operator code" <+> docDefault a

instance forall a. (PrettyCode a, NockNatural a) => PrettyCode (Atom a) where
  ppCode atm = do
    t <- runFail $ do
      failWhenM (asks (^. optIgnoreTags))
      failMaybe (atm ^. atomTag) >>= ppCode
    let def = fmap (t <?+>) (annotate (AnnKind KNameFunction) <$> ppCode (atm ^. atom))
    fmap (t <?+>) . runFailDefaultM def . failFromError @(ErrNockNatural a) $
      do
        whenM (asks (^. optIgnoreHints)) fail
        h' <- failMaybe (atm ^. atomHint)
        case h' of
          AtomHintOp -> nockOp atm >>= ppCode
          AtomHintPath -> nockPath atm >>= ppCode
          AtomHintBool
            | nockmaEq atm nockTrue -> return (annotate (AnnKind KNameInductive) Str.true_)
            | nockmaEq atm nockFalse -> return (annotate (AnnKind KNameAxiom) Str.false_)
            | otherwise -> fail
          AtomHintNil -> return (annotate (AnnKind KNameConstructor) Str.nil)
          AtomHintVoid -> return (annotate (AnnKind KNameAxiom) Str.void)
          AtomHintFunctionsPlaceholder -> return (annotate (AnnKind KNameAxiom) Str.functionsPlaceholder)
          AtomHintStdlibPlaceholder -> return (annotate (AnnKind KNameAxiom) Str.stdlibPlaceholder)
          AtomHintString -> atomToText atm >>= ppCode

instance PrettyCode Text where
  ppCode = return . dquotes . pretty

instance PrettyCode Interval where
  ppCode = return . pretty

instance PrettyCode Natural where
  ppCode = return . pretty

instance PrettyCode Path where
  ppCode = \case
    [] -> return "S"
    ds -> mconcatMapM ppCode ds

instance PrettyCode Direction where
  ppCode =
    return . \case
      L -> annotate (AnnKind KNameAxiom) "L"
      R -> annotate AnnKeyword "R"

instance PrettyCode NockOp where
  ppCode =
    return . annotate (AnnKind KNameFunction) . pretty

instance PrettyCode StdlibFunction where
  ppCode = return . pretty

instance (PrettyCode a, NockNatural a) => PrettyCode (StdlibCall a) where
  ppCode c = do
    fun <- ppCode (c ^. stdlibCallFunction)
    args <- ppCode (c ^. stdlibCallArgs)
    return (Str.stdlibTag <> fun <+> Str.argsTag <> args)

instance PrettyCode Tag where
  ppCode (Tag txt) = return (annotate AnnKeyword Str.tagTag <> pretty txt)

instance (PrettyCode a, NockNatural a) => PrettyCode (Cell a) where
  ppCode c = do
    m <- asks (^. optPrettyMode)
    label <- runFail $ do
      failWhenM (asks (^. optIgnoreTags))
      failMaybe (c ^. cellTag) >>= ppCode
    stdlibCall <- runFail $ do
      failWhenM (asks (^. optIgnoreHints))
      failMaybe (c ^. cellCall) >>= ppCode
    components <- case m of
      AllDelimiters -> do
        l' <- ppCode (c ^. cellLeft)
        r' <- ppCode (c ^. cellRight)
        return (l' <+> r')
      MinimizeDelimiters -> sep <$> mapM ppCode (unfoldCell c)
    let inside = label <?+> stdlibCall <?+> components
    return (oneLineOrNextBrackets inside)

unfoldCell :: Cell a -> NonEmpty (Term a)
unfoldCell c = c ^. cellLeft :| reverse (go [] (c ^. cellRight))
  where
    go :: [Term a] -> Term a -> [Term a]
    go acc t = case t of
      TermAtom {} -> t : acc
      TermCell (Cell' l r i)
        | isNothing (i ^. cellInfoCall) && isNothing (i ^. cellInfoTag) -> go (l : acc) r
        | otherwise -> t : acc

instance (PrettyCode a, NockNatural a) => PrettyCode (Term a) where
  ppCode = \case
    TermAtom t -> ppCode t
    TermCell c -> ppCode c

instance (PrettyCode a, NockNatural a) => PrettyCode [Term a] where
  ppCode ts = do
    ts' <- mapM ppCode ts
    return (braces (commaSep ts'))
