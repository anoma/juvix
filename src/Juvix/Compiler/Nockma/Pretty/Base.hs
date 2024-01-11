module Juvix.Compiler.Nockma.Pretty.Base
  ( module Juvix.Compiler.Nockma.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Nockma.Pretty.Options,
  )
where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Prelude hiding (Atom, Path)

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

runPrettyCode :: (PrettyCode c) => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

instance (PrettyCode a, NockNatural a) => PrettyCode (Atom a) where
  ppCode atm@(Atom k h) = runFailDefaultM (annotate (AnnKind KNameFunction) <$> ppCode k)
    . failFromError @(ErrNockNatural a)
    $ do
      h' <- failMaybe (h ^. unIrrelevant)
      case h' of
        AtomHintOp -> nockOp atm >>= ppCode
        AtomHintPath -> nockPath atm >>= ppCode
        AtomHintBool
          | atm == nockTrue -> return (annotate (AnnKind KNameInductive) "true")
          | atm == nockFalse -> return (annotate (AnnKind KNameAxiom) "false")
          | otherwise -> fail
        AtomHintNil -> return (annotate (AnnKind KNameConstructor) "nil")

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

instance (PrettyCode a, NockNatural a) => PrettyCode (Cell a) where
  ppCode c = do
    m <- asks (^. optPrettyMode)
    inside <- case m of
      AllDelimiters -> do
        l' <- ppCode (c ^. cellLeft)
        r' <- ppCode (c ^. cellRight)
        return (l' <+> r')
      MinimizeDelimiters -> sep <$> mapM ppCode (unfoldCell c)
    return (oneLineOrNextBrackets inside)

unfoldCell :: Cell a -> NonEmpty (Term a)
unfoldCell c = c ^. cellLeft :| go [] (c ^. cellRight)
  where
    go :: [Term a] -> Term a -> [Term a]
    go acc = \case
      t@TermAtom {} -> reverse (t : acc)
      TermCell (Cell l r) -> go (l : acc) r

instance (PrettyCode a, NockNatural a) => PrettyCode (Term a) where
  ppCode = \case
    TermAtom t -> ppCode t
    TermCell c -> ppCode c
