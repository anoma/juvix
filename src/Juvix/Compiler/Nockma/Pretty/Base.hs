module Juvix.Compiler.Nockma.Pretty.Base
  ( module Juvix.Compiler.Nockma.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Nockma.Pretty.Options,
  )
where

import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Prelude hiding (Atom)

doc :: (PrettyCode c) => Options -> c -> Doc Ann
doc opts =
  run
    . runReader opts
    . ppCode

class PrettyCode c where
  ppCode :: (Member (Reader Options) r) => c -> Sem r (Doc Ann)

runPrettyCode :: (PrettyCode c) => Options -> c -> Doc Ann
runPrettyCode opts = run . runReader opts . ppCode

instance (PrettyCode a) => PrettyCode (Atom a) where
  ppCode (Atom k) = annotate (AnnKind KNameFunction) <$> ppCode k

instance PrettyCode Natural where
  ppCode = return . pretty

instance (PrettyCode a) => PrettyCode (Cell a) where
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

instance (PrettyCode a) => PrettyCode (Term a) where
  ppCode = \case
    TermAtom t -> ppCode t
    TermCell c -> ppCode c
