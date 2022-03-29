module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Base where

import qualified MiniJuvix.Syntax.MicroJuvix.Pretty as M
import Prettyprinter
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Types

data Eann = Highlight
  | MicroAnn M.Ann

ppCode :: M.PrettyCode c => c -> Doc Eann
ppCode = reAnnotate MicroAnn . M.runPrettyCode M.defaultOptions

indent' :: Doc ann -> Doc ann
indent' = indent 2

class PrettyError e where
  ppError :: e -> Doc Eann

instance PrettyError WrongConstructorType where
  ppError e = "Type error during pattern matching."
    <> line <> "The constructor" <+> (ppCode (e ^. wrongCtorTypeName)) <+> "has type:"
    <> line <> indent' (ppCode (e ^. wrongCtorTypeActual))
    <> line <> "but is expected to have type:"
    <> line <> indent' (ppCode (e ^. wrongCtorTypeExpected))
