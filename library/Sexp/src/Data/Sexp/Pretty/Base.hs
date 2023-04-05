-- | This module implements a pretty printer for S-expressions using the
--   prettyprinter library.
--
-- __Examples:__
--
-- @
-- import qualified Data.Sexp as Sexp
-- import qualified Data.Sexp.Pretty.Base as Sexp
-- import Prettyprinter
-- import Prettyprinter.Util (putDocW)
--
-- mkAtom n = Sexp.Atom $ Sexp.N n Nothing
-- mkCons = foldr Sexp.Cons
-- mkList = mkCons Sexp.Nil
--
-- -- A basic list
-- ppSexp $ mkList [mkAtom 1, mkAtom 2, mkAtom 3]
-- -- > (1 2 3)
--
-- -- A basic cons
-- ppSexp $ mkCons (mkAtom 3) [mkAtom 1, mkAtom 2]
-- -- > (1 2 . 3)
--
-- -- Nested list
-- ppSexp $ mkList [mkAtom 1, mkList [mkAtom 2, mkAtom 3], mkAtom 4]
-- -- > (1 (2 3) 4)
--
-- -- An example showing aligning and wrapping (using an artificial width)
-- putDocW 5 $ ppSexp $ mkList [mkAtom n | n <- [0..4]]
-- -- >
-- -- (0 1
-- --    2
-- --    3
-- --    4
-- @
module Data.Sexp.Pretty.Base
  ( ppSexp,
    ppSexpBase,
    ppAtomBase,
  )
where

import Mari.Library
import qualified Mari.Library.NameSymbol as NameSymbol
import qualified Data.Sexp as Sexp
import Prettyprinter

--------------------------------------------------------------------------------
-- Pretty functions
--------------------------------------------------------------------------------

ppSexpBase :: Pretty a => Sexp.Base a -> Doc ann
ppSexpBase (Sexp.Atom at) = ppAtomBase at
ppSexpBase (Sexp.Cons car cdr) = parens $ align $ ppCons car cdr
ppSexpBase Sexp.Nil = parens emptyDoc

ppAtomBase :: Pretty a => Sexp.Atom a -> Doc ann
ppAtomBase (Sexp.A n _) = name n
ppAtomBase (Sexp.N i _) = number i
ppAtomBase (Sexp.D d _) = number d
ppAtomBase (Sexp.S t _) = text t
ppAtomBase (Sexp.P x _) = "P#" <> parens (pretty x)

ppSexp :: Sexp.T -> Doc ann
ppSexp = ppSexpBase

--------------------------------------------------------------------------------
-- Pretty instances
--------------------------------------------------------------------------------

instance (Pretty a) => Pretty (Sexp.Base a) where
  pretty = ppSexpBase

instance (Pretty a) => Pretty (Sexp.Atom a) where
  pretty = ppAtomBase

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

name :: NameSymbol.T -> Doc ann
name = pretty . NameSymbol.toText

number :: Pretty a => a -> Doc ann
number = pretty

text :: Pretty a => a -> Doc ann
text = dquotes . pretty

ppCons :: Pretty a => Sexp.Base a -> Sexp.Base a -> Doc ann
ppCons car cdr = ppSexpBase car <+> Prettyprinter.group (nest 2 (consCdr cdr))
  where
    consCdr Sexp.Nil = emptyDoc
    consCdr a@(Sexp.Atom _) = dot <+> ppSexpBase a
    consCdr (Sexp.Cons cadr Sexp.Nil) = ppSexpBase cadr
    consCdr (Sexp.Cons cadr cddr) = ppSexpBase cadr <> line <> consCdr cddr
