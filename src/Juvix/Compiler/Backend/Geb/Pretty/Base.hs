module Juvix.Compiler.Backend.Geb.Pretty.Base
  ( module Juvix.Compiler.Backend.Geb.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Backend.Geb.Pretty.Options,
  )
where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty.Options
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

doc :: (HasAtomicity c, PrettyCode c) => Options -> c -> Doc Ann
doc opts x =
  run $
    runReader opts $
      case atomicity x of
        Atom -> ppCode x
        Aggregate _ -> parens <$> ppCode x

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

instance PrettyCode Case where
  ppCode Case {..} = do
    lty <- ppArg _caseLeftType
    rty <- ppArg _caseRightType
    cod <- ppArg _caseCodomainType
    val <- ppArg _caseOn
    left <- ppArg _caseLeft
    right <- ppArg _caseRight
    return $ kwCase <+> lty <+> rty <+> cod <+> val <+> left <+> right

instance PrettyCode Pair where
  ppCode Pair {..} = do
    lty <- ppArg _pairLeftType
    rty <- ppArg _pairRightType
    left <- ppArg _pairLeft
    right <- ppArg _pairRight
    return $ kwPair <+> lty <+> rty <+> left <+> right

instance PrettyCode Fst where
  ppCode Fst {..} = do
    lty <- ppArg _fstLeftType
    rty <- ppArg _fstRightType
    val <- ppArg _fstValue
    return $ kwFst <+> lty <+> rty <+> val

instance PrettyCode Snd where
  ppCode Snd {..} = do
    lty <- ppArg _sndLeftType
    rty <- ppArg _sndRightType
    val <- ppArg _sndValue
    return $ kwSnd <+> lty <+> rty <+> val

instance PrettyCode Lamb where
  ppCode Lamb {..} = do
    vty <- ppArg _lambVarType
    bty <- ppArg _lambBodyType
    body <- ppArg _lambBody
    return $ kwLamb <+> vty <+> bty <+> body

instance PrettyCode App where
  ppCode App {..} = do
    dom <- ppArg _appDomainType
    cod <- ppArg _appCodomainType
    left <- ppArg _appLeft
    right <- ppArg _appRight
    return $ kwApp <+> dom <+> cod <+> left <+> right

instance PrettyCode Geb where
  ppCode = \case
    GebAbsurd val -> do
      v <- ppArg val
      return $ kwAbsurd <+> v
    GebUnit ->
      return kwUnit
    GebLeft val -> do
      v <- ppArg val
      return $ kwLeft <+> v
    GebRight val -> do
      v <- ppArg val
      return $ kwRight <+> v
    GebCase x -> ppCode x
    GebPair x -> ppCode x
    GebFst x -> ppCode x
    GebSnd x -> ppCode x
    GebLamb x -> ppCode x
    GebApp x -> ppCode x
    GebVar idx -> return $ kwVar <+> annotate AnnLiteralInteger (pretty idx)

instance PrettyCode Prod where
  ppCode Prod {..} = do
    left <- ppArg _prodLeft
    right <- ppArg _prodRight
    return $ kwProd <+> left <+> right

instance PrettyCode Coprod where
  ppCode Coprod {..} = do
    left <- ppArg _coprodLeft
    right <- ppArg _coprodRight
    return $ kwCoprod <+> left <+> right

instance PrettyCode Hom where
  ppCode Hom {..} = do
    dom <- ppArg _homDomain
    cod <- ppArg _homCodomain
    return $ kwHom <+> dom <+> cod

instance PrettyCode Object where
  ppCode = \case
    ObjectInitial -> return kwInitial
    ObjectTerminal -> return kwTerminal
    ObjectProd x -> ppCode x
    ObjectCoprod x -> ppCode x
    ObjectHom x -> ppCode x

{--------------------------------------------------------------------------------}
{- helper functions -}

ppArg ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  a ->
  Sem r (Doc Ann)
ppArg = ppRightExpression appFixity

ppRightExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppRightExpression = ppLRExpression isRightAssoc

ppLeftExpression ::
  (PrettyCode a, HasAtomicity a, Member (Reader Options) r) =>
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLeftExpression = ppLRExpression isLeftAssoc

ppLRExpression ::
  (HasAtomicity a, PrettyCode a, Member (Reader Options) r) =>
  (Fixity -> Bool) ->
  Fixity ->
  a ->
  Sem r (Doc Ann)
ppLRExpression associates fixlr e =
  parensIf (atomParens associates (atomicity e) fixlr)
    <$> ppCode e

{--------------------------------------------------------------------------------}
{- keywords -}

kwAbsurd :: Doc Ann
kwAbsurd = keyword Str.gebAbsurd

kwUnit :: Doc Ann
kwUnit = keyword Str.gebUnit

kwLeft :: Doc Ann
kwLeft = keyword Str.gebLeft

kwRight :: Doc Ann
kwRight = keyword Str.gebRight

kwFst :: Doc Ann
kwFst = keyword Str.gebFst

kwSnd :: Doc Ann
kwSnd = keyword Str.gebSnd

kwCase :: Doc Ann
kwCase = keyword Str.gebCase

kwPair :: Doc Ann
kwPair = keyword Str.gebPair

kwLamb :: Doc Ann
kwLamb = keyword Str.gebLamb

kwApp :: Doc Ann
kwApp = keyword Str.gebApp

kwVar :: Doc Ann
kwVar = keyword Str.gebVar

kwInitial :: Doc Ann
kwInitial = keyword Str.gebInitial

kwTerminal :: Doc Ann
kwTerminal = keyword Str.gebTerminal

kwProd :: Doc Ann
kwProd = keyword Str.gebProd

kwCoprod :: Doc Ann
kwCoprod = keyword Str.gebCoprod

kwHom :: Doc Ann
kwHom = keyword Str.gebHom
