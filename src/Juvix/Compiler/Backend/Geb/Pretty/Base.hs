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

instance PrettyCode First where
  ppCode First {..} = do
    lty <- ppArg _firstLeftType
    rty <- ppArg _firstRightType
    val <- ppArg _firstValue
    return $ kwFst <+> lty <+> rty <+> val

instance PrettyCode Second where
  ppCode Second {..} = do
    lty <- ppArg _secondLeftType
    rty <- ppArg _secondRightType
    val <- ppArg _secondValue
    return $ kwSnd <+> lty <+> rty <+> val

instance PrettyCode Lambda where
  ppCode Lambda {..} = do
    vty <- ppArg _lambdaVarType
    bty <- ppArg _lambdaBodyType
    body <- ppArg _lambdaBody
    return $ kwLamb <+> vty <+> bty <+> body

instance PrettyCode Application where
  ppCode Application {..} = do
    dom <- ppArg _applicationDomainType
    cod <- ppArg _applicationCodomainType
    left <- ppArg _applicationLeft
    right <- ppArg _applicationRight
    return $ kwApp <+> dom <+> cod <+> left <+> right

instance PrettyCode Var where
  ppCode Var {..} = return $ annotate AnnLiteralInteger (pretty _varIndex)

instance PrettyCode Opcode where
  ppCode = \case
    OpAdd -> return kwAdd
    OpSub -> return kwSub
    OpMul -> return kwMul
    OpDiv -> return kwDiv
    OpMod -> return kwMod
    OpEq -> return kwEq
    OpLt -> return kwLt

instance PrettyCode Binop where
  ppCode Binop {..} = do
    op <- ppCode _binopOpcode
    left <- ppArg _binopLeft
    right <- ppArg _binopRight
    return $ op <+> left <+> right

instance PrettyCode Morphism where
  ppCode = \case
    MorphismAbsurd val -> do
      v <- ppArg val
      return $ kwAbsurd <+> v
    MorphismUnit ->
      return kwUnit
    MorphismLeft val -> do
      v <- ppArg val
      return $ kwLeft <+> v
    MorphismRight val -> do
      v <- ppArg val
      return $ kwRight <+> v
    MorphismCase x -> ppCode x
    MorphismPair x -> ppCode x
    MorphismFirst x -> ppCode x
    MorphismSecond x -> ppCode x
    MorphismLambda x -> ppCode x
    MorphismApplication x -> ppCode x
    MorphismVar idx -> do
      i <- ppCode idx
      return $ kwVar <+> i
    MorphismInteger n -> return $ annotate AnnLiteralInteger (pretty n)
    MorphismBinop x -> ppCode x

instance PrettyCode Product where
  ppCode Product {..} = do
    left <- ppArg _productLeft
    right <- ppArg _productRight
    return $ kwProd <+> left <+> right

instance PrettyCode Coproduct where
  ppCode Coproduct {..} = do
    left <- ppArg _coproductLeft
    right <- ppArg _coproductRight
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
    ObjectProduct x -> ppCode x
    ObjectCoproduct x -> ppCode x
    ObjectHom x -> ppCode x
    ObjectInteger -> return kwInteger

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- keywords
--------------------------------------------------------------------------------

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

kwAdd :: Doc Ann
kwAdd = keyword Str.gebAdd

kwSub :: Doc Ann
kwSub = keyword Str.gebSub

kwMul :: Doc Ann
kwMul = keyword Str.gebMul

kwDiv :: Doc Ann
kwDiv = keyword Str.gebDiv

kwMod :: Doc Ann
kwMod = keyword Str.gebMod

kwEq :: Doc Ann
kwEq = keyword Str.gebEq

kwLt :: Doc Ann
kwLt = keyword Str.gebLt

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

kwInteger :: Doc Ann
kwInteger = keyword Str.gebInteger
