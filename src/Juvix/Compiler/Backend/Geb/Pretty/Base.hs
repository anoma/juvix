module Juvix.Compiler.Backend.Geb.Pretty.Base
  ( module Juvix.Compiler.Backend.Geb.Pretty.Base,
    module Juvix.Data.CodeAnn,
    module Juvix.Compiler.Backend.Geb.Pretty.Options,
  )
where

import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty.Keywords
import Juvix.Compiler.Backend.Geb.Pretty.Options
import Juvix.Data.CodeAnn

doc :: (HasAtomicity c, PrettyCode c) => Options -> c -> Doc Ann
doc opts x =
  run $
    runReader opts $
      case atomicity x of
        Atom -> ppCode x
        Aggregate _ -> parens <$> ppCode x

docLisp :: Options -> Text -> Text -> Morphism -> Object -> Doc Ann
docLisp opts packageName entryName morph obj =
  "(defpackage #:"
    <> pretty packageName
    <> line
    <> indent' "(:shadowing-import-from :geb.lambda.spec #:func #:pair)"
    <> line
    <> indent' "(:shadowing-import-from :geb.spec #:case)"
    <> line
    <> indent' "(:use #:common-lisp #:geb.lambda.spec #:geb))"
    <> line
    <> line
    <> "(in-package :"
    <> pretty packageName
    <> ")"
    <> line
    <> line
    <> parens
      ( "defparameter"
          <+> pretty entryName
            <> line
            <> indent'
              ( parens
                  ( "typed"
                      <> line
                      <> indent'
                        (vsep [doc opts morph, doc opts obj])
                  )
              )
      )

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

ppCode' :: (PrettyCode c) => Options -> c -> Doc Ann
ppCode' opts = run . runReader opts . ppCode

instance PrettyCode Case where
  ppCode Case {..} = do
    lty <- ppArg _caseLeftType
    rty <- ppArg _caseRightType
    cod <- ppArg _caseCodomainType
    val <- ppArg _caseOn
    left <- ppArg _caseLeft
    right <- ppArg _caseRight
    return $
      kwCaseOn <> line <> indent 2 (vsep [lty, rty, cod, val, left, right])

instance PrettyCode Pair where
  ppCode Pair {..} = do
    lty <- ppArg _pairLeftType
    rty <- ppArg _pairRightType
    left <- ppArg _pairLeft
    right <- ppArg _pairRight
    return $ kwPair <> line <> indent' (vsep [lty, rty, left, right])

instance PrettyCode First where
  ppCode First {..} = do
    lty <- ppArg _firstLeftType
    rty <- ppArg _firstRightType
    val <- ppArg _firstValue
    return $ kwFst <> line <> indent' (vsep [lty, rty, val])

instance PrettyCode Second where
  ppCode Second {..} = do
    lty <- ppArg _secondLeftType
    rty <- ppArg _secondRightType
    val <- ppArg _secondValue
    return $ kwSnd <> line <> indent' (vsep [lty, rty, val])

instance PrettyCode Lambda where
  ppCode Lambda {..} = do
    vty <- ppArg _lambdaVarType
    bty <- ppArg _lambdaBodyType
    body <- ppArg _lambdaBody
    return $ kwLamb <> line <> indent' (vsep [vty, bty, body])

instance PrettyCode Application where
  ppCode Application {..} = do
    dom <- ppArg _applicationDomainType
    cod <- ppArg _applicationCodomainType
    left <- ppArg _applicationLeft
    right <- ppArg _applicationRight
    return $ kwApp <> line <> indent' (vsep [dom, cod, left, right])

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
    return $ op <> line <> indent' (vsep [left, right])

instance PrettyCode Morphism where
  ppCode = \case
    MorphismAbsurd val -> do
      v <- ppArg val
      return $ kwAbsurd <+> v
    MorphismUnit ->
      return kwUnit
    MorphismLeft val -> do
      v <- ppArg val
      return $ kwLeft <> line <> indent' v
    MorphismRight val -> do
      v <- ppArg val
      return $ kwRight <> line <> indent' v
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
    return $ kwProd <> line <> indent' (vsep [left, right])

instance PrettyCode Coproduct where
  ppCode Coproduct {..} = do
    left <- ppArg _coproductLeft
    right <- ppArg _coproductRight
    return $ kwCoprod <> line <> indent' (vsep [left, right])

instance PrettyCode Hom where
  ppCode Hom {..} = do
    dom <- ppArg _homDomain
    cod <- ppArg _homCodomain
    return $ kwHom <> line <> indent' (vsep [dom, cod])

instance PrettyCode Object where
  ppCode =
    \case
      ObjectInitial -> return kwInitial
      ObjectTerminal -> return kwTerminal
      ObjectProduct x -> ppCode x
      ObjectCoproduct x -> ppCode x
      ObjectHom x -> ppCode x
      ObjectInteger -> return kwInteger

instance PrettyCode Expression where
  ppCode = \case
    ExpressionMorphism x -> ppCode x
    ExpressionObject x -> ppCode x

instance PrettyCode TypedMorphism where
  ppCode TypedMorphism {..} = do
    m <- ppArg _typedMorphism
    o <- ppArg _typedMorphismObject
    return $ kwTyped <> line <> indent' (vsep [m, o])

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
