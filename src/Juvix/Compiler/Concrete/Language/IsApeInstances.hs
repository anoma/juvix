{-# OPTIONS_GHC -Wno-orphans #-}

module Juvix.Compiler.Concrete.Language.IsApeInstances where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language.Base
import Juvix.Data.Ape.Base as Ape
import Juvix.Data.NameKind
import Juvix.Parser.Lexer (isDelimiterStr)
import Juvix.Prelude
import Juvix.Prelude.Pretty (prettyText)

instance IsApe PatternApp ApeLeaf where
  toApe (PatternApp l r) =
    ApeApp
      Ape.App
        { _appLeft = toApe l,
          _appRight = toApe r
        }

instance IsApe Pattern ApeLeaf where
  toApe = \case
    PatternApplication a -> toApe a
    PatternInfixApplication a -> toApe a
    PatternPostfixApplication a -> toApe a
    e ->
      ApeLeaf
        Leaf
          { _leafAtomicity = atomicity e,
            _leafExpr = ApeLeafPattern e
          }

instance IsApe PatternArg ApeLeaf where
  toApe pa
    | Atom == atomicity pa =
        ApeLeaf
          Leaf
            { _leafAtomicity = Atom,
              _leafExpr = ApeLeafPatternArg pa
            }
    | otherwise = toApe (pa ^. patternArgPattern)

instance IsApe PatternPostfixApp ApeLeaf where
  toApe p@(PatternPostfixApp l op) =
    ApePostfix
      Postfix
        { _postfixFixity = getFixity p,
          _postfixLeft = toApe l,
          _postfixOp = ApeLeafPattern (PatternConstructor op)
        }

instance IsApe PatternInfixApp ApeLeaf where
  toApe i@(PatternInfixApp l op r) =
    ApeInfix
      Infix
        { _infixFixity = getFixity i,
          _infixLeft = toApe l,
          _infixRight = toApe r,
          _infixIsDelimiter = isDelimiterStr (prettyText (op ^. scopedIdenSrcName . S.nameConcrete)),
          _infixOp = ApeLeafPattern (PatternConstructor op)
        }

instance IsApe ScopedIden ApeLeaf where
  toApe iden =
    ApeLeaf
      Leaf
        { _leafAtomicity = Atom,
          _leafExpr = ApeLeafExpression (ExpressionIdentifier iden)
        }

toApeIdentifierType :: forall s. (SingI s) => IdentifierType s -> Ape ApeLeaf
toApeIdentifierType = case sing :: SStage s of
  SParsed -> toApe
  SScoped -> toApe

instance IsApe Name ApeLeaf where
  toApe n =
    ApeLeaf
      Leaf
        { _leafAtomicity = atomicity n,
          _leafExpr = ApeLeafAtom (sing :&: AtomIdentifier n)
        }

instance (SingI s) => IsApe (NamedApplication s) ApeLeaf where
  toApe a =
    ApeLeaf
      $ Leaf
        { _leafAtomicity = atomicity a,
          _leafExpr = ApeLeafAtom (sing :&: AtomNamedApplication a)
        }

instance IsApe Application ApeLeaf where
  toApe (Application l r) =
    ApeApp
      Ape.App
        { _appLeft = toApe l,
          _appRight = toApe r
        }

instance IsApe InfixApplication ApeLeaf where
  toApe i@(InfixApplication l op r) =
    ApeInfix
      Infix
        { _infixFixity = getFixity i,
          _infixLeft = toApe l,
          _infixRight = toApe r,
          _infixIsDelimiter = isDelimiterStr (prettyText (op ^. scopedIdenSrcName . S.nameConcrete)),
          _infixOp = ApeLeafExpression (ExpressionIdentifier op)
        }

instance IsApe PostfixApplication ApeLeaf where
  toApe p@(PostfixApplication l op) =
    ApePostfix
      Postfix
        { _postfixFixity = getFixity p,
          _postfixLeft = toApe l,
          _postfixOp = ApeLeafExpression (ExpressionIdentifier op)
        }

instance IsApe (Function 'Scoped) ApeLeaf where
  toApe (Function ps kw ret) =
    ApeInfix
      Infix
        { _infixFixity = funFixity,
          _infixLeft = toApe ps,
          _infixRight = toApe ret,
          _infixIsDelimiter = False,
          _infixOp = ApeLeafFunctionKw kw
        }

instance IsApe RecordUpdateApp ApeLeaf where
  toApe :: RecordUpdateApp -> Ape ApeLeaf
  toApe a =
    ApePostfix
      Postfix
        { _postfixFixity = updateFixity,
          _postfixOp = ApeLeafAtom (sing :&: AtomRecordUpdate (a ^. recordAppUpdate)),
          _postfixLeft = toApe (a ^. recordAppExpression)
        }

instance IsApe Expression ApeLeaf where
  toApe e = case e of
    ExpressionApplication a -> toApe a
    ExpressionInfixApplication a -> toApe a
    ExpressionPostfixApplication a -> toApe a
    ExpressionFunction a -> toApe a
    ExpressionNamedApplication a -> toApe a
    ExpressionRecordUpdate a -> toApe a
    ExpressionParensRecordUpdate {} -> leaf
    ExpressionParensIdentifier {} -> leaf
    ExpressionIdentifier {} -> leaf
    ExpressionList {} -> leaf
    ExpressionDo {} -> leaf
    ExpressionCase {} -> leaf
    ExpressionIf {} -> leaf
    ExpressionLambda {} -> leaf
    ExpressionLet {} -> leaf
    ExpressionUniverse {} -> leaf
    ExpressionHole {} -> leaf
    ExpressionInstanceHole {} -> leaf
    ExpressionLiteral {} -> leaf
    ExpressionBraces {} -> leaf
    ExpressionDoubleBraces {} -> leaf
    ExpressionIterator {} -> leaf
    where
      leaf =
        ApeLeaf
          Leaf
            { _leafAtomicity = atomicity e,
              _leafExpr = ApeLeafExpression e
            }

instance IsApe (FunctionParameters 'Scoped) ApeLeaf where
  toApe f
    | atomicity f == Atom =
        ApeLeaf
          Leaf
            { _leafAtomicity = Atom,
              _leafExpr = ApeLeafFunctionParams f
            }
    | otherwise = toApe (f ^. paramType)

instance HasAtomicity PatternArg where
  atomicity p
    | Implicit <- p ^. patternArgIsImplicit = Atom
    | ImplicitInstance <- p ^. patternArgIsImplicit = Atom
    | isJust (p ^. patternArgName) = Atom
    | otherwise = atomicity (p ^. patternArgPattern)

instance HasNameKind ScopedIden where
  getNameKind = getNameKind . (^. scopedIdenFinal)
  getNameKindPretty = getNameKindPretty . (^. scopedIdenFinal)
