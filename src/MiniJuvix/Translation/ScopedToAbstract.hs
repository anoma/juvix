module MiniJuvix.Translation.ScopedToAbstract where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified MiniJuvix.Syntax.Abstract.Language as A
import qualified MiniJuvix.Syntax.Concrete.Language as C


type Err = Text

unsupported :: Members '[Error Err] r => Err -> Sem r a
unsupported msg = throw $ msg <> " not yet supported"

translateModule :: Module 'Scoped 'ModuleTop -> Either Err A.TopModule
translateModule = undefined

goTopModule :: Members '[Error Err] r => Module 'Scoped 'ModuleTop -> Sem r A.TopModule
goTopModule = undefined

goLocalModule :: Members '[Error Err] r => Module 'Scoped 'ModuleLocal -> Sem r A.LocalModule
goLocalModule (Module n par b) = case par of
  [] -> unsupported "Module parameters"
  _ -> A.Module n <$> mapM goStatement b

goStatement :: Members '[Error Err] r => Statement 'Scoped -> Sem r A.Statement
goStatement s = case s of
  StatementInductive i -> A.StatementInductive <$> goInductive i

goInductive :: Members '[Error Err] r => InductiveDef 'Scoped -> Sem r A.InductiveDef
goInductive = undefined

goConstructorDef :: Members '[Error Err] r => InductiveConstructorDef 'Scoped -> Sem r A.InductiveConstructorDef
goConstructorDef (InductiveConstructorDef c ty) = A.InductiveConstructorDef c <$> goExpression ty

goExpression :: forall r. Members '[Error Err] r => Expression -> Sem r A.Expression
goExpression e = case e of
  ExpressionIdentifier nt -> return (goIden nt)
  ExpressionParensIdentifier nt -> return (goIden nt)
  ExpressionApplication a -> A.ExpressionApplication <$> goApplication a
  ExpressionInfixApplication ia -> A.ExpressionApplication <$> goInfix ia
  ExpressionPostfixApplication pa -> A.ExpressionApplication <$> goPostfix pa
  ExpressionLambda {} -> unsupported "Lambda"
  ExpressionMatch {} -> unsupported "Match"
  ExpressionLetBlock {} -> unsupported "Let Block"
  ExpressionUniverse uni -> return $ A.ExpressionUniverse (goUniverse uni)
  ExpressionFunction func -> A.ExpressionFunction <$> goFunction func
  where
  goIden :: S.Name -> A.Expression
  goIden = A.ExpressionIden . goName 
  goName :: S.Name -> A.Iden
  goName nm = case nm ^. S.nameKind of
     S.KNameConstructor -> A.IdenConstructor nm
     S.KNameFunction -> A.IdenDefined nm
     S.KNameInductive -> A.IdenInductive nm
     S.KNameLocal -> A.IdenVar (fromUnqualified nm)
     S.KNameAxiom -> A.IdenAxiom nm
     S.KNameLocalModule -> impossible
     S.KNameTopModule -> impossible
    where
    fromUnqualified :: S.Name -> S.Symbol
    fromUnqualified = over S.nameConcrete (\c -> case c of
      NameQualified {} -> impossible
      NameUnqualified u -> u)

  goApplication :: Application -> Sem r A.Application
  goApplication (Application l r) = do 
    l' <- goExpression l
    r' <- goExpression r
    return (A.Application l' r')

  goPostfix :: PostfixApplication -> Sem r A.Application
  goPostfix (PostfixApplication l op) = do
    l' <- goExpression l
    let op' = goIden op
    return (A.Application op' l')

  goInfix :: InfixApplication -> Sem r A.Application
  goInfix (InfixApplication l op r) = do
    l' <- goExpression l
    let op' = goIden op
    r' <- goExpression r
    return $ A.Application (A.ExpressionApplication (A.Application op' l')) r'

goUniverse :: Universe -> A.Universe
goUniverse (Universe l) = A.Universe l

goFunction :: Members '[Error Err] r => Function 'Scoped -> Sem r A.Function
goFunction (Function l r) = do
  _funParameter <- goFunctionParameter l
  _funReturn <- goExpression r
  return A.Function {..}

defaultUsage :: Usage
defaultUsage = UsageOmega

goUsage :: Maybe Usage -> Usage
goUsage = fromMaybe defaultUsage

goFunctionParameter :: Members '[Error Err] r => FunctionParameter 'Scoped -> Sem r A.FunctionParameter
goFunctionParameter (FunctionParameter _paramName u ty) = do
  _paramType <- goExpression ty
  let _paramUsage = goUsage u
  return A.FunctionParameter {..}

goPatternApplication :: forall r. Members '[Error Err] r => PatternApp -> Sem r A.ConstructorApp
goPatternApplication (PatternApp l r) = undefined
  where
   viewApp :: Pattern -> Sem r (A.Name, [Pattern])
   viewApp p = case p of
     PatternConstructor c -> return (c, [])
     PatternApplication (PatternApp l' r') -> 
         second (`snoc` r') <$> viewApp l'
     PatternInfixApplication (PatternInfixApp l' c r') -> 
       undefined

goPattern :: forall r. Members '[Error Err] r => Pattern -> Sem r A.Pattern
goPattern p = case p of
  PatternVariable a -> return $ A.PatternVariable a
