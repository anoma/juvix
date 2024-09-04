module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Types where

import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty (fromGenericOptions)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew.Arity
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
import Juvix.Compiler.Store.Internal.Data.InstanceInfo
import Juvix.Data.PPOutput
import Juvix.Prelude

-- | the type of the constructor used in a pattern does
-- not match the type of the inductive being matched
data WrongConstructorType = WrongConstructorType
  { _wrongCtorTypeName :: Name,
    _wrongCtorTypeExpected :: InductiveName,
    _wrongCtorTypeActual :: InductiveName
  }

makeLenses ''WrongConstructorType

instance ToGenericError WrongConstructorType where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          ctorName = e ^. wrongCtorTypeName
          i = getLoc ctorName
          msg =
            "The constructor"
              <+> ppCode opts' ctorName
              <+> "belongs to the inductive type:"
                <> line
                <> indent' (ppCode opts' (e ^. wrongCtorTypeActual))
                <> line
                <> "but is expected to belong to the inductive type:"
                <> line
                <> indent' (ppCode opts' (e ^. wrongCtorTypeExpected))

data WrongReturnType = WrongReturnType
  { _wrongReturnTypeConstructorName :: Name,
    _wrongReturnTypeExpected :: Expression,
    _wrongReturnTypeActual :: Expression
  }

makeLenses ''WrongReturnType

instance ToGenericError WrongReturnType where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = j,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i, j]
            }
        where
          opts' = fromGenericOptions opts
          ctorName = e ^. wrongReturnTypeConstructorName
          i = getLoc ctorName
          ty = e ^. wrongReturnTypeActual
          j = getLoc ty
          msg =
            "The constructor"
              <+> ppCode opts' ctorName
              <+> "has the wrong return type:"
                <> line
                <> indent' (ppCode opts' ty)
                <> line
                <> "but is expected to have type:"
                <> line
                <> indent' (ppCode opts' (e ^. wrongReturnTypeExpected))

data UnsolvedMeta = UnsolvedMeta
  { _unsolvedMeta :: Hole,
    _unsolvedIsLoop :: Bool
  }

makeLenses ''UnsolvedMeta

instance ToGenericError UnsolvedMeta where
  genericError :: forall r. (Member (Reader GenericOptions) r) => UnsolvedMeta -> Sem r GenericError
  genericError e = do
    msg <- mkMsg
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = ppOutput msg,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (e ^. unsolvedMeta)
      mkMsg :: Sem r (Doc Ann)
      mkMsg = do
        m <- holeid
        let loopMsg
              | e ^. unsolvedIsLoop = Just "The inference algorithm found a loop."
              | otherwise = Nothing
            msg = "Unable to infer the hole" <>? loopMsg <>? m
        return msg
      holeid :: Sem r (Maybe (Doc Ann))
      holeid = runFail $ do
        opts <- fromGenericOptions <$> ask
        failUnlessM (asks (^. showNameIds))
        return (" " <> ppCode opts (e ^. unsolvedMeta . holeId))

-- | The arguments of a constructor pattern do not match
-- the expected arguments of the constructor
data WrongConstructorAppArgs = WrongConstructorAppArgs
  { _wrongCtorAppApp :: ConstructorApp,
    _wrongCtorAppTypes :: [FunctionParameter],
    _wrongCtorAppName :: Name
  }

makeLenses ''WrongConstructorAppArgs

instance ToGenericError WrongConstructorAppArgs where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. wrongCtorAppApp . constrAppConstructor)
          msg =
            "The constructor:"
              <+> ctorName
              <+> "is being matched against"
              <+> numPats
                <> ":"
                <> line
                <> indent' (ppCode opts' (e ^. wrongCtorAppApp))
                <> line
                <> "but is expected to be matched against"
              <+> numTypes
              <+> "with the following types:"
                <> line
                <> indent' (hsep (ctorName : (ppCode opts' <$> (e ^. wrongCtorAppTypes))))
          numPats :: Doc ann
          numPats = pat (length (e ^. wrongCtorAppApp . constrAppParameters))
          numTypes :: Doc ann
          numTypes = pat (length (e ^. wrongCtorAppTypes))

          ctorName :: Doc Ann
          ctorName = ppCode opts' (e ^. wrongCtorAppApp . constrAppConstructor)

          pat :: Int -> Doc ann
          pat n = pretty n <+> plural "pattern" "patterns" n

data WrongTypeThing
  = WrongTypeThingPattern Pattern
  | WrongTypeThingExpression WrongTypeThingExpression

data WrongTypeThingExpression = MkWrongTypeThingExpression
  { _wrongTypeNormalizedExpression :: NormalizedExpression,
    _wrongTypeInferredExpression :: Expression
  }

-- | the type of an expression does not match the inferred type
data WrongType = WrongType
  { _wrongTypeThing :: WrongTypeThing,
    _wrongTypeExpected :: NormalizedExpression,
    _wrongTypeActual :: NormalizedExpression
  }

makeLenses ''WrongType
makeLenses ''WrongTypeThingExpression

instance HasLoc WrongTypeThing where
  getLoc = \case
    WrongTypeThingPattern p -> getLoc p
    WrongTypeThingExpression e -> getLoc e

instance HasLoc WrongTypeThingExpression where
  getLoc = getLoc . (^. wrongTypeNormalizedExpression)

-- TODO we should show both the normalized and original version of the expression when relevant.
instance ToGenericError WrongType where
  genericError err = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (err ^. wrongTypeThing)
          msg =
            "The"
              <+> thingName
              <+> thing
              <+> "has type:"
                <> line
                <> indent' (ppCode opts' (err ^. wrongTypeActual . normalizedExpression))
                <> line
                <> "but is expected to have type:"
                <> line
                <> indent' (ppCode opts' (err ^. wrongTypeExpected . normalizedExpression))

          thingName :: Doc a
          thingName = case err ^. wrongTypeThing of
            WrongTypeThingExpression {} -> "expression"
            WrongTypeThingPattern {} -> "pattern"

          thing :: Doc CodeAnn
          thing = case err ^. wrongTypeThing of
            WrongTypeThingExpression e -> ppCode opts' (e ^. wrongTypeInferredExpression)
            WrongTypeThingPattern p -> ppCode opts' p

-- | The left hand expression of a function application is not
-- a function type.
data ExpectedFunctionType = ExpectedFunctionType
  { _expectedFunctionTypeExpression :: Expression,
    _expectedFunctionTypeLeft :: Expression,
    _expectedFunctionTypeType :: Expression
  }

makeLenses ''ExpectedFunctionType

instance ToGenericError ExpectedFunctionType where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. expectedFunctionTypeExpression)
          msg =
            "Type error near"
              <+> pretty (getLoc subjectExpr)
                <> "."
                <> line
                <> "In the expression:"
                <> line
                <> indent' (ppCode opts' (e ^. expectedFunctionTypeExpression))
                <> line
                <> "the expression"
              <+> ppCode opts' (e ^. expectedFunctionTypeLeft)
              <+> "is expected to have a function type but has type:"
                <> line
                <> indent' (ppCode opts' (e ^. expectedFunctionTypeType))
          subjectExpr :: Expression
          subjectExpr = e ^. expectedFunctionTypeExpression

data WrongNumberArgumentsIndType = WrongNumberArgumentsIndType
  { _wrongNumberArgumentsIndTypeActualType :: Expression,
    _wrongNumberArgumentsIndTypeExpectedNumArgs :: Int,
    _wrongNumberArgumentsIndTypeActualNumArgs :: Int
  }

makeLenses ''WrongNumberArgumentsIndType

instance ToGenericError WrongNumberArgumentsIndType where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          ty = e ^. wrongNumberArgumentsIndTypeActualType
          i = getLoc ty
          expectedNumArgs = e ^. wrongNumberArgumentsIndTypeExpectedNumArgs
          actualNumArgs = e ^. wrongNumberArgumentsIndTypeActualNumArgs
          msg =
            "The type"
              <+> ppCode opts' ty
              <+> "expects"
              <+> ( if
                        | expectedNumArgs == 0 -> "no arguments"
                        | expectedNumArgs == 1 -> "one argument"
                        | otherwise -> pretty expectedNumArgs <+> "arguments"
                  )
                <> ", but"
              <+> ( if
                        | actualNumArgs == 0 -> "no argument is"
                        | actualNumArgs == 1 -> "only one argument is"
                        | otherwise -> pretty actualNumArgs <+> "arguments are"
                  )
              <+> "given"

newtype InvalidPatternMatching = InvalidPatternMatching
  { _impracticalPatternMatchingType :: Expression
  }

makeLenses ''InvalidPatternMatching

instance ToGenericError InvalidPatternMatching where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          ty = e ^. impracticalPatternMatchingType
          i = getLoc ty
          msg =
            "The type"
              <+> ppCode opts' ty
              <+> "is not an inductive data type."
              <+> "Therefore, pattern-matching is not available here"

newtype UnsupportedTypeFunction = UnsupportedTypeFunction
  { _unsupportedTypeFunction :: FunctionDef
  }

instance ToGenericError UnsupportedTypeFunction where
  genericError UnsupportedTypeFunction {..} = do
    opts <- fromGenericOptions <$> ask
    let msg =
          "Unsupported type function"
            <+> ppCode opts (_unsupportedTypeFunction ^. funDefName)
              <> "."
              <> line
              <> "Only terminating functions with a single clause and no pattern matching are supported."
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _unsupportedTypeFunction

newtype InvalidInstanceType = InvalidInstanceType
  { _invalidInstanceTypeExpression :: Expression
  }

instance ToGenericError InvalidInstanceType where
  genericError InvalidInstanceType {..} = do
    opts <- fromGenericOptions <$> ask
    let msg =
          "Invalid instance type:"
            <+> ppCode opts _invalidInstanceTypeExpression
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _invalidInstanceTypeExpression

newtype InvalidCoercionType = InvalidCoercionType
  { _invalidCoercionTypeExpression :: Expression
  }

instance ToGenericError InvalidCoercionType where
  genericError InvalidCoercionType {..} = do
    opts <- fromGenericOptions <$> ask
    let msg =
          "Invalid coercion type:"
            <+> ppCode opts _invalidCoercionTypeExpression
              <> line
              <> "A coercion must have exactly one instance argument."
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _invalidCoercionTypeExpression

newtype WrongCoercionArgument = WrongCoercionArgument
  { _wrongCoercionArgumentParameter :: FunctionParameter
  }

makeLenses ''WrongCoercionArgument

instance ToGenericError WrongCoercionArgument where
  genericError e = generr
    where
      generr =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage =
                ppOutput $
                  "Expected an implicit type argument."
                    <> line
                    <> "A coercion can have only implicit type arguments followed by exactly one instance argument.",
              _genericErrorIntervals = [i]
            }
        where
          i = getLoc (e ^. wrongCoercionArgumentParameter)

newtype TargetNotATrait = TargetNotATrait
  { _targetNotATraitType :: Expression
  }

instance ToGenericError TargetNotATrait where
  genericError TargetNotATrait {..} = do
    opts <- fromGenericOptions <$> ask
    let msg =
          "Expected a type with a trait in the target:"
            <+> ppCode opts _targetNotATraitType
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _targetNotATraitType

newtype NotATrait = NotATrait
  { _notATraitExpression :: Expression
  }

makeLenses ''NotATrait

instance ToGenericError NotATrait where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. notATraitExpression)
          msg =
            "Expected a trait:"
              <+> ppCode opts' (e ^. notATraitExpression)

newtype CoercionCycles = CoercionCycles
  { _coercionCycles :: NonEmpty Name
  }

makeLenses ''CoercionCycles

instance ToGenericError CoercionCycles where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (head (e ^. coercionCycles))
          msg =
            "There exist coercion cycles involving these traits:"
              <> line
              <> indent' (hsep (ppCode opts' <$> take 10 (toList (e ^. coercionCycles))))

data NoInstance = NoInstance
  { _noInstanceType :: NormalizedExpression,
    _noInstanceLoc :: Interval
  }

makeLenses ''NoInstance

instance ToGenericError NoInstance where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = e ^. noInstanceLoc
          msg =
            "No trait instance found for:"
              <+> ppCode opts' (e ^. noInstanceType . normalizedExpression)

data AmbiguousInstances = AmbiguousInstances
  { _ambiguousInstancesType :: NormalizedExpression,
    _ambiguousInstancesInfos :: [InstanceInfo],
    _ambiguousInstancesLoc :: Interval
  }

makeLenses ''AmbiguousInstances

instance ToGenericError AmbiguousInstances where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = e ^. ambiguousInstancesLoc
          locs = itemize $ map (pretty . getLoc) (e ^. ambiguousInstancesInfos)
          msg =
            "Multiple trait instances found for"
              <+> ppCode opts' (e ^. ambiguousInstancesType . normalizedExpression)
                <> line
                <> "Matching instances found at:"
                <> line
                <> indent' locs

data SubsumedInstance = SubsumedInstance
  { _subsumedInstance :: InstanceInfo,
    _subsumedInstanceParents :: [InstanceInfo],
    _subsumedInstanceLoc :: Interval
  }

makeLenses ''SubsumedInstance

instance ToGenericError SubsumedInstance where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = e ^. subsumedInstanceLoc
          locs = itemize $ map (pretty . getLoc) (e ^. subsumedInstanceParents)
          msg =
            "The instance"
              <+> ppCode opts' (e ^. subsumedInstance . instanceInfoIden)
              <+> "is subsumed by instances at:"
                <> line
                <> indent' locs

newtype ExplicitInstanceArgument = ExplicitInstanceArgument
  { _explicitInstanceArgumentParameter :: FunctionParameter
  }

makeLenses ''ExplicitInstanceArgument

instance ToGenericError ExplicitInstanceArgument where
  genericError e = generr
    where
      generr =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput "Explicit instance arguments not allowed",
              _genericErrorIntervals = [i]
            }
        where
          i = getLoc (e ^. explicitInstanceArgumentParameter)

newtype TraitNotTerminating = TraitNotTerminating
  { _traitNotTerminating :: Expression
  }

makeLenses ''TraitNotTerminating

instance ToGenericError TraitNotTerminating where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. traitNotTerminating)
          msg =
            "Non-decreasing trait argument:"
              <+> ppCode opts' (e ^. traitNotTerminating)
                <> line
                <> "Each parameter of a trait in an instance argument must be structurally smaller than some parameter of the trait in the instance target"

newtype DefaultArgLoop = DefaultArgLoop
  { _defaultArgLoop :: NonEmpty ArgId
  }

makeLenses ''DefaultArgLoop

instance ToGenericError DefaultArgLoop where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (head (e ^. defaultArgLoop))
          msg :: Doc Ann =
            "Inserting default arguments caused a loop. The involved arguments are:"
              <> line
              <> itemize (ppCode opts' <$> e ^. defaultArgLoop)

newtype BadScope = BadScope
  { _badScopeVar :: VarName
  }

makeLenses ''BadScope

instance ToGenericError BadScope where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. badScopeVar)
          var = e ^. badScopeVar
          msg :: Doc Ann =
            annotate AnnImportant "Oops! This is a known bug in the Juvix compiler."
              <> line
              <> "Most likely, the inference algorithm inserted the variable"
              <+> ppCode opts' var
              <+> "in a place where it is not in scope."
                <> line
                <> "As a workaround, explicitly provide a type in the place where you think the variable got inserted."
                <> line
                <> "More information at https://github.com/anoma/juvix/issues/2247"
