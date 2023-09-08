module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error.Types where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty (fromGenericOptions)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
import Juvix.Data.PPOutput
import Juvix.Prelude

data WrongConstructorAppLength = WrongConstructorAppLength
  { _wrongConstructorAppLength :: ConstructorApp,
    _wrongConstructorAppLengthExpected :: Int
  }

makeLenses ''WrongConstructorAppLength

instance ToGenericError WrongConstructorAppLength where
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
          i = getLoc (e ^. wrongConstructorAppLength)
          msg =
            "The constructor"
              <+> ppCode opts' (e ^. wrongConstructorAppLength . constrAppConstructor)
              <+> "should have"
              <+> arguments (e ^. wrongConstructorAppLengthExpected)
                <> ", but has been given"
              <+> pretty actual
                <> line
                <> "Perhaps you forgot parentheses around a pattern?"

          actual :: Int
          actual = length (e ^. wrongConstructorAppLength . constrAppParameters)
          arguments :: Int -> Doc ann
          arguments n = num <+> plural "argument" "arguments" n
            where
              num
                | n == 0 = "no"
                | otherwise = pretty n

newtype LhsTooManyPatterns = LhsTooManyPatterns
  { _lhsTooManyPatternsRemaining :: NonEmpty PatternArg
  }

makeLenses ''LhsTooManyPatterns

instance ToGenericError LhsTooManyPatterns where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = ppOutput msg,
          _genericErrorIntervals = [i]
        }
    where
      i = getLocSpan (e ^. lhsTooManyPatternsRemaining)
      n = length (e ^. lhsTooManyPatternsRemaining)
      howMany =
        "The last" <+> case n of
          1 -> "pattern"
          _ -> pretty n <+> "patterns"
      msg =
        howMany <+> "on the left hand side of the function clause" <+> wasWere <+> "not expected"
      wasWere
        | n == 1 = "was"
        | otherwise = "were"

data WrongPatternIsImplicit = WrongPatternIsImplicit
  { _wrongPatternIsImplicitExpected :: IsImplicit,
    _wrongPatternIsImplicitActual :: PatternArg
  }

makeLenses ''WrongPatternIsImplicit

instance ToGenericError WrongPatternIsImplicit where
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
          i = getLoc (e ^. wrongPatternIsImplicitActual)
          expec = e ^. wrongPatternIsImplicitExpected
          found = e ^. wrongPatternIsImplicitActual . patternArgIsImplicit
          pat = e ^. wrongPatternIsImplicitActual
          msg =
            "Expected an"
              <+> pretty expec
              <+> "pattern but found an"
              <+> pretty found
              <+> "pattern:"
              <+> ppCode opts' pat

data ExpectedExplicitArgument = ExpectedExplicitArgument
  { _expectedExplicitArgumentApp :: (Expression, [ApplicationArg]),
    _expectedExplicitArgumentIx :: Int
  }

makeLenses ''ExpectedExplicitArgument

instance ToGenericError ExpectedExplicitArgument where
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
          app@(f, args) = e ^. expectedExplicitArgumentApp
          idx = e ^. expectedExplicitArgumentIx
          arg :: ApplicationArg
          arg = (toList args !! idx)
          i = getLoc arg
          msg =
            "Expected an explicit argument as the"
              <+> ordinal (succ idx)
              <+> "argument of"
              <+> ppCode opts' f
              <+> "but found"
              <+> ppArg opts' arg
                <> "."
                <> softline
                <> "In the application"
              <+> ppApp opts' app

newtype PatternFunction = PatternFunction
  { _patternFunction :: ConstructorApp
  }

makeLenses ''PatternFunction

instance ToGenericError PatternFunction where
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
          i = getLoc (e ^. patternFunction)
          msg =
            "Invalid pattern"
              <+> ppCode opts' (e ^. patternFunction) <> "."
              <+> "Function types cannot be pattern matched"

data TooManyArguments = TooManyArguments
  { _tooManyArgumentsApp :: (Expression, [ApplicationArg]),
    _tooManyArgumentsUnexpected :: Int
  }

makeLenses ''TooManyArguments

instance ToGenericError TooManyArguments where
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
          i = getLocSpan (nonEmpty' (map (^. appArg) unexpectedArgs))
          (fun, args) = e ^. tooManyArgumentsApp
          numUnexpected :: Int
          numUnexpected = e ^. tooManyArgumentsUnexpected
          unexpectedArgs :: [ApplicationArg]
          unexpectedArgs = reverse . take numUnexpected . reverse $ args
          ppUnexpectedArgs = hsep (map (ppArg opts') unexpectedArgs)
          app :: Expression
          app = foldApplication fun args
          msg =
            "Too many arguments in the application"
              <+> ppCode opts' app <> "."
              <+> "The last"
              <+> numArguments
                <> ", namely"
              <+> ppUnexpectedArgs
                <> ","
              <+> wasNotExpected
          numArguments :: Doc ann
          numArguments = plural "argument" (pretty numUnexpected <+> "arguments") numUnexpected
          wasNotExpected :: Doc ann
          wasNotExpected
            | numUnexpected == 1 = "was not expected"
            | otherwise = "were not expected"

data FunctionApplied = FunctionApplied
  { _functionAppliedFunction :: Function,
    _functionAppliedArguments :: [ApplicationArg]
  }

makeLenses ''FunctionApplied

instance ToGenericError FunctionApplied where
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
          i = getLocSpan (fun :| map (^. appArg) args)
          args = e ^. functionAppliedArguments
          fun = ExpressionFunction (e ^. functionAppliedFunction)
          msg =
            "A function type cannot be applied."
              <> softline
              <> "In the application"
              <+> ppApp opts' (fun, args)

data BuiltinNotFullyApplied = BuiltinNotFullyApplied
  { _builtinNotFullyAppliedName :: Name,
    _builtinNotFullyAplliedExpectedArgsNum :: Int
  }

makeLenses ''BuiltinNotFullyApplied

instance ToGenericError BuiltinNotFullyApplied where
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
          i = getLoc (e ^. builtinNotFullyAppliedName)
          argsNum = e ^. builtinNotFullyAplliedExpectedArgsNum
          msg =
            "The lazy builtin"
              <+> ppCode opts' (e ^. builtinNotFullyAppliedName)
              <+> "must be applied to exactly"
              <+> pretty argsNum
              <+> "arguments"
