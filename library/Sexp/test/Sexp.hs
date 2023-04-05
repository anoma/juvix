module Sexp (top) where

import qualified Data.IORef as IORef
import Mari.Library
import qualified Data.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

top :: T.TestTree
top =
  T.testGroup
    "sexp pass tests:"
    [ lastWorksAsExpected,
      foldrWorksAsExpted,
      mapPredStarWorksAsExpted,
      mapPredStarWorksAsExpted2,
      mapPredStarWorksAsExpted3,
      listWorksAsExpected,
      listStarWorksAsExpected,
      traversePredWorksAsExpected,
      subsetSeralization
    ]

--------------------------------------------------------------------------------
-- Tests not exported
--------------------------------------------------------------------------------

lastWorksAsExpected :: T.TestTree
lastWorksAsExpected =
  T.testGroup
    "last works as expected"
    [ T.testCase
        "last on Cons"
        (fmap Sexp.last (Sexp.parse "(1 2 3 4)") T.@=? Sexp.parse "4"),
      T.testCase
        "last on atom"
        (fmap Sexp.last (Sexp.parse "4") T.@=? Sexp.parse "4"),
      T.testCase
        "last on Nil"
        (fmap Sexp.last (Sexp.parse "()") T.@=? Sexp.parse "()")
    ]

foldrWorksAsExpted :: T.TestTree
foldrWorksAsExpted =
  T.testGroup
    "foldr works as epxected"
    [ T.testCase
        "foldr on (1 2 3 4 5) properly adds"
        (Sexp.foldr (\(Sexp.Atom (Sexp.N n Nothing)) acc -> n + acc) 0 ns T.@=? 15)
    ]
  where
    Right ns = Sexp.parse "(1 2 3 4 5)"

mapPredStarWorksAsExpted :: T.TestTree
mapPredStarWorksAsExpted =
  T.testGroup
    "mapPredStar works as Exptected"
    [ T.testCase
        "mapPredStar properly recurses and does not change the non car position"
        ( Sexp.mapPredStar nest (== "if") (Sexp.Cons (Sexp.atom ":if") . Sexp.cdr)
            T.@=? expectedNest
        )
    ]
  where
    Right nest =
      Sexp.parse "(if x y (if if l))"
    Right expectedNest =
      Sexp.parse "(:if x y (:if if l))"

mapPredStarWorksAsExpted2 :: T.TestTree
mapPredStarWorksAsExpted2 =
  T.testGroup
    "mapPredStar works as Exptected"
    [ T.testCase
        "mapPredStar properly searches"
        ( Sexp.mapPredStar nest (== "if") (Sexp.Cons (Sexp.atom ":if") . Sexp.cdr)
            T.@=? expectedNest
        )
    ]
  where
    Right nest =
      Sexp.parse "(g x y (f z l))"
    Right expectedNest =
      Sexp.parse "(g x y (f z l))"

mapPredStarWorksAsExpted3 :: T.TestTree
mapPredStarWorksAsExpted3 =
  T.testGroup
    "mapPredStar works as Exptected"
    [ T.testCase
        "mapPredStar can deal with bigger sexp"
        ( Sexp.mapPredStar
            nest
            (== ":defop")
            (\sexp -> Sexp.Cons (Sexp.atom ":op") (Sexp.cdr sexp))
            T.@=? expectedNest
        )
    ]
  where
    Right nest =
      Sexp.parse
        ( "(:defmodule Printer ()                     "
            <> "   (:defhandler printer                     "
            <> "    ((:defop print () printLn)              "
            <> "     (:defop pure (x) (toString x))))       "
            <> "   (:defun prog (a)                         "
            <> "      (:do                                  "
            <> "          (:do-body (:do-op print (a)))     "
            <> "          (:do-body (:do-pure a))))         "
            <> "   (:defun foo ()                           "
            <> "      (:via printer prog)))))"
        )
    Right expectedNest =
      Sexp.parse
        ( "(:defmodule Printer ()                       "
            <> "   (:defhandler printer                     "
            <> "    ((:op print () printLn)                 "
            <> "     (:op pure (x) (toString x))))          "
            <> "   (:defun prog (a)                         "
            <> "      (:do                                  "
            <> "          (:do-body (:do-op print (a)))     "
            <> "          (:do-body (:do-pure a))))         "
            <> "   (:defun foo ()                           "
            <> "      (:via printer prog)))))"
        )

listWorksAsExpected :: T.TestTree
listWorksAsExpected =
  T.testGroup
    "list works as epctected"
    [ T.testCase
        "list on a term is correct"
        (Sexp.parse "(1 2 3)" T.@=? Right manualList)
    ]
  where
    manualList =
      Sexp.list [Sexp.number 1, Sexp.number 2, Sexp.number 3]

listStarWorksAsExpected :: T.TestTree
listStarWorksAsExpected =
  T.testGroup
    "list* works as epctected"
    [ T.testCase
        "list* on a list properly removes the last list"
        (Sexp.parse "(1 2 3)" T.@=? Right manualList)
    ]
  where
    manualList =
      Sexp.listStar [Sexp.number 1, Sexp.number 2, Sexp.list [Sexp.number 3]]

traversePredWorksAsExpected :: T.TestTree
traversePredWorksAsExpected =
  T.testGroup
    "traversePredOptStar works as expected"
    [ T.testCase
        "traversePredOpStar properly matches on car application"
        $ do
          y <- IORef.newIORef Sexp.Nil
          let f :: Sexp.T -> IO Sexp.T
              f rest = do
                IORef.modifyIORef' y (rest Sexp.:>)
                pure (Sexp.Cons (Sexp.atom "bob") (Sexp.cdr rest))
              Right xs =
                Sexp.parse "(let let (let))"
          _ <- Sexp.traversePredOptStar xs (== "let") (Sexp.autoRecurse f) mempty
          v <- IORef.readIORef y
          Sexp.parse "((let) (let let (let)))" T.@=? Right v
    ]

data SubSet
  = Add (Sexp.B SubSet) (Sexp.B SubSet)
  | Mul (Sexp.B SubSet) (Sexp.B SubSet)
  deriving (Show, Generic, Eq)

instance Sexp.DefaultOptions SubSet

instance Sexp.Serialize SubSet

subsetSeralization :: T.TestTree
subsetSeralization =
  T.testGroup
    "partial serialization works"
    [ T.testCase
        "Deeply Nested Serializers work as one ought to expect"
        $ do
          let expected =
                Sexp.list
                  [ Sexp.atom ":sub",
                    Sexp.primOp $
                      Add
                        (Sexp.primOp (Mul (Sexp.number 3) (Sexp.number 6)))
                        (Sexp.primOp (Add (Sexp.number 4) (Sexp.number 5))),
                    Sexp.primOp $
                      Add
                        ( Sexp.list
                            [ Sexp.atom ":sub",
                              Sexp.number 3,
                              Sexp.primOp (Add (Sexp.number 5) (Sexp.number 6))
                            ]
                        )
                        (Sexp.number 5)
                  ]
              Right term =
                Sexp.parse
                  "(:sub (:add (:mul 3 6) (:add 4 5)) (:add (:sub 3 (:add 5 6)) 5))"
          expected T.@=? Sexp.partiallyDeserialize @SubSet term
    ]
