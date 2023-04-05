module Sexp.Serialize where

import Mari.Library hiding (identity)
import qualified Mari.Library.HashMap as Map
import qualified Data.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

data Test a
  = Test
  | Test2 a
  | TestRec a (Test a)
  | TestRec' (Test a)
  | Test3 {fstt :: a, sndd :: Integer}
  deriving (Show, Generic)

-- let's turn this to deriving via
instance Sexp.DefaultOptions (Test a)

instance (Sexp.Serialize a) => Sexp.Serialize (Test a)

data TestRename
  = TestRename
  | TestRename1 Integer
  deriving (Show, Generic, Data)

testRenameConsturoctrs :: Sexp.Options
testRenameConsturoctrs =
  Sexp.changeName
    (Sexp.defaultOptions @TestRename)
    (Map.fromList [("TestRename", ":test"), ("TestRename1", ":test-1")])

data Infix a
  = Infix
      { infixOp :: (Sexp.B (Infix a)),
        infixLt :: (Sexp.B (Infix a)),
        infixInf :: (Infix a)
      }
  | InfixNoMore
      { infixOp :: (Sexp.B (Infix a)),
        infixLt :: (Sexp.B (Infix a)),
        infixRt :: (Sexp.B (Infix a))
      }
  deriving (Show, Generic, Eq)

-- Right xs = Sexp.parse "(+ 2 (:infix + 2 (:infix * (:infix * 5 (:infix - 2 3)) (:infix * 6 7 8))))"
-- λ> Sexp.partiallyDeserialize @(Infix Integer) xs

infixRename :: Sexp.Options
infixRename =
  Sexp.changeName
    (Sexp.defaultOptions @(Infix ()))
    (Map.fromList [("InfixNoMore", ":infix")])

instance Sexp.DefaultOptions (Infix a)

instance Sexp.Serialize a => Sexp.Serialize (Infix a) where
  serialize = Sexp.serializeOpt infixRename
  deserialize = Sexp.deserializeOpt infixRename

instance Sexp.DefaultOptions TestRename

instance Sexp.Serialize TestRename where
  serialize = Sexp.serializeOpt testRenameConsturoctrs
  deserialize = Sexp.deserializeOpt testRenameConsturoctrs

top :: T.TestTree
top =
  T.testGroup
    "automatic serialization"
    [ compositionIsJust,
      serializerTest
    ]

type TInt = Test Integer

compositionIsJust :: T.TestTree
compositionIsJust =
  T.testGroup
    "Composing Serialize and Deserialize is Just"
    [ T.testCase "No Argument" (idCheck Test),
      T.testCase "One Argument" (idCheck (Test2 3)),
      T.testCase "2 named arguments" (idCheck (Test3 4 5)),
      T.testCase "recursive constructor" (idCheck (TestRec 3 (Test2 2))),
      T.testCase "nested serialize" (isJust (identityRec (Test2 (Test2 3))) T.@=? True),
      T.testCase "testing renmaing" (isJust (identityRename (TestRename1 5)) T.@=? True)
    ]
  where
    idCheck x =
      x
        |> identity
        |> isJust
        |> (T.@=? True)

serializerTest :: T.TestTree
serializerTest =
  T.testGroup
    "Serialization form testing"
    [ T.testCase "name is expected" $
        Right (Sexp.serialize (TestRec (3 :: Integer) Test))
          T.@=? Sexp.parse "(:test-rec 3 :test)",
      T.testCase "name is expected for renaming" $
        Right (Sexp.serialize (TestRename1 5))
          T.@=? Sexp.parse "(:test-1 5)"
    ]

identityRec :: Test (Test Integer) -> Maybe (Test (Test Integer))
identityRec = Sexp.deserialize . Sexp.serialize

identityRename :: TestRename -> Maybe TestRename
identityRename = Sexp.deserialize . Sexp.serialize

identity :: TInt -> Maybe TInt
identity = Sexp.deserialize . Sexp.serialize

-- test ::
--   Maybe (C1 ('MetaCons "Test" 'PrefixI 'False) U1 p)
-- test = do
--   let M1 t = from (Test :: Test ())
--       L1 t2 = t
--   (Sexp.gput t2 |> Sexp.gget :: Maybe (C1 ('MetaCons "Test" 'PrefixI 'False) U1 p))
