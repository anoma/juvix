module Nockma.Parse.Positive where

import Base
import Data.ByteString qualified as BS
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty (ppPrint)
import Juvix.Compiler.Nockma.Translation.FromSource (parseText)
import Juvix.Parser.Error
import Juvix.Prelude.Pretty
import Text.Megaparsec

data PosTest = PosTest
  { _name :: String,
    _relDir :: Path Rel Dir,
    _file :: Path Rel File
  }

makeLenses ''PosTest

root :: Path Abs Dir
root = relToProject $(mkRelDir "tests/nockma/positive")

testDescr :: PosTest -> TestDescr
testDescr PosTest {..} =
  let tRoot = root <//> _relDir
      file' = tRoot <//> _file
   in TestDescr
        { _testName = _name,
          _testRoot = tRoot,
          _testAssertion = Steps $ \step -> do
            step "Parsing"
            txt <- decodeUtf8 <$> BS.readFile (toFilePath file')
            nockmaTerm <- assertParse txt

            step "Pretty printing"
            let ppTerm = ppPrint nockmaTerm

            step "parse . pretty . parse == parse"
            prettyNockmaTerm <- assertParse ppTerm
            assertEqual "expected equal" nockmaTerm prettyNockmaTerm
        }

assertParse :: Text -> IO (Term Natural)
assertParse txt = case parseText txt of
  Left (MegaparsecError b) -> assertFailure ("Nockma parsing failed " <> unpack (prettyText (errorBundlePretty b)))
  Right t -> return t

allTests :: TestTree
allTests = testGroup "Nockma parse positive" (map (mkTest . testDescr) tests)

tests :: [PosTest]
tests =
  [ PosTest "Identity" $(mkRelDir ".") $(mkRelFile "Identity.nock"),
    PosTest "Identity Pretty" $(mkRelDir ".") $(mkRelFile "IdentityPretty.pnock"),
    PosTest "Stdlib" $(mkRelDir ".") $(mkRelFile "Stdlib.nock")
  ]
