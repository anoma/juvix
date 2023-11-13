module BackendMarkdown
  ( allTests,
  )
where

import BackendMarkdown.Negative qualified as N
import BackendMarkdown.Positive qualified as P
import Base

allTests :: TestTree
allTests = testGroup "BackendMarkdown tests" [P.allTests, N.allTests]
