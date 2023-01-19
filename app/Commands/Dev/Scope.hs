module Commands.Dev.Scope where

import Commands.Base
import Commands.Dev.Scope.Options
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty qualified as Scoper
import Juvix.Compiler.Concrete.Print qualified as Print
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Data.Comment
import Juvix.Prelude.Pretty

runCommand :: Members '[Embed IO, App] r => ScopeOptions -> Sem r ()
runCommand opts = do
  globalOpts <- askGlobalOptions
  res :: Scoper.ScoperResult <- runPipeline (opts ^. scopeInputFile) upToScoping
  let modules :: NonEmpty (Module 'Scoped 'ModuleTop) = res ^. Scoper.resultModules
  forM_ modules $ \s ->
    if
        | opts ^. scopeWithComments ->
            renderStdOut (Print.ppOut (globalOpts, opts) (res ^. Scoper.comments) s)
        | otherwise ->
            renderStdOut (Scoper.ppOut (globalOpts, opts) s)
  when (opts ^. scopeListComments) $ do
    let mainFile :: Path Abs File = getLoc (res ^. Scoper.mainModule) ^. intervalFile
    newline
    say "Coments:"
    forM_ (fileComments mainFile (res ^. Scoper.comments) ^. fileCommentsSorted) $ \c ->
      say (prettyText (c :: Comment))
