module Commands.Dev
  ( module Commands.Dev,
    module Commands.Dev.Options,
  )
where

import Commands.Base
import Commands.Dev.Doc qualified as Doc
import Commands.Dev.Highlight qualified as Highlight
import Commands.Dev.Internal.Arity qualified as Arity
import Commands.Dev.Internal.Pretty qualified as InternalPretty
import Commands.Dev.Internal.Typecheck qualified as InternalTypecheck
import Commands.Dev.MiniC qualified as MiniC
import Commands.Dev.Options
import Commands.Dev.Parse qualified as Parse
import Commands.Dev.Scope qualified as Scope
import Commands.Dev.Termination.CallGraph qualified as TerminationCallGraph
import Commands.Dev.Termination.Calls qualified as TerminationCalls

runCommand :: Members '[Embed IO, App] r => EntryPoint -> DevCommand -> Sem r ()
runCommand entryPoint cmd = do
  case cmd of
    Highlight localOpts -> Highlight.runCommand entryPoint localOpts
    Parse localOpts -> Parse.runCommand entryPoint localOpts
    Scope localOpts -> Scope.runCommand entryPoint localOpts
    Doc localOpts -> Doc.runCommand entryPoint localOpts
    Internal i -> case i of
      Pretty -> InternalPretty.runCommand entryPoint
      Arity -> Arity.runCommand entryPoint
      TypeCheck localOpts -> InternalTypecheck.runCommand entryPoint localOpts
    MiniC -> MiniC.runCommand entryPoint
    Termination t -> case t of
      Calls localOpts -> TerminationCalls.runCommand entryPoint localOpts
      CallGraph localOpts -> TerminationCallGraph.runCommand entryPoint localOpts
    _ -> impossible -- do not require entrypoint
