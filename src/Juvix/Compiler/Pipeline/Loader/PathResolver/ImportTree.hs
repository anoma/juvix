module Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error
import Juvix.Compiler.Concrete.Translation.ImportScanner
import Juvix.Compiler.Pipeline.Loader.PathResolver.Base
import Juvix.Compiler.Pipeline.Loader.PathResolver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Prelude

-- | If an entry file is given, it scans imports reachable from that file,
-- otherwise it scans all files in all packages
mkImportTree ::
  forall r.
  ( Members
      '[ Reader ImportScanStrategy,
         Error JuvixError,
         PathResolver,
         Logger,
         Files
       ]
      r
  ) =>
  Maybe (Path Abs File) ->
  Sem r ImportTree
mkImportTree mentrypointModulePath =
  mapError (JuvixError @ParserError)
    . mapError (JuvixError @ScoperError)
    $ do
      pkgInfosTable <- getPackageInfos
      let pkgs :: [PackageInfo] = toList pkgInfosTable
          allNodes :: [ImportNode] = concatMap packageNodes pkgs

          mEntryImportNode :: Maybe ImportNode
          mEntryImportNode = do
            absPath <- mentrypointModulePath
            let cond :: ImportNode -> Bool
                cond n = absPath == n ^. importNodeAbsFile
            find cond allNodes

          startingNodes = maybe allNodes pure mEntryImportNode
      tree <-
        execImportTreeBuilder
          . evalVisitEmpty scanNode
          $ mapM_ visit startingNodes
      checkImportTreeCycles tree
      return tree
  where
    packageNodes :: PackageInfo -> [ImportNode]
    packageNodes pkg =
      [ ImportNode
          { _importNodePackageRoot = pkg ^. packageRoot,
            _importNodeFile = f
          }
        | f <- filter isJuvixOrJuvixMdFile (toList (pkg ^. packageJuvixRelativeFiles))
      ]

    getNodeImports ::
      forall r'.
      (Members '[Reader ImportScanStrategy, Files, Logger, Error ParserError] r') =>
      ImportNode ->
      Sem r' (HashSet ImportScan)
    getNodeImports n = (^. scanResultImports) <$> scanFileImports (n ^. importNodeAbsFile)

    scanNode ::
      forall r'.
      ( Members
          '[ ImportTreeBuilder,
             Reader ImportScanStrategy,
             Error ParserError,
             Files,
             Logger,
             PathResolver,
             Visit ImportNode
           ]
          r'
      ) =>
      ImportNode ->
      Sem r' ()
    scanNode fromNode = withImportNode fromNode $ do
      scans <- toList <$> getNodeImports fromNode
      imports :: [ImportNode] <- mapM resolveImportScan scans
      forM_ (zipExact scans imports) $ \(importscan, toNode) -> do
        importTreeAddEdge importscan toNode
        withResolverRoot (toNode ^. importNodePackageRoot) (visit toNode)

    resolveImportScan :: forall r'. (Members '[PathResolver] r') => ImportScan -> Sem r' ImportNode
    resolveImportScan s = do
      let rel = importScanToRelPath s
      (pkg, ext) <- resolvePath s
      return
        ImportNode
          { _importNodePackageRoot = pkg ^. packageRoot,
            _importNodeFile = addFileExt ext rel
          }

withImportTree ::
  forall r a.
  ( Members
      '[ Reader ImportScanStrategy,
         Error JuvixError,
         PathResolver,
         Logger,
         Files
       ]
      r
  ) =>
  Maybe (Path Abs File) ->
  Sem (Reader ImportTree ': r) a ->
  Sem r a
withImportTree entryModule x = do
  t <- mkImportTree entryModule
  runReader t x

checkImportTreeCycles :: forall r. (Members '[Error ScoperError] r) => ImportTree -> Sem r ()
checkImportTreeCycles tree = do
  let sccs =
        stronglyConnComp
          [ (node, node, toList v) | (node, v) <- HashMap.toList (tree ^. importTree)
          ]
  whenJust (firstJust getCycle sccs) $ \(cyc :: NonEmpty ImportNode) ->
    throw
      . ErrImportCycleNew
      . ImportCycleNew
      $ getEdges cyc
  where
    getEdges :: NonEmpty ImportNode -> NonEmpty ImportScan
    getEdges = fmap (uncurry getEdge) . zipWithNextLoop

    getEdge :: ImportNode -> ImportNode -> ImportScan
    getEdge fromN toN = fromMaybe unexpected $ do
      edges <- tree ^. importTreeEdges . at fromN
      let rel :: Path Rel File = removeExtensions (toN ^. importNodeFile)
          cond :: ImportScan -> Bool
          cond = (== rel) . importScanToRelPath
      find cond edges
      where
        unexpected =
          error $
            "Impossible: Could not find edge between\n"
              <> prettyText fromN
              <> "\nand\n"
              <> prettyText toN
              <> "\n"
              <> "Available Edges:\n"
              <> prettyText (toList (tree ^. importTreeEdges . at fromN . _Just))

    getCycle :: SCC ImportNode -> Maybe (NonEmpty ImportNode)
    getCycle = \case
      AcyclicSCC {} -> Nothing
      CyclicSCC l -> Just (nonEmpty' l)
