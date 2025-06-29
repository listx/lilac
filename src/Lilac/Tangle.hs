module Lilac.Tangle
  ( FQBN,
    SourceCode (..),
    expandRootBlocks,
    findCycles,
    getSourceCode,
    getSourceCodeMulti,
    tangle,
  )
where

import Data.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Lilac.Parse (parseOrgDoc)
import Lilac.Types
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta, parentOid),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Link (Link),
    LinkOid (LinkOid),
    LinkTarget (LinkTargetOid),
    OID,
    OrgDoc (cells, oid),
  )
import Lilac.Util qualified as LU
import Relude
  ( Bool (False, True),
    Either (Left, Right),
    FilePath,
    IO,
    Int,
    Maybe (Just, Nothing),
    Type,
    catMaybes,
    elem,
    error,
    filter,
    fmap,
    foldl',
    foldlM,
    fst,
    length,
    map,
    mapM_,
    not,
    otherwise,
    pure,
    putStrLn,
    reverse,
    show,
    sort,
    when,
    ($),
    (&&),
    (+),
    (.),
    (/=),
    (<>),
    (==),
    (>),
    (||),
  )
import Relude.Extra.Map qualified as EM
import Relude.Nub
  ( hashNub,
  )
import System.Directory.OsPath qualified as D
import System.IO (utf8)
import System.OsPath qualified as P

type SourceCode :: Type
data SourceCode = SourceCode
  { roots :: [FQBN],
    fqbnToCodeBlock :: M.Map FQBN Block
  }

type FQBN :: Type
type FQBN = T.Text

defaultSourceCode :: SourceCode
defaultSourceCode =
  SourceCode
    { roots = [],
      fqbnToCodeBlock = M.fromList []
    }

getSourceCode :: OrgDoc -> FilePath -> SourceCode
getSourceCode od odPath = foldl' f defaultSourceCode od.cells
  where
    f :: SourceCode -> Cell -> SourceCode
    f sc cell = case cell of
      CBlock b
        | Just name <- EM.lookup "name" b.meta,
          Just path <- EM.lookup "tangle" b.meta,
          Right filePath <- qualify path ->
            let fqbn1 = getFqbnByDocOID name
                fqbn2 = getFqbn b name
                newBlock1 = useQualifiedTanglePath filePath $ injectFqbn fqbn1 b
                newBlock2 = useQualifiedTanglePath filePath $ injectFqbn fqbn2 b
             in sc
                  { roots = fqbn1 : sc.roots,
                    fqbnToCodeBlock =
                      M.insert fqbn1 newBlock1
                        $ M.insert fqbn2 newBlock2 sc.fqbnToCodeBlock
                  }
        | Just path <- EM.lookup "tangle" b.meta,
          Right filePath <- qualify path ->
            let name = (T.pack filePath) <> "\NUL"
                fqbn1 = getFqbnByDocOID name
                fqbn2 = getFqbn b name
                newBlock1 = useQualifiedTanglePath filePath $ injectFqbn fqbn1 b
                newBlock2 = useQualifiedTanglePath filePath $ injectFqbn fqbn2 b
             in sc
                  { roots = fqbn1 : sc.roots,
                    fqbnToCodeBlock =
                      M.insert fqbn1 newBlock1
                        $ M.insert fqbn2 newBlock2 sc.fqbnToCodeBlock
                  }
        | Just name <- EM.lookup "name" b.meta ->
            let fqbn1 = getFqbnByDocOID name
                fqbn2 = getFqbn b name
                newBlock1 = injectFqbn fqbn1 b
                newBlock2 = injectFqbn fqbn2 b
             in sc
                  { fqbnToCodeBlock =
                      M.insert fqbn1 newBlock1
                        $ M.insert fqbn2 newBlock2 sc.fqbnToCodeBlock
                  }
        | otherwise -> sc
      CHeading {}
      CListItem {}
      CParagraph {}
      CMathFragment {}
      CFigure {}
      CTable {} -> sc
    getFqbn :: Block -> T.Text -> T.Text
    getFqbn b name = LU.mkFqcn b.parentOid name
    getFqbnByDocOID name = LU.mkFqcn od.oid name
    injectFqbn fqbn block =
      block
        { meta = M.insert "__fqbn" fqbn block.meta
        }
    tanglePathPrefix = P.takeDirectory $ P.unsafeEncodeUtf odPath
    qualify =
      P.decodeWith utf8 utf8
        . P.combine tanglePathPrefix
        . P.unsafeEncodeUtf
        . T.unpack
    useQualifiedTanglePath qualifiedFilePath block =
      block
        { meta = M.insert "tangle" (T.pack qualifiedFilePath) block.meta,
          body = block.body
        }

growBlockUniverse ::
  Set.Set OID ->
  Set.Set FQBN ->
  M.Map OID P.OsPath ->
  M.Map FQBN Block ->
  [FQBN] ->
  IO (M.Map FQBN Block)
growBlockUniverse _ _ _ blockUniverse [] = pure blockUniverse
growBlockUniverse parsedOids seenFqbns goids blockUniverse (root : roots)
  | Just block <- EM.lookup root blockUniverse = do
      let nowebLinks = catMaybes $ map getLinkTarget block.body
          fqbns =
            map
              (\(LinkOid oid targetName) -> LU.mkFqcn oid targetName)
              nowebLinks
          newFqbns = filter (\fqbn -> not $ Set.member fqbn seenFqbns) fqbns
      (newlyParsedOids, grownBlockUniverse, toGrowFrom) <-
        foldlM f (parsedOids, blockUniverse, []) nowebLinks
      growBlockUniverse
        newlyParsedOids
        (Set.union seenFqbns $ Set.fromList newFqbns)
        goids
        grownBlockUniverse
        (roots <> newFqbns <> toGrowFrom)
  | otherwise = growBlockUniverse parsedOids seenFqbns goids blockUniverse roots
  where
    f ::
      (Set.Set OID, M.Map FQBN Block, [FQBN]) ->
      LinkOid ->
      IO (Set.Set OID, M.Map FQBN Block, [FQBN])
    f (pu, bu, tgf) (LinkOid oid targetName)
      | Set.member (LU.mkFqcn oid targetName) seenFqbns = pure (pu, bu, tgf)
      | Set.member oid pu = pure (pu, bu, (LU.mkFqcn oid targetName) : tgf)
      | Just oidOsPath <- EM.lookup oid goids = do
          input <- LU.readFileRobustly oidOsPath
          case parseOrgDoc input of
            Right od -> do
              oidFilePath <- P.decodeUtf oidOsPath
              let sc = getSourceCode od oidFilePath
              pure
                ( Set.insert oid pu,
                  M.union bu sc.fqbnToCodeBlock,
                  (LU.mkFqcn oid targetName) : tgf
                )
            Left err -> do
              putStrLn $ show err
              pure (pu, bu, tgf)
      | otherwise = pure (pu, bu, tgf)
    getLinkTarget = \case
      (BTokenLink (Link _ (LinkTargetOid linkOid))) ->
        Just linkOid
      BTokenLink {}
      BTokenPadding {}
      BTokenString {} -> Nothing

getSourceCodeMulti ::
  OrgDoc ->
  FilePath ->
  M.Map OID P.OsPath ->
  IO SourceCode
getSourceCodeMulti od odPath goids = do
  let sc = getSourceCode od odPath
  bu <- growBlockUniverse Set.empty Set.empty goids sc.fqbnToCodeBlock sc.roots
  pure
    $ sc
      { roots = sc.roots,
        fqbnToCodeBlock = bu
      }

buildGraph ::
  [Block] ->
  ( G.Graph,
    G.Vertex -> (Block, FQBN, [FQBN]),
    FQBN -> Maybe G.Vertex
  )
buildGraph blocks = G.graphFromEdges $ foldl' f [] blocks
  where
    f ::
      [(Block, FQBN, [FQBN])] ->
      Block ->
      [(Block, FQBN, [FQBN])]
    f acc block
      | Just fqbn <- EM.lookup "__fqbn" block.meta =
          (\targets -> (block, fqbn, targets) : acc)
            . hashNub
            . catMaybes
            $ map getLinkTarget block.body
      | otherwise = acc
    getLinkTarget = \case
      BTokenLink (Link _ (LinkTargetOid (LinkOid oid targetName))) ->
        Just $ LU.mkFqcn oid targetName
      BTokenLink {}
      BTokenPadding {}
      BTokenString {} -> Nothing

findCycles :: SourceCode -> [G.Tree FQBN]
findCycles sc = map (fmap toFqbn) . foldl' f [] $ G.scc graph
  where
    (graph, vertexToNode, _) = buildGraph $ M.elems sc.fqbnToCodeBlock
    toFqbn vertex =
      let (_, blockName, _) = vertexToNode vertex
       in blockName
    f acc tree@(G.Node vertex _)
      | length tree > 1 = tree : acc
      | (_, name, targets) <- vertexToNode vertex,
        elem name targets =
          tree : acc
      | otherwise = acc

expandRootBlocks :: SourceCode -> [(P.OsPath, T.Text)]
expandRootBlocks sc = catMaybes $ map expandRootBlock sc.roots
  where
    expandRootBlock :: T.Text -> Maybe (P.OsPath, T.Text)
    expandRootBlock rootFqbn
      | Just rootBlock <- EM.lookup rootFqbn sc.fqbnToCodeBlock =
          Just
            ( tanglePathOf rootBlock,
              maybeAddShebang rootBlock
                . trimTrailingSpaces
                $ expandBlock sc rootBlock ""
            )
      | otherwise = Nothing
    trimTrailingSpaces :: T.Text -> T.Text
    trimTrailingSpaces text =
      T.unlines
        . map (T.dropWhileEnd (== ' '))
        $ T.lines text
    tanglePathOf :: Block -> P.OsPath
    tanglePathOf block
      | Just path <- EM.lookup "tangle" block.meta = P.unsafeEncodeUtf $ T.unpack path
      | otherwise = error "programmer error"
    maybeAddShebang :: Block -> T.Text -> T.Text
    maybeAddShebang rootBlock text
      | Just shebang <- EM.lookup "shebang" rootBlock.meta =
          shebang <> "\n" <> text
      | otherwise = text

expandBlock :: SourceCode -> Block -> T.Text -> T.Text
expandBlock sc block existingPrefix =
  let acc = "" :: T.Text
      prefix = "" :: T.Text
   in fst $ foldl' expand (acc, prefix) block.body
  where
    expand (acc, _) BTokenPadding =
      (acc <> "\n", "")
    expand (acc, _) (BTokenString text) =
      (acc <> prependExistingPrefix text, getPrefix text)
    expand (acc, prefix) (BTokenLink (Link _ (LinkTargetOid (LinkOid oid target))))
      | Just targetBlock <-
          EM.lookup
            (LU.mkFqcn oid target)
            sc.fqbnToCodeBlock =
          ( acc <> (expandBlock sc targetBlock $ existingPrefix <> prefix),
            prefix
          )
      | otherwise = (acc, prefix)
    expand (acc, prefix) _ = (acc, prefix)
    getPrefix text = T.takeWhileEnd (/= '\n') text
    prependExistingPrefix text =
      T.intercalate "\n"
        . reverse
        . fst
        . foldl' prefixTailLines ([], True)
        $ T.split (== '\n') text
    prefixTailLines (acc, isFirstLine) line
      | isFirstLine = (line : acc, False)
      | otherwise = (T.append existingPrefix line : acc, False)

tangle :: SourceCode -> IO ()
tangle sc = do
  let blocks = sort $ expandRootBlocks sc
  putStrLn $ "Blocks to process: " <> (show $ length blocks)
  (tangled, tangledPaths, skipped) <- foldlM f (0, [], 0) blocks
  putStrLn $ "Tangled: " <> (show tangled)
  when (tangled > 0) $ mapM_ printTangled $ reverse tangledPaths
  putStrLn $ "Skipped: " <> (show skipped)
  where
    f :: (Int, [P.OsPath], Int) -> (P.OsPath, T.Text) -> IO (Int, [P.OsPath], Int)
    f (tangled, tangledPaths, skipped) (path, contentToWrite) = do
      alreadyExists <- D.doesPathExist path
      existingContent <- LU.readFileRobustly path
      let g
            | (alreadyExists && existingContent /= contentToWrite)
                || not alreadyExists = do
                LU.writeFileRobustly path contentToWrite
                pure (tangled + 1, path : tangledPaths, skipped)
            | otherwise = pure (tangled, tangledPaths, skipped + 1)
       in g
    printTangled :: P.OsPath -> IO ()
    printTangled path = putStrLn $ "  " <> show path
