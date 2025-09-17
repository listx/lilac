module Lilac.Compile
  ( cumulativeOffsets,
    findCycles,
    findExternalOids,
    makeRelativeToObjectPath,
    mkObject,
    mkArchive,
    parseCitationStyle,
    readObjectsFrom,
  )
where

import Citeproc qualified as CP
import Citeproc.CslJson (CslJson)
import Data.Aeson (eitherDecodeStrictText)
import Data.Graph qualified as G
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Lilac.Parse qualified as LP
import Lilac.Types
  ( Archive
      ( Archive,
        bibs,
        csls,
        objects,
        offsets,
        offsetsInverted,
        symbolEdges,
        symbolPath,
        symbols
      ),
    BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta, parentOid),
    Cell
      ( CBibliography,
        CBlock,
        CFigure,
        CFootnote,
        CHeading,
        CListItem,
        CMathFragment,
        CParagraph,
        CTable
      ),
    Figure (meta, parentOid),
    GPath,
    Heading (body, meta),
    Link (Link, target),
    LinkOid (LinkOid, name, oid),
    LinkTarget
      ( LinkTargetFilePath,
        LinkTargetFootnote,
        LinkTargetOid,
        LinkTargetURI
      ),
    MathFragment (meta, parentOid),
    OID,
    Object (Object, externalOids, orgDoc, symbols),
    OrgDoc (bibPaths, cells, cslPath, oid, pseudoEdges),
    PToken
      ( PTokenCitation,
        PTokenDivEnd,
        PTokenDivStart,
        PTokenLink,
        PTokenMathFragment,
        PTokenStyledText
      ),
    Prose (unProse),
    PseudoEdge (PseudoEdge),
    Table (grid, meta, parentOid),
    cellIdxToObjCell,
    decodeGPath,
    encodeGPath,
  )
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    FilePath,
    IO,
    Int,
    Map,
    Maybe (Just, Nothing),
    String,
    Text,
    catMaybes,
    concat,
    concatMap,
    elem,
    filter,
    fmap,
    foldl',
    foldlM,
    fromList,
    fst,
    intercalate,
    length,
    map,
    mapAccumL,
    mapM,
    maybeToList,
    mempty,
    otherwise,
    pure,
    repeat,
    show,
    snd,
    sort,
    zip,
    ($),
    (+),
    (.),
    (/=),
    (<$>),
    (<>),
    (=<<),
    (>),
  )
import Relude.Extra.Map (elems, insert, keys, lookup, member, toPairs)
import System.Exit (die)
import System.OsPath qualified as P

mkObject :: OrgDoc -> IO Object
mkObject orgDoc = do
  pure
    Object
      { orgDoc = orgDoc,
        symbols = mkSymbols orgDoc,
        externalOids = findExternalOids orgDoc
      }

mkSymbols :: OrgDoc -> Map LinkOid Int
mkSymbols orgDoc = fromList . concatMap f $ zip [0 ..] orgDoc.cells
  where
    f (localCellIdx, cell) = case cell of
      CBlock block -> getSymbols block.parentOid block.meta localCellIdx
      CHeading heading
        | Just oidText <- lookup "ID" heading.meta,
          Just oid <- LP.validateOidText oidText ->
            [ ( LinkOid
                  { oid = oid,
                    name = T.empty
                  },
                localCellIdx
              )
            ]
        | otherwise -> []
      CFigure figure -> getSymbols figure.parentOid figure.meta localCellIdx
      CMathFragment mathFragment -> getSymbols mathFragment.parentOid mathFragment.meta localCellIdx
      CTable table -> getSymbols table.parentOid table.meta localCellIdx
      CFootnote {}
      CListItem {}
      CParagraph {}
      CBibliography {} -> []
    getSymbols :: OID -> Map Text Text -> Int -> [(LinkOid, Int)]
    getSymbols parentOid cellMeta localCellIdx =
      let oids =
            if parentOid /= orgDoc.oid
              then [parentOid, orgDoc.oid]
              else [orgDoc.oid]
       in concatMap requireName oids
      where
        requireName oid
          | Just name <- lookup "name" cellMeta =
              [ ( LinkOid
                    { oid = oid,
                      name = name
                    },
                  localCellIdx
                )
              ]
          | Just tanglePath <- lookup "tangle" cellMeta =
              [ ( LinkOid
                    { oid = oid,
                      name = tanglePath <> "\NUL"
                    },
                  localCellIdx
                )
              ]
          | otherwise = []

mkArchive :: [(GPath, Object)] -> IO (Either String Archive)
mkArchive pathObjs = do
  let (objects, symbols, symbolPath) =
        foldl' concatObjs (mempty, mempty, mempty) offsetsFull
      symbolEdges = foldl' (finalizeEdges symbols) [] offsetsFull
      brokenLinks = findBrokenLinks objects symbols
  allBibPaths <-
    concat
      <$> mapM (\(gp, o) -> makeRelativeToObjectPath gp o.orgDoc.bibPaths) pathObjs
  allCslPaths <-
    concat
      <$> mapM (\(gp, o) -> makeRelativeToObjectPath gp $ maybeToList o.orgDoc.cslPath) pathObjs
  bibs <- getBibliographies allBibPaths
  cslRawXMLs <- map fst <$> getCitationStyles allCslPaths
  let csls = zip allCslPaths cslRawXMLs
  if
    | brokenLinks /= [] ->
        pure
          . Left
          $ "Found broken links:\n"
          <> intercalate
            "\n"
            (map showBrokenLink brokenLinks)
    | otherwise ->
        pure
          $ Right
            Archive
              { objects = objects,
                symbols = symbols,
                symbolPath = symbolPath,
                symbolEdges = symbolEdges,
                offsets = offsets,
                offsetsInverted = offsetsInverted,
                csls = fromList csls,
                bibs = fromList bibs
              }
  where
    offsets :: IntMap GPath
    offsets =
      IntMap.fromList
        $ map (\(offset, (path, _obj)) -> (offset, path)) offsetsFull
    offsetsFull =
      cumulativeOffsets
        . filter (\(numCells, _) -> numCells > 0)
        . map (\(path, obj) -> (length obj.orgDoc.cells, (path, obj)))
        $ sort pathObjs
    offsetsInverted =
      M.fromList . map (\(offset, path) -> (path, offset)) $ toPairs offsets
    concatObjs ::
      (Map GPath Object, Map LinkOid Int, Map LinkOid GPath) ->
      (Int, (GPath, Object)) ->
      (Map GPath Object, Map LinkOid Int, Map LinkOid GPath)
    concatObjs (accObjs, accSyms, accSymPaths) (offset, (path, obj)) =
      ( insert path obj accObjs,
        (M.map (+ offset) obj.symbols) <> accSyms,
        (fromList $ zip (keys obj.symbols) (repeat path)) <> accSymPaths
      )
    finalizeEdges ::
      Map LinkOid Int ->
      [G.Edge] ->
      (Int, (GPath, Object)) ->
      [G.Edge]
    finalizeEdges syms acc (offset, (_, obj)) =
      let f (PseudoEdge (localCellIdx, linkOid)) = case lookup linkOid syms of
            Nothing -> Nothing
            Just targetCellIdx -> Just (localCellIdx + offset, targetCellIdx)
       in (catMaybes $ map f obj.orgDoc.pseudoEdges) <> acc
    findBrokenLinks ::
      Map GPath Object ->
      Map LinkOid Int ->
      [(GPath, LinkOid)]
    findBrokenLinks objs syms =
      foldl' (getBrokenLink syms) []
        $ allExternalOids objs
    getBrokenLink ::
      Map LinkOid Int ->
      [(GPath, LinkOid)] ->
      (GPath, LinkOid) ->
      [(GPath, LinkOid)]
    getBrokenLink syms acc (gPath, linkOid) = case lookup linkOid syms of
      Just _ -> acc
      Nothing -> (gPath, linkOid) : acc
    allExternalOids :: Map GPath Object -> [(GPath, LinkOid)]
    allExternalOids objs =
      M.foldlWithKey'
        (\acc gPath obj -> (zip (repeat gPath) obj.externalOids) <> acc)
        []
        objs
    showBrokenLink :: (GPath, LinkOid) -> String
    showBrokenLink (gPath, link) =
      "  " <> (T.unpack $ show link) <> " (" <> (decodeGPath gPath) <> ")"

readObjectsFrom :: FilePath -> [FilePath] -> IO [(GPath, Object)]
readObjectsFrom parentDir = foldlM f []
  where
    f :: [(GPath, Object)] -> FilePath -> IO [(GPath, Object)]
    f acc path = do
      parentDirEncoded <- P.encodeUtf parentDir
      p <- P.encodeUtf path
      let relPath = P.combine parentDirEncoded p
      input <- LU.readFileRobustly relPath
      case LP.parseOrgDoc input of
        Left errTxt -> die $ T.unpack errTxt
        Right od -> do
          obj <- mkObject od
          relPath' <- encodeGPath =<< P.decodeUtf relPath
          pure $ (relPath', obj) : acc

buildGraph ::
  [Block] ->
  ( G.Graph,
    G.Vertex -> (Block, LinkOid, [LinkOid]),
    LinkOid -> Maybe G.Vertex
  )
buildGraph blocks = G.graphFromEdges $ foldl' f [] blocks
  where
    f ::
      [(Block, LinkOid, [LinkOid])] ->
      Block ->
      [(Block, LinkOid, [LinkOid])]
    f acc block
      | Just name <- lookup "name" block.meta =
          let selfLink = LinkOid block.parentOid name
           in (\outLinks -> (block, selfLink, outLinks) : acc)
                . catMaybes
                $ map getOutLinks block.body
      | otherwise = acc
    getOutLinks = \case
      BTokenLink (Link _ (LinkTargetOid linkOid)) -> Just linkOid
      BTokenLink {}
      BTokenPadding {}
      BTokenString {} -> Nothing

findCycles :: Archive -> [G.Tree LinkOid]
findCycles archive = map (fmap toLinkOid) . foldl' f [] $ G.scc graph
  where
    blocks = foldl' getBlocksFromAllSymbols [] $ elems archive.symbols
    getBlocksFromAllSymbols acc globalCellIdx
      | Just (_, cell) <- cellIdxToObjCell archive globalCellIdx =
          case cell of
            CBlock b -> b : acc
            CHeading {}
            CListItem {}
            CParagraph {}
            CMathFragment {}
            CFigure {}
            CTable {}
            CFootnote {}
            CBibliography {} -> acc
      | otherwise = acc
    (graph, vertexToNode, _) = buildGraph blocks
    toLinkOid vertex =
      let (_, selfLink, _) = vertexToNode vertex
       in selfLink
    f acc tree@(G.Node vertex _)
      | length tree > 1 = tree : acc
      | (_, selfLink, outLinks) <- vertexToNode vertex,
        elem selfLink outLinks =
          tree : acc
      | otherwise = acc

cumulativeOffsets :: [(Int, a)] -> [(Int, a)]
cumulativeOffsets xs = snd (mapAccumL f 0 xs)
  where
    f :: Int -> (Int, a) -> (Int, (Int, a))
    f currentSum (offset, x) =
      let newSum = currentSum + offset
       in (newSum, (currentSum, x))

findExternalOids :: OrgDoc -> [LinkOid]
findExternalOids orgDoc =
  sort
    . Set.toList
    . fromList
    $ concatMap f orgDoc.cells
  where
    f cell = case cell of
      CBlock block -> foldl' g [] block.body
      CHeading heading -> foldl' h [] heading.body.unProse
      CParagraph prose -> foldl' h [] prose.unProse
      CTable table -> foldl' (\acc prose -> foldl' h acc prose.unProse) [] table.grid
      CFigure {}
      CFootnote {}
      CListItem {}
      CMathFragment {}
      CBibliography {} -> []
    g acc = \case
      BTokenLink link -> getExternal acc link
      BTokenString {}
      BTokenPadding -> acc
    h acc = \case
      PTokenLink link -> getExternal acc link
      PTokenStyledText {}
      PTokenCitation {}
      PTokenMathFragment {}
      PTokenDivStart {}
      PTokenDivEnd {} -> acc
    getExternal acc link = case link.target of
      LinkTargetOid linkOid ->
        if member linkOid symbols
          then acc
          else linkOid : acc
      LinkTargetFootnote {}
      LinkTargetFilePath {}
      LinkTargetURI {} -> acc
    symbols = mkSymbols orgDoc

getCitationStyles :: [FilePath] -> IO [(Text, CP.Style (CslJson Text))]
getCitationStyles filePaths = mapM getCitationStyle filePaths

getCitationStyle :: FilePath -> IO (Text, (CP.Style (CslJson Text)))
getCitationStyle filePath = do
  p <- P.encodeUtf filePath
  input <- LU.readFileRobustly p
  case parseCitationStyle input of
    Left errMsg -> die $ "while parsing " <> filePath <> ":" <> errMsg
    Right style -> pure (input, style)

parseCitationStyle :: Text -> Either String (CP.Style (CslJson Text))
parseCitationStyle input = do
  result <- CP.parseStyle (\_ -> pure mempty) input
  case result of
    Left citeprocError -> Left $ show citeprocError
    Right style -> Right style

getBibliographies :: [FilePath] -> IO [(FilePath, [CP.Reference (CslJson Text)])]
getBibliographies filePaths = do
  bibs <- mapM getBibliography filePaths
  pure $ zip filePaths bibs

getBibliography :: FilePath -> IO [CP.Reference (CslJson Text)]
getBibliography filePath = do
  p <- P.encodeUtf filePath
  input <- LU.readFileRobustly p
  case parseBibliography input of
    Left errMsg -> die $ "while parsing " <> filePath <> ":" <> errMsg
    Right references -> pure references

parseBibliography :: Text -> Either String [CP.Reference (CslJson Text)]
parseBibliography = eitherDecodeStrictText

makeRelativeToObjectPath :: GPath -> [FilePath] -> IO [FilePath]
makeRelativeToObjectPath objGPath fps = do
  let objPath = decodeGPath objGPath
  objOsp <- P.encodeUtf objPath
  let objDir = P.takeDirectory objOsp
  mapM (f objDir) fps
  where
    f :: P.OsPath -> FilePath -> IO FilePath
    f objDir fp = do
      p <- P.encodeUtf fp
      let relPath = P.combine objDir p
      P.decodeUtf relPath
