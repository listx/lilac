module Lilac.Compile
  ( cumulativeOffsets,
    findCycles,
    findExternalOids,
    gciToName,
    makeRelativeToObjectPath,
    mkArchive,
    mkGTOC,
    mkObject,
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
import Data.List (partition)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tree
  ( Tree (Node),
    unfoldTreeM,
  )
import Lilac.Parse qualified as LP
import Lilac.Types
  ( Archive
      ( Archive,
        ancestry,
        backlinks,
        bibs,
        csls,
        glossary,
        gtoc,
        objects,
        offsets,
        oidAliases,
        preciseBacklinks,
        symbols
      ),
    BToken (BTokenLinkTarget, BTokenString),
    Block (body, meta, parentOid),
    Cell
      ( CBlock,
        CCommand,
        CFigure,
        CFootnote,
        CGlossaryTerm,
        CHeading,
        CListItem,
        CMathFragment,
        CParagraph,
        CRichBlock,
        CTable
      ),
    Figure (caption, link, meta, parentOid),
    Footnote (body, name),
    GCI (GCI, unGCI),
    GPath,
    GTOC (GTOC),
    GlossaryTerm (definition, term),
    Heading (body, meta),
    LCI (LCI, unLCI),
    Link (Link, target),
    LinkOid (LinkOid, name, oid),
    LinkTarget
      ( LinkTargetFilePath,
        LinkTargetLineLabel,
        LinkTargetOid
      ),
    MathFragment (meta, parentOid),
    OID,
    Object (Object, externalOids, orgDoc, symbols),
    OrgDoc (bibPaths, cells, cslPath, glossary, headingOids, oid, pseudoEdges),
    PToken
      ( PTokenLink
      ),
    ProjectConf (bibliography, citationStyle),
    Prose (Prose, unProse),
    PseudoEdge (PseudoEdge),
    Style (Verbatim),
    Table (caption, grid, meta, parentOid),
    decodeGPath,
    emptyGPath,
    encodeGPath,
    gciToObjCell,
    getProseContents,
    getSectionNumber,
    getTangledBlocks,
    ptext,
    stripLinks,
  )
import Lilac.Util qualified as LU
import Relude
  ( Bool,
    Either (Left, Right),
    FilePath,
    IO,
    Int,
    Map,
    Maybe (Just, Nothing),
    State,
    String,
    Text,
    Type,
    abs,
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
    get,
    hashNub,
    intercalate,
    length,
    map,
    mapAccumL,
    mapM,
    mapMaybe,
    maybeToList,
    mempty,
    modify,
    not,
    null,
    otherwise,
    pure,
    repeat,
    runState,
    show,
    snd,
    sort,
    unlines,
    when,
    zip,
    (!!?),
    ($),
    (+),
    (-),
    (.),
    (/=),
    (<),
    (<$>),
    (<>),
    (=<<),
    (==),
    (>),
  )
import Relude.Extra.Map (elems, insert, insertWith, lookup, lookupDefault, member)
import System.Directory.OsPath qualified as D
import System.Exit (die)
import System.FilePath (splitDirectories)
import System.OsPath qualified as P

mkObject :: OrgDoc -> Object
mkObject orgDoc =
  Object
    { orgDoc = orgDoc,
      symbols = mkSymbols orgDoc, --
      externalOids = findExternalOids orgDoc --
    }

mkSymbols :: OrgDoc -> Map LinkOid LCI
mkSymbols orgDoc =
  fromList
    . concatMap f
    $ zip
      (map LCI [0 ..]) --
      orgDoc.cells
  where
    f (lci, cell) = case cell of --
      CHeading heading
        | Just oidText <- lookup "ID" heading.meta,
          Just oid <- LP.validateOidText oidText ->
            [ ( LinkOid
                  { oid = oid,
                    name = T.empty
                  },
                lci
              )
            ]
        | otherwise -> []
      CBlock block ->
        ( getSymbols
            orgDoc.oid
            block.parentOid
            block.meta
            lci
        )
          <> (getLineLabels orgDoc.oid block.parentOid lci block) --
      CFigure figure ->
        getSymbols
          orgDoc.oid
          figure.parentOid
          figure.meta
          lci
      CMathFragment mathFragment ->
        getSymbols
          orgDoc.oid
          mathFragment.parentOid
          mathFragment.meta
          lci
      CTable table ->
        getSymbols
          orgDoc.oid
          table.parentOid
          table.meta
          lci
      _ -> [] --

getLineLabels :: OID -> OID -> LCI -> Block -> [(LinkOid, LCI)]
getLineLabels docOid parentOid lci block = foldl' f [] block.body
  where
    f acc bToken = case bToken of
      (BTokenLinkTarget (LinkTargetLineLabel linkOid)) ->
        (LinkOid docOid linkOid.name, lci)
          : (LinkOid parentOid linkOid.name, lci)
          : acc
      _ -> acc

getSymbols :: OID -> OID -> Map Text Text -> LCI -> [(LinkOid, LCI)]
getSymbols docOid parentOid cellMeta lci =
  let oids =
        if parentOid /= docOid --
          then [parentOid, docOid]
          else [docOid]
   in mapMaybe extractName oids
  where
    extractName oid --
      | Just name <- lookup "name" cellMeta =
          Just
            ( LinkOid
                { oid = oid,
                  name = name
                },
              lci
            )
      | Just tanglePath <- lookup "tangle" cellMeta --
        =
          Just
            ( LinkOid
                { oid = oid,
                  name = tanglePath <> "\NUL"
                },
              lci
            )
      | Just anonBlockNumber <- lookup "__anon_block_number" cellMeta --
        =
          Just
            ( LinkOid
                { oid = oid,
                  name = anonBlockNumber <> "\NUL__anon_block_number"
                },
              lci
            )
      | otherwise = Nothing

mkArchive :: ProjectConf -> [(GPath, Object)] -> IO (Either String Archive)
mkArchive projectConf pathObjs = do
  let pathObjsNonEmpty =
        filter (\(_, obj) -> length obj.orgDoc.cells > 0) pathObjs --
      (symbols, glossary) = combineSymbolsAndGlossary pathObjsNonEmpty
      offsets = mkOffsets pathObjsNonEmpty
      backlinks = mkBacklinks pathObjsNonEmpty symbols offsets
      preciseBacklinks = mkPreciseBacklinks pathObjsNonEmpty oidAliases
      oidAliases = mkOidAliases pathObjsNonEmpty
      brokenLinks = findBrokenLinks pathObjsNonEmpty symbols --
      duplicateTanglePaths = findDuplicateTanglePaths pathObjsNonEmpty --
  brokenFileLinks <- findBrokenFileLinks pathObjs
  (csls, bibs) <- getCitationDeps projectConf pathObjsNonEmpty
  if
    | brokenLinks /= [] ->
        pure
          . Left
          $ "Found broken links:\n"
          <> intercalate
            "\n"
            (map showBrokenLink brokenLinks)
    | brokenFileLinks /= [] ->
        pure
          . Left
          $ "Found broken file paths (from \"[[file:...]]\" links):\n"
          <> intercalate
            "\n"
            (map showBrokenFileLink brokenFileLinks)
    | duplicateTanglePaths /= [] ->
        pure
          . Left
          $ "Found duplicate tangle paths:\n"
          <> intercalate
            "\n"
            (map showDuplicateTanglePaths duplicateTanglePaths)
    | otherwise ->
        case mkGTOC pathObjs of
          Left err -> pure $ Left err
          Right gtoc -> do
            let archive =
                  Archive
                    { objects = fromList pathObjs,
                      symbols = symbols,
                      ancestry = mempty,
                      glossary = glossary,
                      backlinks = backlinks,
                      preciseBacklinks = preciseBacklinks,
                      oidAliases = oidAliases,
                      offsets = offsets,
                      gtoc = gtoc,
                      csls = csls,
                      bibs = bibs
                    }
                ancestry =
                  mkBlockAncestry archive
                    $ getBlockGCIsFromArchive archive --
            pure $ Right archive {ancestry = ancestry}
  where
    showBrokenLink :: (GPath, LinkOid) -> String
    showBrokenLink (gPath, link) =
      "  " <> (T.unpack $ show link) <> " (" <> (decodeGPath gPath) <> ")"
    showBrokenFileLink :: (GPath, FilePath) -> String
    showBrokenFileLink (gPath, filePath) =
      "  " <> (filePath) <> " (" <> (decodeGPath gPath) <> ")"
    showDuplicateTanglePaths :: (Text, [GPath]) -> String
    showDuplicateTanglePaths (tanglePath, gPaths) =
      "  " <> (T.unpack tanglePath) <> " (" <> (show $ map decodeGPath gPaths) <> ")"

combineSymbolsAndGlossary ::
  [(GPath, Object)] ->
  (Map LinkOid GCI, Map (Text, GPath) (LCI, LCI))
combineSymbolsAndGlossary pathObjs =
  foldl' f (docOidsToSymbols pathObjs, mempty) $ prependOffsets pathObjs
  where
    f ::
      (Map LinkOid GCI, Map (Text, GPath) (LCI, LCI)) ->
      (Int, (GPath, Object)) ->
      (Map LinkOid GCI, Map (Text, GPath) (LCI, LCI))
    f (accSyms, accGloss) (offset, (path, obj)) =
      ( (M.map (\lci -> GCI $ lci.unLCI + offset) obj.symbols) --
          <> accSyms,
        (M.mapKeys (\term -> (term, path)) obj.orgDoc.glossary) <> accGloss
      )

docOidsToSymbols :: [(GPath, Object)] -> Map LinkOid GCI
docOidsToSymbols pathObjs =
  let topLevelLinkOids =
        fromList
          $ zip
            (map (objToLinkOid . snd) $ sort pathObjs)
            (map GCI [-1, -2 ..])
      objToLinkOid :: Object -> LinkOid
      objToLinkOid obj =
        LinkOid
          { oid = obj.orgDoc.oid,
            name = ""
          }
   in topLevelLinkOids

mkOffsets :: [(GPath, Object)] -> IntMap GPath
mkOffsets pathObjs =
  IntMap.fromList
    . map (\(offset, (path, _obj)) -> (offset, path))
    $ prependOffsets pathObjs

prependOffsets :: [(GPath, Object)] -> [(Int, (GPath, Object))]
prependOffsets pathObjs =
  cumulativeOffsets
    . map (\(path, obj) -> (length obj.orgDoc.cells, (path, obj)))
    $ sort pathObjs

findBrokenLinks ::
  [(GPath, Object)] ->
  Map LinkOid GCI ->
  [(GPath, LinkOid)]
findBrokenLinks pathObjs symbols =
  foldl' getBrokenLink [] allExternalOids
  where
    getBrokenLink ::
      [(GPath, LinkOid)] ->
      (GPath, LinkOid) ->
      [(GPath, LinkOid)]
    getBrokenLink acc (gPath, linkOid) = case lookup linkOid symbols of
      Just _ -> acc
      Nothing -> (gPath, linkOid) : acc --
    allExternalOids :: [(GPath, LinkOid)]
    allExternalOids =
      foldl'
        (\acc (gPath, obj) -> (zip (repeat gPath) obj.externalOids) <> acc)
        []
        pathObjs

findDuplicateTanglePaths :: [(GPath, Object)] -> [(Text, [GPath])]
findDuplicateTanglePaths pathObjs =
  [ (tanglePath, gPaths)
  | (tanglePath, gPaths) <- M.toList groupedByTanglePath,
    length gPaths > 1
  ]
  where
    groupedByTanglePath :: Map Text [GPath]
    groupedByTanglePath =
      M.fromListWith
        (<>)
        [ (tanglePath, [gPath])
        | (gPath, obj) <- pathObjs,
          tanglePath <- map fst $ getTangledBlocks obj.orgDoc.cells
        ]

findBrokenFileLinks ::
  [(GPath, Object)] ->
  IO [(GPath, FilePath)]
findBrokenFileLinks =
  foldlM validateFilePath [] . allFilePaths
  where
    validateFilePath ::
      [(GPath, FilePath)] ->
      (GPath, FilePath) ->
      IO [(GPath, FilePath)]
    validateFilePath acc (objPath, filePath) = do
      objOsPath <- P.encodeUtf $ decodeGPath objPath
      let objDirOsPath = P.takeDirectory objOsPath
      osPath <- P.encodeUtf filePath
      exists <- D.doesPathExist $ P.combine objDirOsPath osPath
      if exists
        then pure acc
        else pure $ (objPath, filePath) : acc
    allFilePaths :: [(GPath, Object)] -> [(GPath, FilePath)]
    allFilePaths pathObjs =
      foldl'
        ( \acc (gPath, obj) ->
            let result =
                  zip
                    (repeat gPath)
                    (concatMap extractFilesFromCell obj.orgDoc.cells)
             in acc <> result
        )
        []
        pathObjs
    extractFilesFromCell :: Cell -> [FilePath]
    extractFilesFromCell = \case
      CHeading heading -> extractFilesFromProse heading.body
      CParagraph prose -> extractFilesFromProse prose
      CGlossaryTerm glossaryTerm ->
        concatMap
          extractFilesFromProse
          [ glossaryTerm.term,
            glossaryTerm.definition
          ]
      CFigure figure ->
        extractFilesFromProse
          figure.caption
          <> (maybeToList $ extractFileFromLink figure.link)
      CTable table ->
        extractFilesFromProse
          table.caption
          <> (concatMap extractFilesFromProse $ elems table.grid)
      CFootnote footnote -> extractFilesFromProse footnote.body
      _ -> []
    extractFilesFromProse :: Prose -> [FilePath]
    extractFilesFromProse prose = mapMaybe extractFileFromPToken prose.unProse
    extractFileFromPToken :: PToken -> Maybe FilePath
    extractFileFromPToken = \case
      PTokenLink link _ -> extractFileFromLink link
      _ -> Nothing
    extractFileFromLink :: Link -> Maybe FilePath
    extractFileFromLink (Link _ (LinkTargetFilePath filePath)) = Just filePath
    extractFileFromLink _ = Nothing

getCitationDeps ::
  ProjectConf ->
  [(GPath, Object)] ->
  IO (Map FilePath Text, Map FilePath [CP.Reference (CslJson Text)])
getCitationDeps projectConf pathObjs = do
  csls <- getCitationStyles projectConf pathObjs
  bibs <- getBibliographies projectConf pathObjs
  pure (csls, bibs)

finalizeEdges ::
  Map LinkOid GCI ->
  [(GCI, GCI)] ->
  (Int, (GPath, Object)) ->
  [(GCI, GCI)]
finalizeEdges symbols acc (offset, (_, obj)) =
  let f (PseudoEdge (lci, linkOid)) = case lookup linkOid symbols of --
        Nothing -> Nothing
        Just targetGCI -> Just (GCI $ lci.unLCI + offset, targetGCI) --
   in (catMaybes $ map f obj.orgDoc.pseudoEdges) <> acc

mkBacklinks ::
  [(GPath, Object)] ->
  Map LinkOid GCI ->
  IntMap GPath ->
  Map GPath (Map GPath [(GCI, GCI)])
mkBacklinks pathObjs symbols offsets =
  M.map (M.fromListWith (<>)) $ foldl' f mempty symbolEdges
  where
    symbolEdges :: [(GCI, GCI)]
    symbolEdges =
      foldl'
        (finalizeEdges symbols) --
        []
        $ prependOffsets pathObjs
    sortedPaths :: [GPath]
    sortedPaths = sort $ map fst pathObjs
    f acc (from, to)
      | Just (_, fromPath) <- IntMap.lookupLE from.unGCI offsets,
        Just (_, toPath) <- IntMap.lookupLE to.unGCI offsets,
        fromPath /= toPath =
          M.insertWith (<>) toPath [(fromPath, [(from, to)])] acc
      | Just (_, fromPath) <- IntMap.lookupLE from.unGCI offsets,
        to.unGCI < 0,
        Just toPath <- sortedPaths !!? (abs to.unGCI - 1),
        fromPath /= toPath =
          M.insertWith (<>) toPath [(fromPath, [(from, to)])] acc
      | otherwise = acc

mkPreciseBacklinks ::
  [(GPath, Object)] ->
  Map OID OID ->
  Map LinkTarget [(Maybe GCI, GCI, Int)]
mkPreciseBacklinks pathObjs oidAliases =
  fst . foldl' f (mempty, 0) $ sort pathObjs
  where
    f ::
      (Map LinkTarget [(Maybe GCI, GCI, Int)], Int) ->
      (GPath, Object) ->
      (Map LinkTarget [(Maybe GCI, GCI, Int)], Int)
    f (acc, gciNum) (_, obj) =
      let (_, finalAcc) =
            foldl' g (Nothing, acc) $ zip [gciNum ..] obj.orgDoc.cells
       in (finalAcc, gciNum + length obj.orgDoc.cells)
    g ::
      (Maybe GCI, Map LinkTarget [(Maybe GCI, GCI, Int)]) ->
      (Int, Cell) ->
      (Maybe GCI, Map LinkTarget [(Maybe GCI, GCI, Int)])
    g (maybeHeadingGci, acc) (gciNum, cell) = case cell of
      CHeading heading ->
        let newHeading = Just (GCI gciNum)
            newAcc =
              foldl'
                (processLinkFromPToken newHeading (GCI gciNum))
                acc
                heading.body.unProse
         in (newHeading, newAcc)
      _ ->
        let h = processLinkFromPToken maybeHeadingGci (GCI gciNum)
            newAcc = case cell of
              CParagraph prose ->
                foldl' h acc prose.unProse
              CGlossaryTerm glossaryTerm ->
                foldl' h acc glossaryTerm.definition.unProse
              CTable table ->
                let gridAcc =
                      foldl'
                        (\a prose -> foldl' h a prose.unProse)
                        acc
                        table.grid
                 in foldl' h gridAcc table.caption.unProse
              CFigure figure ->
                foldl' h acc figure.caption.unProse
              CFootnote footnote ->
                foldl' h acc footnote.body.unProse
              _ -> acc
         in (maybeHeadingGci, newAcc)
    processLinkFromPToken ::
      Maybe GCI ->
      GCI ->
      Map LinkTarget [(Maybe GCI, GCI, Int)] ->
      PToken ->
      Map LinkTarget [(Maybe GCI, GCI, Int)]
    processLinkFromPToken activeHeading cellGci acc pToken = case pToken of
      PTokenLink link idx
        | idx > 0 ->
            let resolvedLinkOid (LinkOid oid name) =
                  let resolvedOid = lookupDefault oid oid oidAliases
                   in LinkOid resolvedOid name
                resolvedLinkTarget = case link.target of
                  LinkTargetOid linkOid ->
                    LinkTargetOid $ resolvedLinkOid linkOid
                  LinkTargetLineLabel linkOid ->
                    LinkTargetLineLabel $ resolvedLinkOid linkOid
                  _ -> link.target
             in insertWith
                  (\old new -> old <> new)
                  resolvedLinkTarget
                  [(activeHeading, cellGci, idx)]
                  acc
        | otherwise -> acc
      _ -> acc

mkOidAliases :: [(GPath, Object)] -> Map OID OID
mkOidAliases pathObjs =
  foldl' f mempty pathObjs
  where
    f ::
      Map OID OID ->
      (GPath, Object) ->
      Map OID OID
    f acc (_, obj) =
      foldl' g acc obj.orgDoc.headingOids
      where
        g ::
          Map OID OID ->
          OID ->
          Map OID OID
        g acc' headingOid = insert headingOid obj.orgDoc.oid acc'

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
          relPath' <- encodeGPath =<< P.decodeUtf relPath
          pure $ (relPath', mkObject od) : acc

buildGraph ::
  [(OID, Block)] -> --
  ( G.Graph,
    G.Vertex -> (Block, LinkOid, [LinkOid]),
    LinkOid -> Maybe G.Vertex
  )
buildGraph = G.graphFromEdges . foldl' f []
  where
    f ::
      [(Block, LinkOid, [LinkOid])] ->
      (OID, Block) ->
      [(Block, LinkOid, [LinkOid])]
    f acc (docOid, block)
      | Just name <- lookup "name" block.meta =
          let selfLinkByParentOid = LinkOid block.parentOid name
              selfLinkByDocOid = LinkOid docOid name
              addVertices outLinks =
                if block.parentOid == docOid
                  then (block, selfLinkByDocOid, outLinks) : acc
                  else
                    (block, selfLinkByParentOid, outLinks) --
                      : (block, selfLinkByDocOid, outLinks)
                      : acc
           in addVertices . catMaybes $ map getOutLinks block.body
      | otherwise = acc
    getOutLinks = \case
      BTokenLinkTarget (LinkTargetOid linkOid) -> Just linkOid
      _ -> Nothing

getBlocksFromArchive :: Archive -> [(OID, Block)]
getBlocksFromArchive archive = foldl' f [] $ elems archive.symbols
  where
    f :: [(OID, Block)] -> GCI -> [(OID, Block)]
    f acc gci
      | Just (obj, cell) <- gciToObjCell archive gci =
          case cell of
            CBlock b -> (obj.orgDoc.oid, b) : acc
            _ -> acc
      | otherwise = acc

getBlockGCIsFromArchive :: Archive -> [GCI]
getBlockGCIsFromArchive archive = foldl' f [] $ elems archive.symbols
  where
    f :: [GCI] -> GCI -> [GCI]
    f acc gci
      | Just (_, cell) <- gciToObjCell archive gci =
          case cell of
            CBlock {} -> gci : acc
            _ -> acc
      | otherwise = acc

mkBlockAncestry :: Archive -> [GCI] -> Map GCI [GCI]
mkBlockAncestry archive blockGCIs = foldl' f mempty blockGCIs
  where
    f :: Map GCI [GCI] -> GCI -> Map GCI [GCI]
    f acc blockGCI
      | Just (_, CBlock block) <- gciToObjCell archive blockGCI =
          ( \childLinks ->
              foldl'
                ( \acc' childLink ->
                    case lookup childLink archive.symbols of
                      Just childGCI ->
                        insertWith
                          (\old new -> hashNub (old <> new))
                          childGCI
                          [blockGCI]
                          acc'
                      Nothing -> acc'
                )
                acc
                childLinks
          )
            . catMaybes
            $ map getChildLinks block.body
      | otherwise = acc
    getChildLinks = \case
      BTokenLinkTarget (LinkTargetOid linkOid) -> Just linkOid
      _ -> Nothing

findCycles :: Archive -> [G.Tree LinkOid]
findCycles archive = map (fmap toLinkOid) . foldl' f [] $ G.scc graph
  where
    (graph, vertexToNode, _) = buildGraph $ getBlocksFromArchive archive
    toLinkOid vertex =
      let (_, selfLink, _) = vertexToNode vertex
       in selfLink
    f acc tree@(G.Node vertex _) --
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
    $ concatMap searchInsideProse orgDoc.cells
  where
    searchInsideProse cell = case cell of --
      CBlock block -> foldl' getLinkFromBToken [] block.body
      CHeading heading -> foldl' getLinkFromPToken [] heading.body.unProse
      CParagraph prose -> foldl' getLinkFromPToken [] prose.unProse
      CGlossaryTerm glossaryTerm ->
        foldl' getLinkFromPToken [] glossaryTerm.definition.unProse
      CTable table ->
        foldl'
          (\acc prose -> foldl' getLinkFromPToken acc prose.unProse)
          []
          table.grid
      _ -> []
    getLinkFromBToken acc bToken = case bToken of --
      BTokenLinkTarget linkTarget -> keepIfExternal acc linkTarget
      _ -> acc
    getLinkFromPToken acc pToken = case pToken of --
      PTokenLink link _ -> keepIfExternal acc link.target
      _ -> acc
    symbols = mkSymbols orgDoc --
    keepIfExternal acc linkTarget = case linkTarget of --
      LinkTargetOid linkOid ->
        if member linkOid symbols
          then acc
          else linkOid : acc
      LinkTargetLineLabel linkOid ->
        if member linkOid symbols
          then acc
          else linkOid : acc
      _ -> acc

type PathState :: Type
data PathState = PathState
  { objMap :: Map GPath Object, --
    invalidPaths :: [GPath] --
  }

mkGTOC :: [(GPath, Object)] -> Either String GTOC
mkGTOC pathObjs
  | Just pathObj <- findRootIndex pathObjs =
      let objMap = fromList pathObjs
          invalidPaths = [] :: [GPath]
          initialState = PathState objMap invalidPaths
          (gtoc, finalState) = runState (f pathObj) initialState
       in if null finalState.invalidPaths
            then Right gtoc
            else
              let paths =
                    map
                      (("  " <>) . T.pack . decodeGPath)
                      finalState.invalidPaths
               in Left
                    $ "Invalid GTOC object paths:\n"
                    <> (T.unpack $ unlines paths)
  | otherwise = Right . GTOC $ Node emptyGPath []
  where
    f :: (GPath, Object) -> State PathState GTOC
    f pathObj = GTOC <$> unfoldTreeM generator pathObj --
    generator ::
      (GPath, Object) ->
      State PathState (GPath, [(GPath, Object)])
    generator (path, obj)
      | isIndexObjPath path = do
          childPaths <- getChildPaths (path, obj)
          pure (path, childPaths)
      | otherwise = pure (path, [])

findRootIndex :: [(GPath, Object)] -> Maybe (GPath, Object)
findRootIndex pathObjs =
  let result = foldl' f Nothing pathObjs
   in snd <$> result
  where
    f ::
      Maybe (Int, (GPath, Object)) ->
      (GPath, Object) ->
      Maybe (Int, (GPath, Object))
    f acc (curGPath, obj) =
      let pathStr = decodeGPath curGPath
       in if isIndexObjPath curGPath
            then
              let curDepth = length $ splitDirectories pathStr
               in case acc of
                    Nothing -> Just (curDepth, (curGPath, obj))
                    Just (shallowestDepth, _)
                      | curDepth < shallowestDepth ->
                          Just (curDepth, (curGPath, obj))
                      | otherwise -> acc
            else acc

isIndexObjPath :: GPath -> Bool
isIndexObjPath gpath = "index.lobj" `T.isSuffixOf` (T.pack $ decodeGPath gpath)

getChildPaths :: (GPath, Object) -> State PathState [(GPath, Object)]
getChildPaths (path, obj) = foldlM f [] obj.orgDoc.cells
  where
    f :: [(GPath, Object)] -> Cell -> State PathState [(GPath, Object)]
    f acc = \case
      CBlock b
        | Just "index" <- lookup "name" b.meta -> do
            let rawText = T.concat $ map extractBTokenStringOnly b.body
                indexPath = T.pack $ decodeGPath path --
                startingPath = T.dropEnd 10 indexPath --
                rawChildPaths = T.lines rawText
                isIndexPath :: Text -> Bool
                isIndexPath = (== "index.org") --
                (indexPaths, standardPaths) = partition isIndexPath rawChildPaths
                toGPath' = mapMaybe (toGPath startingPath)
                paths = toGPath' standardPaths --
                invalidIndexPaths = toGPath' indexPaths
            when (not $ null invalidIndexPaths) $ do
              modify
                $ \s ->
                  s
                    { invalidPaths = s.invalidPaths <> invalidIndexPaths
                    }
            results <- mapM lookupSingle paths
            pure $ acc <> catMaybes results
      _ -> pure acc
    toGPath :: Text -> Text -> Maybe GPath
    toGPath startingPath textPath =
      let joinedPath = startingPath <> textPath --
          constructedPath =
            --
            if ".org" `T.isSuffixOf` joinedPath
              then (T.dropEnd 3 joinedPath) <> "lobj"
              else joinedPath <> "/index.lobj"
       in encodeGPath $ T.unpack constructedPath
    lookupSingle :: GPath -> State PathState (Maybe (GPath, Object))
    lookupSingle p = do
      st <- get
      case lookup p st.objMap of
        Just o -> pure $ Just (p, o)
        Nothing -> do
          modify $ \s -> s {invalidPaths = s.invalidPaths <> [p]}
          pure Nothing
    extractBTokenStringOnly :: BToken -> Text
    extractBTokenStringOnly = \case
      BTokenString text -> text
      _ -> ""

getCitationStyles :: ProjectConf -> [(GPath, Object)] -> IO (Map FilePath Text)
getCitationStyles projectConf pathObjs = do
  allCslPaths <-
    concat
      <$> mapM
        ( \(gPath, obj) ->
            makeRelativeToObjectPath gPath --
              $ maybeToList obj.orgDoc.cslPath
        )
        pathObjs
  cslRawXMLs <- map fst <$> mapM getCitationStyle allCslPaths --
  let m = fromList $ zip allCslPaths cslRawXMLs
  case projectConf.citationStyle of
    Just cslPath -> do
      (text, _) <- getCitationStyle cslPath
      pure $ insert cslPath text m --
    Nothing -> do
      pure m

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

getBibliographies ::
  ProjectConf ->
  [(GPath, Object)] ->
  IO (Map FilePath [CP.Reference (CslJson Text)])
getBibliographies projectConf pathObjs = do
  bibPaths <-
    concat
      <$> mapM
        (\(gPath, obj) -> makeRelativeToObjectPath gPath obj.orgDoc.bibPaths)
        pathObjs
  let allBibPaths = projectConf.bibliography <> bibPaths
  fromList <$> mapM (\p -> fmap (\b -> (p, b)) (getBibliography p)) allBibPaths

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

gciToName :: Archive -> Int -> GCI -> Maybe Prose
gciToName archive offset gci
  | Just (_, cell) <- gciToObjCell archive gci =
      case cell of
        CHeading heading ->
          plain
            $ "§ "
            <> (getSectionNumber heading)
            <> " "
            <> (getProseContents heading.body)
        CParagraph _ ->
          Just
            $ Prose
              [ ptext "¶ (" [],
                ptext cellRef [Verbatim],
                ptext ")" []
              ]
        CGlossaryTerm glossaryTerm ->
          Just
            $ Prose [ptext "✿ " []]
            <> stripLinks glossaryTerm.term --
        CFigure figure
          | Just name <- lookup "name" figure.meta -> verbatim name
          | Just figureNum <- lookup "__figure_number" figure.meta ->
              plain $ "Figure " <> figureNum
          | otherwise -> plain "Figure"
        CTable table
          | Just name <- lookup "name" table.meta -> verbatim name
          | Just tableNum <- lookup "__table_number" table.meta ->
              plain $ "Table " <> tableNum
          | otherwise -> plain "Table"
        CFootnote footnote ->
          Just
            $ Prose
              [ ptext "Footnote (" [],
                ptext ("fn:" <> footnote.name) [Verbatim],
                ptext ")" []
              ]
        CBlock block
          | Just name <- lookup "name" block.meta ->
              verbatim name
          | otherwise -> plain $ "Anonymous Block" <> cellRef
        CMathFragment mathFragment
          | Just name <- lookup "name" mathFragment.meta ->
              verbatim name
          | Just eqNum <- lookup "__equation_number" mathFragment.meta ->
              plain $ "Equation " <> eqNum
          | Just eqNum <- lookup "__equation_anon_number" mathFragment.meta ->
              plain $ "Anonymous Equation " <> eqNum
          | otherwise -> plain "Math Fragment"
        CListItem {} -> Nothing
        CRichBlock {} -> Nothing
        CCommand {} -> Nothing
  | otherwise = Nothing
  where
    cellRef = "Cell " <> (T.show $ gci.unGCI - offset)
    plain body = Just $ Prose [ptext body []]
    verbatim body = Just $ Prose [ptext body [Verbatim]]
