module Lilac.Compile
  ( cumulativeOffsets,
    findCycles,
    findExternalOids,
    gciToName,
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
        ancestry,
        backlinks,
        bibs,
        csls,
        glossary,
        objects,
        offsets,
        symbols
      ),
    BToken (BTokenLinkTarget, BTokenPadding, BTokenString),
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
        CTable
      ),
    Figure (meta, parentOid),
    Footnote (name),
    GCI (GCI, unGCI),
    GPath,
    GlossaryTerm (definition, term),
    Heading (body, meta),
    LCI (LCI, unLCI),
    Link (target),
    LinkOid (LinkOid, name, oid),
    LinkTarget
      ( LinkTargetFilePath,
        LinkTargetFootnote,
        LinkTargetLineLabel,
        LinkTargetOid,
        LinkTargetURI
      ),
    MathFragment (meta, parentOid),
    OID,
    Object (Object, externalOids, orgDoc, symbols),
    OrgDoc (bibPaths, cells, cslPath, glossary, oid, pseudoEdges),
    PToken
      ( PTokenCitation,
        PTokenDivEnd,
        PTokenDivStart,
        PTokenLink,
        PTokenMathFragment,
        PTokenStyledText
      ),
    Prose (Prose, unProse),
    PseudoEdge (PseudoEdge),
    Style (Verbatim),
    Table (grid, meta, parentOid),
    decodeGPath,
    encodeGPath,
    gciToObjCell,
    getProseContents,
    getSectionNumber,
    ptext,
    stripLinks,
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
    hashNub,
    intercalate,
    length,
    map,
    mapAccumL,
    mapM,
    mapMaybe,
    maybeToList,
    mempty,
    otherwise,
    pure,
    repeat,
    show,
    snd,
    sort,
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
    (>),
  )
import Relude.Extra.Map (elems, insert, insertWith, lookup, member)
import System.Exit (die)
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
      CFootnote {}
      CListItem {}
      CParagraph {}
      CGlossaryTerm {}
      CCommand {} -> [] --

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

mkArchive :: [(GPath, Object)] -> IO (Either String Archive)
mkArchive pathObjs = do
  let (objects, symbolsWithoutTopLevelUUIDs, glossary) =
        foldl' concatObjs (mempty, mempty, mempty) offsetsFull
      symbols = topLevelUUIDs <> symbolsWithoutTopLevelUUIDs
      backlinks =
        mkBacklinks pathObjs offsets
          $ foldl' (finalizeEdges symbols) [] offsetsFull
      brokenLinks = findBrokenLinks objects symbols
  allBibPaths <-
    concat
      <$> mapM
        (\(gp, o) -> makeRelativeToObjectPath gp o.orgDoc.bibPaths)
        pathObjs
  allCslPaths <-
    concat
      <$> mapM
        ( \(gp, o) ->
            makeRelativeToObjectPath gp
              $ maybeToList o.orgDoc.cslPath
        )
        pathObjs
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
        let archive =
              Archive
                { objects = objects,
                  symbols = symbols,
                  ancestry = mempty,
                  glossary = glossary,
                  backlinks = backlinks,
                  offsets = offsets,
                  csls = fromList csls,
                  bibs = fromList bibs
                }
            ancestry = mkBlockAncestry archive $ getBlockGCIsFromArchive archive
         in pure $ Right archive {ancestry = ancestry}
  where
    topLevelUUIDs :: Map LinkOid GCI --
    topLevelUUIDs =
      let topLevelLinkOids =
            fromList
              $ zip (map (mkLinkOid . snd) $ sort pathObjs) (map GCI [-1, -2 ..])
          mkLinkOid :: Object -> LinkOid
          mkLinkOid obj =
            LinkOid
              { oid = obj.orgDoc.oid,
                name = ""
              }
       in topLevelLinkOids
    offsets :: IntMap GPath
    offsets =
      IntMap.fromList
        $ map (\(offset, (path, _obj)) -> (offset, path)) offsetsFull
    offsetsFull :: [(Int, (GPath, Object))]
    offsetsFull =
      cumulativeOffsets
        . filter (\(numCells, _) -> numCells > 0)
        . map (\(path, obj) -> (length obj.orgDoc.cells, (path, obj)))
        $ sort pathObjs
    concatObjs ::
      (Map GPath Object, Map LinkOid GCI, Map (Text, GPath) LCI) ->
      (Int, (GPath, Object)) ->
      (Map GPath Object, Map LinkOid GCI, Map (Text, GPath) LCI)
    concatObjs (accObjs, accSyms, accGloss) (offset, (path, obj)) =
      ( insert path obj accObjs,
        (M.map (\lci -> GCI $ lci.unLCI + offset) obj.symbols) <> accSyms,
        (M.mapKeys (\term -> (term, path)) obj.orgDoc.glossary) <> accGloss
      )
    findBrokenLinks ::
      Map GPath Object ->
      Map LinkOid GCI ->
      [(GPath, LinkOid)]
    findBrokenLinks objs syms =
      foldl' (getBrokenLink syms) []
        $ allExternalOids objs
    getBrokenLink ::
      Map LinkOid GCI ->
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

finalizeEdges ::
  Map LinkOid GCI ->
  [(GCI, GCI)] ->
  (Int, (GPath, Object)) ->
  [(GCI, GCI)]
finalizeEdges syms acc (offset, (_, obj)) =
  let f (PseudoEdge (lci, linkOid)) = case lookup linkOid syms of
        Nothing -> Nothing
        Just targetGCI -> Just (GCI $ lci.unLCI + offset, targetGCI)
   in (catMaybes $ map f obj.orgDoc.pseudoEdges) <> acc

mkBacklinks ::
  [(GPath, Object)] ->
  IntMap GPath ->
  [(GCI, GCI)] ->
  Map GPath (Map GPath [(GCI, GCI)])
mkBacklinks pathObjs offsets symbolEdges =
  let f acc (from, to)
        | Just (_, fromPath) <- IntMap.lookupLE from.unGCI offsets,
          Just (_, toPath) <- IntMap.lookupLE to.unGCI offsets,
          fromPath /= toPath =
            M.insertWith (<>) toPath [(fromPath, [(from, to)])] acc
        | Just (_, fromPath) <- IntMap.lookupLE from.unGCI offsets,
          to.unGCI < 0,
          Just toPath <- (sort $ map fst pathObjs) !!? (abs to.unGCI - 1),
          fromPath /= toPath =
            M.insertWith (<>) toPath [(fromPath, [(from, to)])] acc
        | otherwise = acc
   in M.map (M.fromListWith (<>)) $ foldl' f mempty symbolEdges

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
      BTokenLinkTarget (LinkTargetOid linkOid) -> Just linkOid
      BTokenLinkTarget {}
      BTokenPadding {}
      BTokenString {} -> Nothing

getBlocksFromArchive :: Archive -> [Block]
getBlocksFromArchive archive = foldl' f [] $ elems archive.symbols
  where
    f :: [Block] -> GCI -> [Block]
    f acc gci
      | Just (_, cell) <- gciToObjCell archive gci =
          case cell of
            CBlock b -> b : acc
            CHeading {}
            CListItem {}
            CParagraph {}
            CGlossaryTerm {}
            CMathFragment {}
            CFigure {}
            CTable {}
            CFootnote {}
            CCommand {} -> acc
      | otherwise = acc

getBlockGCIsFromArchive :: Archive -> [GCI]
getBlockGCIsFromArchive archive = foldl' f [] $ elems archive.symbols
  where
    f :: [GCI] -> GCI -> [GCI]
    f acc gci
      | Just (_, cell) <- gciToObjCell archive gci =
          case cell of
            CBlock {} -> gci : acc
            CHeading {}
            CListItem {}
            CParagraph {}
            CGlossaryTerm {}
            CMathFragment {}
            CFigure {}
            CTable {}
            CFootnote {}
            CCommand {} -> acc
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
      BTokenLinkTarget {}
      BTokenPadding {}
      BTokenString {} -> Nothing

findCycles :: Archive -> [G.Tree LinkOid]
findCycles archive = map (fmap toLinkOid) . foldl' f [] $ G.scc graph
  where
    (graph, vertexToNode, _) = buildGraph $ getBlocksFromArchive archive
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
      CFigure {}
      CFootnote {}
      CListItem {}
      CMathFragment {}
      CCommand {} -> []
    getLinkFromBToken acc bToken = case bToken of --
      BTokenLinkTarget linkTarget -> keepIfExternal acc linkTarget
      BTokenString {}
      BTokenPadding -> acc
    getLinkFromPToken acc pToken = case pToken of --
      PTokenLink link -> keepIfExternal acc link.target
      PTokenStyledText {}
      PTokenCitation {}
      PTokenMathFragment {}
      PTokenDivStart {}
      PTokenDivEnd {} -> acc
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
      LinkTargetFootnote {}
      LinkTargetFilePath {}
      LinkTargetURI {} -> acc

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

getBibliographies ::
  [FilePath] ->
  IO [(FilePath, [CP.Reference (CslJson Text)])]
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
        CListItem {}
        CCommand {} -> Nothing
  | otherwise = Nothing
  where
    cellRef = "Cell " <> (T.show $ gci.unGCI - offset)
    plain body = Just $ Prose [ptext body []]
    verbatim body = Just $ Prose [ptext body [Verbatim]]
