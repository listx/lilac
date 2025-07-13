module Lilac.Compile
  ( Archive,
    Object,
    expandRootBlocks,
    findCycles,
    findExternalOids,
    mkObject,
    mkArchive,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lilac.Parse (validateOidText)
import Lilac.Types
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta, parentOid),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Figure (meta),
    GPath,
    Heading (body, meta),
    Link (Link, target),
    LinkOid (LinkOid, name, oid),
    LinkTarget (LinkTargetOid, LinkTargetOther, LinkTargetURI),
    MathFragment (meta),
    OID,
    OrgDoc (cells, oid),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Table (grid, meta),
    decodeGPath,
  )
import Relude
  ( Bool (False, True),
    Either (Left, Right),
    Eq,
    Maybe (Just, Nothing),
    Show,
    String,
    Type,
    catMaybes,
    concat,
    concatMap,
    elem,
    fmap,
    foldl',
    fst,
    intercalate,
    length,
    map,
    otherwise,
    repeat,
    reverse,
    show,
    sort,
    zip,
    ($),
    (.),
    (/=),
    (<>),
    (==),
    (>),
  )
import Relude.Extra.Map qualified as EM
import System.OsPath qualified as P

type Object :: Type
data Object = Object
  { orgDoc :: OrgDoc,
    symbols :: M.Map LinkOid Cell,
    externalOids :: [LinkOid]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Archive :: Type
data Archive = Archive
  { objects :: M.Map GPath Object,
    symbols :: M.Map LinkOid Cell
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

mkObject :: OrgDoc -> Object
mkObject orgDoc =
  Object
    { orgDoc = orgDoc,
      symbols = mkSymbols orgDoc,
      externalOids = findExternalOids orgDoc
    }

mkSymbols :: OrgDoc -> M.Map LinkOid Cell
mkSymbols orgDoc = M.fromList $ concatMap f orgDoc.cells
  where
    f cell = case cell of
      CBlock block -> getSymbols (Just block.parentOid) block.meta cell
      CHeading heading
        | Just oidText <- EM.lookup "ID" heading.meta,
          Just oid <- validateOidText oidText ->
            [ ( LinkOid
                  { oid = oid,
                    name = T.empty
                  },
                cell
              )
            ]
        | otherwise -> []
      CFigure figure -> getSymbols Nothing figure.meta cell
      CMathFragment mathFragment -> getSymbols Nothing mathFragment.meta cell
      CTable table -> getSymbols Nothing table.meta cell
      CListItem {}
      CParagraph {} -> []
    getSymbols :: Maybe OID -> M.Map T.Text T.Text -> Cell -> [(LinkOid, Cell)]
    getSymbols parentOid cellMeta cell =
      concatMap requireName $ catMaybes [parentOid, Just $ orgDoc.oid]
      where
        requireName oid
          | Just name <- EM.lookup "name" cellMeta =
              [ ( LinkOid
                    { oid = oid,
                      name = name
                    },
                  cell
                )
              ]
          | otherwise = []

mkArchive :: [(GPath, Object)] -> Either String Archive
mkArchive pathObjs =
  let (objects, symbols) = foldl' concatObjs (M.empty, M.empty) pathObjs
      brokenLinks = findBrokenLinks objects symbols
   in if
        | brokenLinks /= [] ->
            Left
              $ "Found broken links:\n"
              <> intercalate
                "\n"
                (map showBrokenLink brokenLinks)
        | otherwise ->
            Right
              Archive
                { objects = objects,
                  symbols = symbols
                }
  where
    concatObjs ::
      (M.Map GPath Object, M.Map LinkOid Cell) ->
      (GPath, Object) ->
      (M.Map GPath Object, M.Map LinkOid Cell)
    concatObjs (accObjs, accSyms) (path, obj) =
      ( M.insert path obj accObjs,
        M.union obj.symbols accSyms
      )
    findBrokenLinks ::
      M.Map GPath Object ->
      M.Map LinkOid Cell ->
      [(GPath, LinkOid)]
    findBrokenLinks objs syms =
      foldl' (getBrokenLink syms) []
        $ allExternalOids objs
    getBrokenLink ::
      M.Map LinkOid Cell ->
      [(GPath, LinkOid)] ->
      (GPath, LinkOid) ->
      [(GPath, LinkOid)]
    getBrokenLink syms acc (gPath, linkOid) = case EM.lookup linkOid syms of
      Just _ -> acc
      Nothing -> (gPath, linkOid) : acc
    allExternalOids :: M.Map GPath Object -> [(GPath, LinkOid)]
    allExternalOids objs =
      M.foldlWithKey
        (\acc gPath obj -> (zip (repeat gPath) obj.externalOids) <> acc)
        []
        objs
    showBrokenLink :: (GPath, LinkOid) -> String
    showBrokenLink (gPath, link) =
      "  " <> (T.unpack $ show link) <> " (" <> (decodeGPath gPath) <> ")"

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
      | Just name <- EM.lookup "name" block.meta =
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
    blocks = foldl' getBlocksFromAllSymbols [] $ M.elems archive.symbols
    getBlocksFromAllSymbols acc cell = case cell of
      CBlock b -> b : acc
      CHeading {}
      CListItem {}
      CParagraph {}
      CMathFragment {}
      CFigure {}
      CTable {} -> acc
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

findExternalOids :: OrgDoc -> [LinkOid]
findExternalOids orgDoc =
  sort
    . Set.toList
    . Set.fromList
    $ concatMap f orgDoc.cells
  where
    f cell = case cell of
      CBlock block -> foldl' g [] block.body
      CHeading heading -> foldl' h [] heading.body
      CParagraph prose -> foldl' h [] prose
      CTable table -> M.foldl (foldl' h) [] table.grid
      CFigure {}
      CListItem {}
      CMathFragment {} -> []
    g acc = \case
      BTokenLink link -> getExternal acc link
      BTokenString {}
      BTokenPadding -> acc
    h acc = \case
      PTokenLink link -> getExternal acc link
      PTokenStyledText {}
      PTokenMathFragment {} -> acc
    getExternal acc link = case link.target of
      LinkTargetOid linkOid ->
        if M.member linkOid symbols
          then acc
          else linkOid : acc
      LinkTargetURI {}
      LinkTargetOther {} -> acc
    symbols = mkSymbols orgDoc

expandRootBlocks :: Archive -> [(P.OsPath, T.Text)]
expandRootBlocks archive = map expandRootBlock rootBlocks
  where
    rootBlocks =
      concat $ M.foldlWithKey getRootBlocksFromObject [] archive.objects
    getRootBlocksFromObject ::
      [[(P.OsPath, Block)]] ->
      GPath ->
      Object ->
      [[(P.OsPath, Block)]]
    getRootBlocksFromObject acc objPath object =
      M.foldl (getRootBlock objPath) [] object.symbols : acc
    getRootBlock objPath acc cell = case cell of
      CBlock b
        | Just path <- EM.lookup "tangle" b.meta,
          Just osPath <- P.encodeUtf $ T.unpack path,
          Just osPathOfObject <- P.encodeUtf $ decodeGPath objPath ->
            (P.combine (P.takeDirectory osPathOfObject) osPath, b) : acc
        | otherwise -> acc
      CHeading {}
      CListItem {}
      CParagraph {}
      CMathFragment {}
      CFigure {}
      CTable {} -> acc
    expandRootBlock :: (P.OsPath, Block) -> (P.OsPath, T.Text)
    expandRootBlock (p, rootBlock) =
      ( p,
        maybeAddShebang rootBlock
          . trimTrailingSpaces
          $ expandBlock archive rootBlock ""
      )
    trimTrailingSpaces :: T.Text -> T.Text
    trimTrailingSpaces text =
      T.unlines
        . map (T.dropWhileEnd (== ' '))
        $ T.lines text
    maybeAddShebang :: Block -> T.Text -> T.Text
    maybeAddShebang rootBlock text
      | Just shebang <- EM.lookup "shebang" rootBlock.meta =
          shebang <> "\n" <> text
      | otherwise = text

expandBlock :: Archive -> Block -> T.Text -> T.Text
expandBlock archive block existingPrefix =
  let acc = "" :: T.Text
      prefix = "" :: T.Text
   in fst $ foldl' expand (acc, prefix) block.body
  where
    expand (acc, _) BTokenPadding =
      (acc <> "\n", "")
    expand (acc, _) (BTokenString text) =
      (acc <> prependExistingPrefix text, getPrefix text)
    expand (acc, prefix) (BTokenLink (Link _ (LinkTargetOid linkOid)))
      | Just (CBlock targetBlock) <- EM.lookup linkOid archive.symbols =
          ( acc <> (expandBlock archive targetBlock $ existingPrefix <> prefix),
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
