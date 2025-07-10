module Lilac.Compile
  ( Archive,
    Object,
    findCycles,
    findExternalOids,
    mkObject,
    mkArchive,
  )
where

import Data.Aeson (ToJSON)
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
    Heading (body, meta),
    Link (Link, target),
    LinkOid (LinkOid, name, oid),
    LinkTarget (LinkTargetOid, LinkTargetOther, LinkTargetURI),
    MathFragment (meta),
    OID,
    OrgDoc (cells, oid),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Table (grid, meta),
  )
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    Eq,
    FilePath,
    Maybe (Just, Nothing),
    Show,
    Type,
    catMaybes,
    concatMap,
    elem,
    fmap,
    foldl',
    length,
    map,
    otherwise,
    sort,
    ($),
    (.),
    (/=),
    (<>),
    (>),
  )
import Relude.Extra.Map qualified as EM

type Object :: Type
data Object = Object
  { orgDoc :: OrgDoc,
    symbols :: M.Map LinkOid Cell,
    externalOids :: [LinkOid]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

type Archive :: Type
data Archive = Archive
  { objects :: M.Map FilePath Object,
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

mkArchive :: [(FilePath, Object)] -> Either T.Text Archive
mkArchive pathObjs
  | brokenLinks /= [] =
      Left
        $ "broken links:\n"
        <> T.intercalate
          "\n"
          (map (\l -> "  " <> LU.mkFqcn l.oid l.name) brokenLinks)
  | otherwise = Right archive
  where
    archive =
      Archive
        { objects = objects,
          symbols = symbols
        }
    (objects, symbols) = foldl' f (M.empty, M.empty) pathObjs
    f ::
      (M.Map FilePath Object, M.Map LinkOid Cell) ->
      (FilePath, Object) ->
      (M.Map FilePath Object, M.Map LinkOid Cell)
    f (accObjs, accSyms) (path, obj) =
      ( M.insert path obj accObjs,
        M.union obj.symbols accSyms
      )
    brokenLinks = foldl' g [] allExternalOids
    g acc linkOid = case EM.lookup linkOid archive.symbols of
      Just _ -> acc
      Nothing -> linkOid : acc
    allExternalOids =
      M.foldl (\acc obj -> obj.externalOids <> acc) [] archive.objects

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
