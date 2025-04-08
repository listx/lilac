module Lilac.Tangle
  ( BlockName,
    SourceCode (..),
    findCycles,
    getSourceCode,
  )
where

import Data.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Lilac.Parse
  ( BToken (BTokenLink),
    Block (body, meta),
    Cell (CBlock),
    Link (Link),
    LinkType (LinkNoweb),
    OrgDoc (cells),
  )
import Relude
  ( Maybe (Just, Nothing),
    Type,
    catMaybes,
    elem,
    fmap,
    foldl',
    length,
    map,
    otherwise,
    ($),
    (.),
    (>),
  )
import Relude.Extra.Map qualified as EM
import Relude.Nub
  ( hashNub,
  )

type SourceCode :: Type
data SourceCode = SourceCode
  { roots :: [BlockName],
    nameToCodeBlock :: M.Map BlockName Block
  }

type BlockName :: Type
type BlockName = T.Text

defaultSourceCode :: SourceCode
defaultSourceCode =
  SourceCode
    { roots = [],
      nameToCodeBlock = M.fromList []
    }

getSourceCode :: OrgDoc -> SourceCode
getSourceCode od = foldl' f defaultSourceCode od.cells
  where
    f :: SourceCode -> Cell -> SourceCode
    f sc (CBlock b)
      | Just name <- EM.lookup "name" b.meta,
        Just tangleMe <- EM.lookup "tangle" b.meta =
          sc
            { roots = tangleMe : sc.roots,
              nameToCodeBlock = M.insert name b sc.nameToCodeBlock
            }
      | Just name <- EM.lookup "name" b.meta =
          sc
            { nameToCodeBlock = M.insert name b sc.nameToCodeBlock
            }
      | otherwise = sc
    f sc _ = sc

buildGraph ::
  [Block] ->
  ( G.Graph,
    G.Vertex -> (Block, BlockName, [BlockName]),
    BlockName -> Maybe G.Vertex
  )
buildGraph blocks = G.graphFromEdges $ foldl' f [] blocks
  where
    f ::
      [(Block, BlockName, [BlockName])] ->
      Block ->
      [(Block, BlockName, [BlockName])]
    f acc block
      | Just name <- EM.lookup "name" block.meta =
          (\targets -> (block, name, targets) : acc)
            . hashNub
            . catMaybes
            $ map getLinkTarget block.body
      | otherwise = acc
    getLinkTarget = \case
      (BTokenLink (Link _ target LinkNoweb)) -> Just target
      _ -> Nothing

findCycles :: SourceCode -> [G.Tree BlockName]
findCycles sc = map (fmap toBlockName) . foldl' f [] $ G.scc graph
  where
    (graph, vertexToNode, _) = buildGraph $ M.elems sc.nameToCodeBlock
    toBlockName vertex =
      let (_, blockName, _) = vertexToNode vertex
       in blockName
    f acc tree@(G.Node vertex _)
      | length tree > 1 = tree : acc
      | (_, name, targets) <- vertexToNode vertex,
        elem name targets =
          tree : acc
      | otherwise = acc
