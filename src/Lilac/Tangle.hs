module Lilac.Tangle
  ( BlockName,
    SourceCode (..),
    expandRootBlocks,
    findCycles,
    getSourceCode,
    tangle,
  )
where

import Data.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO.Utf8 (writeFile)
import Lilac.Parse
  ( BToken (BTokenLink, BTokenString),
    Block (body, meta),
    Cell (CBlock),
    Link (Link),
    LinkType (LinkNoweb),
    OrgDoc (cells),
  )
import Relude
  ( Bool (False, True),
    IO,
    Maybe (Just, Nothing),
    Type,
    catMaybes,
    elem,
    error,
    fmap,
    foldl',
    fst,
    length,
    map,
    mapM_,
    otherwise,
    reverse,
    ($),
    (.),
    (/=),
    (<>),
    (==),
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
        Just _ <- EM.lookup "tangle" b.meta =
          sc
            { roots = name : sc.roots,
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

expandRootBlocks :: SourceCode -> [(T.Text, T.Text)]
expandRootBlocks sc = catMaybes $ map expandRootBlock sc.roots
  where
    expandRootBlock :: T.Text -> Maybe (T.Text, T.Text)
    expandRootBlock rootBlockName
      | Just rootBlock <- EM.lookup rootBlockName sc.nameToCodeBlock =
          Just
            ( tanglePathOf rootBlock,
              trimTrailingSpaces $ expandBlock sc rootBlock ""
            )
      | otherwise = Nothing
    trimTrailingSpaces :: T.Text -> T.Text
    trimTrailingSpaces text =
      T.unlines
        . map (T.dropWhileEnd (== ' '))
        $ T.lines text
    tanglePathOf :: Block -> T.Text
    tanglePathOf block
      | Just path <- EM.lookup "tangle" block.meta = path
      | otherwise = error "programmer error"

expandBlock :: SourceCode -> Block -> T.Text -> T.Text
expandBlock sc block existingPrefix =
  let acc = "" :: T.Text
      prefix = "" :: T.Text
   in fst $ foldl' expand (acc, prefix) block.body
  where
    expand (acc, _) (BTokenString text) =
      (acc <> prependExistingPrefix text, getPrefix text)
    expand (acc, prefix) (BTokenLink (Link _ target LinkNoweb))
      | Just targetBlock <- EM.lookup target sc.nameToCodeBlock =
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

tangle :: OrgDoc -> IO ()
tangle od = mapM_ f (expandRootBlocks $ getSourceCode od)
  where
    f (tanglePath, content) = writeFile (T.unpack tanglePath) content
