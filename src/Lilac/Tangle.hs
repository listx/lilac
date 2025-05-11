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
import Lilac.Parse
  ( BToken (BTokenLink, BTokenString),
    Block (body, meta),
    Cell (CBlock),
    Link (Link),
    LinkNoweb (LinkNoweb),
    LinkTarget (LinkTargetNoweb),
    OrgDoc (cells),
  )
import Lilac.Util qualified as LU
import Relude
  ( Bool (False, True),
    Either (Right),
    FilePath,
    IO,
    Int,
    Maybe (Just, Nothing),
    Type,
    catMaybes,
    elem,
    error,
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

getSourceCode :: OrgDoc -> FilePath -> SourceCode
getSourceCode od odPath = foldl' f defaultSourceCode od.cells
  where
    f :: SourceCode -> Cell -> SourceCode
    f sc (CBlock b)
      | Just name <- EM.lookup "name" b.meta,
        Just path <- EM.lookup "tangle" b.meta,
        Right filePath <- qualify path =
          let newBlock = useQualifiedTanglePath filePath b
           in sc
                { roots = name : sc.roots,
                  nameToCodeBlock = M.insert name newBlock sc.nameToCodeBlock
                }
      | Just path <- EM.lookup "tangle" b.meta,
        Right filePath <- qualify path =
          let name = (T.pack filePath) <> "\NUL"
              newBlock = useQualifiedTanglePath filePath b
           in sc
                { roots = name : sc.roots,
                  nameToCodeBlock = M.insert name newBlock sc.nameToCodeBlock
                }
      | Just name <- EM.lookup "name" b.meta =
          sc
            { nameToCodeBlock = M.insert name b sc.nameToCodeBlock
            }
      | otherwise = sc
    f sc _ = sc
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
      (BTokenLink (Link _ (LinkTargetNoweb (LinkNoweb _uuid targetName)))) -> Just targetName
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

expandRootBlocks :: SourceCode -> [(P.OsPath, T.Text)]
expandRootBlocks sc = catMaybes $ map expandRootBlock sc.roots
  where
    expandRootBlock :: T.Text -> Maybe (P.OsPath, T.Text)
    expandRootBlock rootBlockName
      | Just rootBlock <- EM.lookup rootBlockName sc.nameToCodeBlock =
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
    expand (acc, _) (BTokenString text) =
      (acc <> prependExistingPrefix text, getPrefix text)
    expand (acc, prefix) (BTokenLink (Link _ (LinkTargetNoweb (LinkNoweb _uuid target))))
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
