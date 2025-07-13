module Lilac.Tangle
  ( expandRootBlocks,
    tangle,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Lilac.Types
  ( Archive (objects, symbols),
    BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    GPath,
    Link (Link),
    LinkTarget (LinkTargetOid),
    Object (symbols),
    decodeGPath,
  )
import Lilac.Util qualified as LU
import Relude
  ( Bool (False, True),
    IO,
    Int,
    Maybe (Just),
    concat,
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
import System.Directory.OsPath qualified as D
import System.OsPath qualified as P

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

tangle :: Archive -> IO ()
tangle archive = do
  let blocks = sort $ expandRootBlocks archive
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
