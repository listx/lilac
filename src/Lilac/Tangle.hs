module Lilac.Tangle (expandRootBlocks) where

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
import Relude
  ( Bool (False, True),
    Maybe (Just),
    Text,
    concat,
    foldl',
    fst,
    map,
    otherwise,
    reverse,
    ($),
    (.),
    (/=),
    (<>),
    (==),
  )
import Relude.Extra.Map qualified as EM
import System.OsPath (OsPath)
import System.OsPath qualified as P

expandRootBlocks :: Archive -> [(OsPath, Text)]
expandRootBlocks archive = map expandRootBlock rootBlocks
  where
    rootBlocks =
      concat $ M.foldlWithKey getRootBlocksFromObject [] archive.objects
    getRootBlocksFromObject ::
      [[(OsPath, Block)]] ->
      GPath ->
      Object ->
      [[(OsPath, Block)]]
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
    expandRootBlock :: (OsPath, Block) -> (OsPath, Text)
    expandRootBlock (p, rootBlock) =
      ( p,
        maybeAddShebang rootBlock
          . trimTrailingSpaces
          $ expandBlock archive rootBlock ""
      )
    trimTrailingSpaces :: Text -> Text
    trimTrailingSpaces text =
      T.unlines
        . map (T.dropWhileEnd (== ' '))
        $ T.lines text
    maybeAddShebang :: Block -> Text -> Text
    maybeAddShebang rootBlock text
      | Just shebang <- EM.lookup "shebang" rootBlock.meta =
          shebang <> "\n" <> text
      | otherwise = text

expandBlock :: Archive -> Block -> Text -> Text
expandBlock archive block existingPrefix =
  let acc = "" :: Text
      prefix = "" :: Text
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
