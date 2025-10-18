module Lilac.Tangle (expandRootBlocks) where

import Data.Text qualified as T
import Lilac.Types
  ( Archive (objects, symbols),
    BToken (BTokenLinkTarget, BTokenPadding, BTokenString),
    Block (body, meta),
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
    LinkTarget (LinkTargetOid),
    Object (orgDoc, symbols),
    OrgDoc (cells),
    cellIdxToObjCell,
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
    (!!?),
    ($),
    (.),
    (/=),
    (<>),
    (==),
  )
import Relude.Extra.Map (lookup)
import System.OsPath (OsPath)
import System.OsPath qualified as P

expandRootBlocks :: Archive -> OsPath -> [(OsPath, Text)]
expandRootBlocks archive outDir = map expandRootBlock rootBlocks
  where
    rootBlocks =
      concat $ foldl' getRootBlocksFromObject [] archive.objects
    getRootBlocksFromObject ::
      [[(OsPath, Block)]] ->
      Object ->
      [[(OsPath, Block)]]
    getRootBlocksFromObject acc object =
      foldl' (getRootBlock object.orgDoc.cells) [] object.symbols : acc
    getRootBlock cells acc localCellIdx
      | Just cell <- cells !!? localCellIdx =
          case cell of
            CBlock b
              | Just tPath <- lookup "tangle" b.meta,
                Just tOsPath <- P.encodeUtf $ T.unpack tPath ->
                  (P.combine outDir tOsPath, b) : acc
              | otherwise -> acc
            CHeading {}
            CListItem {}
            CParagraph {}
            CMathFragment {}
            CFigure {}
            CTable {}
            CFootnote {}
            CBibliography {} -> acc
      | otherwise = acc
    expandRootBlock :: (OsPath, Block) -> (OsPath, Text)
    expandRootBlock (p, rootBlock) =
      ( P.combine outDir p,
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
      | Just shebang <- lookup "shebang" rootBlock.meta =
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
    expand (acc, prefix) (BTokenLinkTarget (LinkTargetOid linkOid))
      | Just globalCellIdx <- lookup linkOid archive.symbols,
        Just (_, (CBlock targetBlock)) <- cellIdxToObjCell archive globalCellIdx =
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
