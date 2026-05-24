module Lilac.Tangle (expandRootBlocks) where

import Data.Text qualified as T
import Lilac.Types
  ( Archive (objects, symbols),
    BToken (BTokenLinkTarget, BTokenPadding, BTokenString),
    Block (body, meta),
    Cell (CBlock),
    LCI (unLCI),
    LinkOid (name),
    LinkTarget (LinkTargetLineLabel, LinkTargetOid),
    Object (orgDoc, symbols),
    OrgDoc (cells),
    gciToObjCell,
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
expandRootBlocks archive outDir =
  map (expandRootBlock archive) rootBlocks
  where
    rootBlocks =
      concat $ foldl' getRootBlocksFromObject [] archive.objects
    getRootBlocksFromObject ::
      [[(OsPath, Block)]] ->
      Object ->
      [[(OsPath, Block)]]
    getRootBlocksFromObject acc object =
      foldl' (getRootBlock object.orgDoc.cells) [] object.symbols : acc
    getRootBlock cells acc lci --
      | Just (CBlock b) <- cells !!? lci.unLCI,
        Just tPath <- lookup "tangle" b.meta,
        Just tOsPath <- P.encodeUtf $ T.unpack tPath =
          (P.combine outDir tOsPath, b) : acc
      | otherwise = acc

expandRootBlock :: Archive -> (OsPath, Block) -> (OsPath, Text)
expandRootBlock archive (tanglePath, rootBlock) =
  ( tanglePath,
    maybeAddShebang rootBlock --
      . trimTrailingSpaces --
      $ expandBlock archive rootBlock ""
  )
  where
    trimTrailingSpaces :: Text -> Text
    trimTrailingSpaces text =
      T.unlines
        . map (T.dropWhileEnd (== ' '))
        $ T.lines text
    maybeAddShebang :: Block -> Text -> Text
    maybeAddShebang block text
      | Just shebang <- lookup "shebang" block.meta =
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
      (acc <> prependExistingPrefix existingPrefix text, getPrefix text) --
    expand (acc, prefix) (BTokenLinkTarget (LinkTargetOid linkOid)) --
      | Just gci <- lookup linkOid archive.symbols,
        Just (_, (CBlock targetBlock)) <- gciToObjCell archive gci =
          ( acc <> (expandBlock archive targetBlock $ existingPrefix <> prefix),
            prefix
          )
      | otherwise = (acc, prefix)
    expand (acc, prefix) (BTokenLinkTarget (LinkTargetLineLabel lineLabel))
      | Just "tangle" <- lookup "line-label" block.meta =
          ( acc <> "(ref:" <> (T.drop 1 $ T.dropEnd 1 lineLabel.name) <> ")",
            prefix
          )
      | otherwise = (acc, prefix)
    expand (acc, prefix) _ = (acc, prefix)

getPrefix :: Text -> Text
getPrefix text = T.takeWhileEnd (/= '\n') text

prependExistingPrefix :: Text -> Text -> Text
prependExistingPrefix existingPrefix text =
  T.intercalate "\n"
    . reverse
    . fst
    . foldl' (prefixTailLines existingPrefix) ([], True)
    $ T.split (== '\n') text

prefixTailLines :: Text -> ([Text], Bool) -> Text -> ([Text], Bool)
prefixTailLines existingPrefix (acc, isFirstLine) line
  | isFirstLine = (line : acc, False)
  | otherwise = (T.append existingPrefix line : acc, False)
