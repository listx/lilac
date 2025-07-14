module Lilac.Weave
  ( WeaveConf (..),
    renderAsText,
    weave,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lilac.Parse (maskToStyles)
import Lilac.Types
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (Block, body, meta, parentOid),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Heading (body, level, section),
    Link (Link, name, target),
    LinkOid (name, oid),
    LinkTarget (LinkTargetOid, LinkTargetOther, LinkTargetURI),
    ListMarker
      ( LItemEnd,
        LItemStart,
        LOrderedEnd,
        LOrderedStart,
        LUnorderedEnd,
        LUnorderedStart
      ),
    MathFragment (body),
    OID,
    OrgDoc (cells, meta, oid),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (body, style),
  )
import Lucid
  ( HtmlT,
    a_,
    body_,
    charset_,
    class_,
    code_,
    crossorigin_,
    div_,
    doctypehtml_,
    h1_,
    h2_,
    h3_,
    h4_,
    h5_,
    h6_,
    head_,
    href_,
    id_,
    link_,
    meta_,
    p_,
    pre_,
    rel_,
    renderText,
    script_,
    span_,
    src_,
    title_,
    toHtml,
    toHtmlRaw,
    type_,
  )
import Lucid.Base (hoistHtmlT)
import Network.URI qualified as N
import Relude
  ( Bool (False, True),
    Either (Right),
    Identity,
    Map,
    Maybe (Just, Nothing),
    ReaderT,
    String,
    Text,
    Type,
    ask,
    catMaybes,
    drop,
    flip,
    foldMap',
    foldl',
    forM_,
    fst,
    id,
    lift,
    map,
    otherwise,
    pure,
    reverse,
    runReaderT,
    sequence_,
    take,
    when,
    ($),
    (.),
    (<>),
    (=<<),
    (==),
    (||),
  )
import Relude.Extra.Map qualified as EM
import Skylighting qualified as Sky
import System.IO (utf8)
import System.OsPath (OsPath)
import System.OsPath qualified as P
import Text.Blaze.Html.Renderer.Text (renderHtml)

type WeaveConf :: Type
data WeaveConf = WeaveConf
  { orgDoc :: OrgDoc,
    goids :: Map OID OsPath,
    watch :: Bool
  }

type HtmlWeaver :: Type -> Type
type HtmlWeaver a = HtmlT (ReaderT WeaveConf Identity) a

weave :: HtmlWeaver ()
weave = do
  weaveConf <- ask
  let title' = toHtml $ case EM.lookup "title" weaveConf.orgDoc.meta of
        Just title -> title
        Nothing -> "Untitled"
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ title'
      when weaveConf.watch
        $ script_
          [ type_ "text/javascript",
            src_ "https://livejs.com/live.js"
          ]
          ("" :: Text)

      link_ [rel_ "stylesheet", href_ "./css/default.css"]
      link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
      link_
        [ rel_ "preconnect",
          href_ "https://fonts.gstatic.com",
          crossorigin_ "anonymous"
        ]
      let fontsLink =
            T.intercalate
              ""
              [ "https://fonts.googleapis.com/css2?display=swap",
                "&family=Source+Code+Pro:ital,wght@0,200..900;1,200..900",
                "&family=Source+Sans+3:ital,wght@0,200..900;1,200..900",
                "&family=Source+Serif+4:ital,opsz,wght@0,8..60,200..900;1,8..60,200..900"
              ]
      link_ [rel_ "stylesheet", href_ fontsLink]
    body_ $ do
      forM_ weaveConf.orgDoc.cells weaveCell

weaveCell :: Cell -> HtmlWeaver ()
weaveCell = \case
  CHeading heading -> weaveHeading heading
  CListItem listMarkers -> sequence_ $ map weaveListMarker listMarkers
  CParagraph prose -> p_ $ sequence_ $ map weavePToken prose
  CBlock block -> weaveBlock block
  CMathFragment _ -> do
    p_ "FRAGMENT"
  CFigure _ -> do
    p_ "FIGURE"
  CTable _ -> do
    p_ "TABLE"

toSafeHtmlId :: Text -> Text
toSafeHtmlId = T.map onlyKeepSomeChars
  where
    onlyKeepSomeChars c
      | isAsciiLower c || isAsciiUpper c = c
      | isDigit c = c
      | T.elem c "-_:." = c
      | otherwise = '-'

getProseContents :: [PToken] -> Text
getProseContents = join . foldl' f []
  where
    join = T.intercalate "" . reverse
    f acc = \case
      PTokenStyledText styledText -> styledText.body : acc
      PTokenLink link ->
        let linkText =
              join
                $ foldl'
                  (\acc' n -> n.body : acc')
                  []
                  link.name
         in linkText : acc
      PTokenMathFragment mathFragment -> mathFragment.body : acc

weaveHeading :: Heading -> HtmlWeaver ()
weaveHeading heading =
  hN [id_ linkAnchor] $ do
    a_ [href_ $ "#" <> linkAnchor] $ do
      toHtml $ sectionText <> " "
      sequence_ $ map weavePToken heading.body
  where
    hN = case heading.level of
      1 -> h1_
      2 -> h2_
      3 -> h3_
      4 -> h4_
      5 -> h5_
      _ -> h6_
    sectionText = getSectionText heading
    linkAnchor = getLinkAnchor $ CHeading heading

getSectionText :: Heading -> Text
getSectionText heading =
  T.intercalate "."
    . map T.show
    . reverse
    $ heading.section

getLinkAnchor :: Cell -> Text
getLinkAnchor = \case
  CHeading heading ->
    T.intercalate
      "-"
      [ "h",
        getSectionText heading,
        toSafeHtmlId $ getProseContents heading.body
      ]
  CListItem {} -> ""
  CParagraph {} -> ""
  CBlock block ->
    let ref
          | Just name <- EM.lookup "name" block.meta =
              name
          | Just anonBlockNum <- EM.lookup "__anon_block_number" block.meta =
              anonBlockNum
          | otherwise = "anon-block-error"
     in T.intercalate
          "-"
          [ "b",
            toSafeHtmlId ref
          ]
  CMathFragment {} -> ""
  CFigure {} -> ""
  CTable {} -> ""

weavePToken :: PToken -> HtmlWeaver ()
weavePToken = \case
  PTokenStyledText styledText -> weaveStyledText styledText
  PTokenLink link -> weaveLink link
  PTokenMathFragment _ -> p_ ""

isExternalLink :: LinkTarget -> Bool
isExternalLink = \case
  LinkTargetOid {} -> False
  LinkTargetURI {} -> True
  LinkTargetOther {} -> True

weaveStyledText :: StyledText -> HtmlWeaver ()
weaveStyledText styledText = do
  let b = toHtml $ styledText.body
  if styles == []
    then b
    else span_ [class_ $ T.unwords styles] b
  where
    styles = foldl' styleToClasses [] $ maskToStyles styledText.style
    styleToClasses :: [Text] -> Style -> [Text]
    styleToClasses acc = \case
      Bold -> "bold" : acc
      Italic -> "italic" : acc
      Underline -> "underline" : acc
      Verbatim -> "verbatim" : acc
      Code -> "code" : acc

weaveListMarker :: ListMarker -> HtmlWeaver ()
weaveListMarker =
  toHtmlRaw . \case
    LOrderedStart -> "<ol>" :: Text
    LOrderedEnd -> "</ol>"
    LUnorderedStart -> "<ul>"
    LUnorderedEnd -> "</ul>"
    LItemStart -> "<li>"
    LItemEnd -> "</li>"

weaveBlock :: Block -> HtmlWeaver ()
weaveBlock block = do
  weaveConf <- ask
  div_ [class_ "code-block-container", id_ linkAnchor] $ do
    div_ [class_ "side-popup"] $ do
      a_ [href_ $ "#" <> linkAnchor] $ do
        toHtmlRaw ("&#x1F517;" :: String)
    maybeAssignName
    let bodyBorder = case EM.lookup "name" block.meta of
          Just _ -> ""
          Nothing -> " fully-rounded"
    pre_ [class_ $ "code-block-body" <> bodyBorder] $ do
      let bodyText = getBlockContentsWithoutLinks block.body
      case EM.lookup "__src_language" block.meta of
        Just lang -> do
          let colorized = renderAsText weaveConf $ colorizeText lang bodyText
              getLink = \case
                BTokenLink l -> Just l
                BTokenString {}
                BTokenPadding {} -> Nothing
              links = catMaybes $ map getLink block.body
          toHtmlRaw =<< spliceLinks colorized links
        Nothing -> code_ $ toHtml bodyText
  where
    maybeAssignName =
      case EM.lookup "name" block.meta of
        Just name ->
          div_ [class_ "code-block-name"] $ do
            code_ $ toHtml name
        Nothing -> pure ()
    getBlockContentsWithoutLinks :: [BToken] -> Text
    getBlockContentsWithoutLinks = T.concat . map bTokenToText
      where
        bTokenToText :: BToken -> Text
        bTokenToText = \case
          BTokenString text -> text
          BTokenPadding -> ""
          BTokenLink (Link _name (LinkTargetOid _)) -> "\NUL"
          BTokenLink {} -> ""
    linkAnchor = getLinkAnchor $ CBlock block

colorizeText :: Text -> Text -> HtmlWeaver ()
colorizeText lang txt
  | Just syntax <- Sky.lookupSyntax lang Sky.defaultSyntaxMap,
    Just lines <- foldMap' Just $ Sky.tokenize defaultTokeniserConfig syntax txt =
      toHtmlRaw
        . Text.Blaze.Html.Renderer.Text.renderHtml
        $ Sky.formatHtmlInline Sky.defaultFormatOpts lines
  | otherwise = code_ $ toHtml txt
  where
    defaultTokeniserConfig = Sky.TokenizerConfig Sky.defaultSyntaxMap False

spliceLinks :: Text -> [Link] -> HtmlWeaver Text
spliceLinks colorized links = do
  weaveConf <- ask
  let colorizedStrs = T.splitOn "\NUL" colorized
      f :: ([Text], [Link]) -> Text -> ([Text], [Link])
      f (acc, []) colorizedStr = (colorizedStr : acc, [])
      f (acc, link : links') colorizedStr =
        ( colorizedStr
            : (renderAsText weaveConf $ weaveLink link)
            : acc,
          links'
        )
  pure
    . T.intercalate ""
    . reverse
    . fst
    . foldl' f (take 1 colorizedStrs, links)
    $ drop 1 colorizedStrs

weaveLink :: Link -> HtmlWeaver ()
weaveLink link = do
  weaveConf <- lift ask
  a_
    [ href_ $ linkTarget weaveConf,
      class_ linkCategory
    ]
    $ sequence_
    $ map weaveStyledText link.name
  where
    linkCategory =
      if isExternalLink link.target
        then "external-link"
        else "internal-link"
    linkTarget weaveConf = case link.target of
      LinkTargetOid linkOid
        | Just orgFile <- EM.lookup linkOid.oid weaveConf.goids,
          Just currentOrgFile <- EM.lookup weaveConf.orgDoc.oid weaveConf.goids,
          Right str <- P.decodeWith utf8 utf8 $ P.replaceExtension orgFile htmlExt ->
            if orgFile == currentOrgFile
              then "#" <> linkAnchor
              else (T.pack str) <> "#" <> linkAnchor
        | otherwise -> "#" <> linkAnchor
        where
          htmlExt = P.unsafeEncodeUtf "html"
          linkAnchor =
            getLinkAnchor
              $ CBlock
                Block
                  { parentOid = weaveConf.orgDoc.oid,
                    meta = M.fromList [("name", linkOid.name)],
                    body = []
                  }
      LinkTargetURI uri -> T.pack $ N.uriToString id uri ""
      LinkTargetOther protocol target -> protocol <> ":" <> target

renderAsText :: WeaveConf -> HtmlWeaver a -> Text
renderAsText weaveConf weaver =
  TL.toStrict
    . renderText
    $ hoistHtmlT (flip runReaderT weaveConf) weaver
