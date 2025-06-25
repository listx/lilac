module Lilac.Weave
  ( WeaveConf (..),
    renderAsText,
    weave,
    weaveToDisk,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.UUID.Types (UUID)
import Lilac.Parse
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (Block, body, meta, parentUuid),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Heading (body, level, section),
    Link (Link, name, target),
    LinkNoweb (name, uuid),
    LinkTarget (LinkTargetNoweb, LinkTargetOther),
    ListMarker
      ( LItemEnd,
        LItemStart,
        LOrderedEnd,
        LOrderedStart,
        LUnorderedEnd,
        LUnorderedStart
      ),
    MathFragment (body),
    OrgDoc (cells, meta, uuid),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (body, style),
    maskToStyles,
  )
import Lilac.Util qualified as LU
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
import Relude
  ( Bool (False, True),
    Either (Right),
    IO,
    Identity,
    Maybe (Just, Nothing),
    ReaderT,
    String,
    Type,
    ask,
    catMaybes,
    drop,
    flip,
    foldMap',
    foldl',
    forM_,
    fst,
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
import System.OsPath qualified as P
import Text.Blaze.Html.Renderer.Text (renderHtml)

type WeaveConf :: Type
data WeaveConf = WeaveConf
  { orgDoc :: OrgDoc,
    goids :: M.Map UUID P.OsPath,
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
          ("" :: T.Text)

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

toSafeHtmlId :: T.Text -> T.Text
toSafeHtmlId = T.map onlyKeepSomeChars
  where
    onlyKeepSomeChars c
      | isAsciiLower c || isAsciiUpper c = c
      | isDigit c = c
      | T.elem c "-_:." = c
      | otherwise = '-'

getProseContents :: [PToken] -> T.Text
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

getSectionText :: Heading -> T.Text
getSectionText heading =
  T.intercalate "."
    . map T.show
    . reverse
    $ heading.section

getLinkAnchor :: Cell -> T.Text
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
  LinkTargetNoweb _ -> False
  LinkTargetOther target
    | T.isPrefixOf "http" target -> True
    | otherwise -> False

weaveStyledText :: StyledText -> HtmlWeaver ()
weaveStyledText styledText = do
  let b = toHtml $ styledText.body
  if styles == []
    then b
    else span_ [class_ $ T.unwords styles] b
  where
    styles = foldl' styleToClasses [] $ maskToStyles styledText.style
    styleToClasses :: [T.Text] -> Style -> [T.Text]
    styleToClasses acc = \case
      Bold -> "bold" : acc
      Italic -> "italic" : acc
      Underline -> "underline" : acc
      Verbatim -> "verbatim" : acc
      Code -> "code" : acc

weaveListMarker :: ListMarker -> HtmlWeaver ()
weaveListMarker =
  toHtmlRaw . \case
    LOrderedStart -> "<ol>" :: T.Text
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
    getBlockContentsWithoutLinks :: [BToken] -> T.Text
    getBlockContentsWithoutLinks = T.concat . map bTokenToText
      where
        bTokenToText :: BToken -> T.Text
        bTokenToText = \case
          BTokenString text -> text
          BTokenPadding -> ""
          BTokenLink (Link _name (LinkTargetNoweb _)) -> "\NUL"
          BTokenLink {} -> ""
    linkAnchor = getLinkAnchor $ CBlock block

colorizeText :: T.Text -> T.Text -> HtmlWeaver ()
colorizeText lang txt
  | Just syntax <- Sky.lookupSyntax lang Sky.defaultSyntaxMap,
    Just lines <- foldMap' Just $ Sky.tokenize defaultTokeniserConfig syntax txt =
      toHtmlRaw
        . Text.Blaze.Html.Renderer.Text.renderHtml
        $ Sky.formatHtmlInline Sky.defaultFormatOpts lines
  | otherwise = code_ $ toHtml txt
  where
    defaultTokeniserConfig = Sky.TokenizerConfig Sky.defaultSyntaxMap False

spliceLinks :: T.Text -> [Link] -> HtmlWeaver T.Text
spliceLinks colorized links = do
  weaveConf <- ask
  let colorizedStrs = T.splitOn "\NUL" colorized
      f :: ([T.Text], [Link]) -> T.Text -> ([T.Text], [Link])
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
      LinkTargetNoweb linkNoweb
        | Just linkUuid <- linkNoweb.uuid,
          Just orgFile <- EM.lookup linkUuid weaveConf.goids,
          Just docUuid <- weaveConf.orgDoc.uuid,
          Just currentOrgFile <- EM.lookup docUuid weaveConf.goids,
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
                  { parentUuid = Nothing,
                    meta = M.fromList [("name", linkNoweb.name)],
                    body = []
                  }
      LinkTargetOther targetText -> targetText

weaveToDisk :: P.OsPath -> OrgDoc -> Bool -> M.Map UUID P.OsPath -> IO ()
weaveToDisk fp od watch goids = do
  let weaveConf =
        WeaveConf
          { orgDoc = od,
            goids = goids,
            watch = watch
          }
  LU.writeFileRobustly fp
    $ renderAsText weaveConf weave

renderAsText :: WeaveConf -> HtmlWeaver a -> T.Text
renderAsText weaveConf weaver =
  TL.toStrict
    . renderText
    $ hoistHtmlT (flip runReaderT weaveConf) weaver
