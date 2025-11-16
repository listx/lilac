module Lilac.Weave
  ( commonPrefix,
    getRelativePathWithoutSymlinks,
    parseProjectConf,
    prepareCitations,
    renderAsText,
    weaveObject,
  )
where

import Citeproc
  ( ItemId (unItemId),
    Reference (referenceId),
    Result (resultBibliography, resultCitations),
  )
import Citeproc qualified as CP
import Citeproc.CslJson
  ( CslJson
      ( CslBaseline,
        CslBold,
        CslConcat,
        CslDiv,
        CslEmpty,
        CslItalic,
        CslLink,
        CslNoCase,
        CslNoDecoration,
        CslNormal,
        CslQuoted,
        CslSmallCaps,
        CslSub,
        CslSup,
        CslText,
        CslUnderline
      ),
    parseCslJson,
  )
import Data.Aeson (eitherDecodeStrictText, encode)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Graph qualified as G
import Data.IntMap.Strict qualified as IntMap
import Data.List (groupBy)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Lilac.Compile (cellIdxToName, makeRelativeToObjectPath, parseCitationStyle)
import Lilac.Parse
  ( compressProse,
    compressStyledText,
    isNumericText,
    maskToStyles,
    styleMaskFrom,
    tableHorizontalLine,
    unparseLink,
    unparseMathFragment,
  )
import Lilac.Types
  ( Archive
      ( backlinks,
        bibs,
        csls,
        glossary,
        objects,
        offsets,
        symbols
      ),
    BToken (BTokenLinkTarget, BTokenPadding, BTokenString),
    Block (body, meta),
    Cell
      ( CBlock,
        CCommand,
        CFigure,
        CFootnote,
        CGlossaryTerm,
        CHeading,
        CListItem,
        CMathFragment,
        CParagraph,
        CTable
      ),
    Citation (cites, noteNumber, prefix, styleVariation, suffix),
    Cite (id, locator, prefix, suffix),
    Command (PrintBibliography, PrintGlossary),
    Figure (caption, link, meta),
    Footnote (body, name),
    GPath,
    GlossaryTerm (definition, term),
    Heading (body, level, section),
    Link (Link, name, target),
    LinkOid (LinkOid),
    LinkTarget
      ( LinkTargetFilePath,
        LinkTargetFootnote,
        LinkTargetLineLabel,
        LinkTargetOid,
        LinkTargetURI
      ),
    ListMarker
      ( LItemEnd,
        LItemStart,
        LOrderedEnd,
        LOrderedStart,
        LUnorderedEnd,
        LUnorderedStart
      ),
    MathFragment (body, meta),
    Object (orgDoc, symbols),
    OrgDoc
      ( bibPaths,
        cells,
        citations,
        cslPath,
        footnoteRefs,
        glossary,
        lineLabels,
        meta
      ),
    PToken
      ( PTokenCitation,
        PTokenDivEnd,
        PTokenDivStart,
        PTokenLink,
        PTokenMathFragment,
        PTokenStyledText
      ),
    ProjectConf (ProjectConf, homepage, name, repo, version),
    Prose (Prose, unProse),
    RawProjectConf (homepage, name, repo, version),
    Style
      ( Bold,
        Code,
        Italic,
        Smallcaps,
        Subscript,
        Superscript,
        Underline,
        Verbatim
      ),
    StyledText (StyledText, body, style),
    Table (caption, grid, meta),
    WeaveConf
      ( archive,
        citeprocResult,
        originObjectPath,
        outDir,
        projectConf
      ),
    cellIdxToObjCell,
    decodeGPath,
    getCellName,
    getProseContents,
    getSectionNumber,
    markingChars,
    markingCharsInvisible,
    stripLinks,
    stripStyles,
  )
import Lucid
  ( Attributes,
    HtmlT,
    a_,
    alt_,
    aside_,
    async_,
    body_,
    br_,
    button_,
    charset_,
    class_,
    code_,
    col_,
    colgroup_,
    crossorigin_,
    dd_,
    div_,
    doctypehtml_,
    dt_,
    figcaption_,
    figure_,
    footer_,
    h1_,
    h2_,
    h3_,
    h4_,
    h5_,
    h6_,
    head_,
    height_,
    hr_,
    href_,
    id_,
    img_,
    li_,
    link_,
    main_,
    meta_,
    nav_,
    ol_,
    p_,
    pre_,
    rel_,
    renderText,
    script_,
    span_,
    src_,
    sup_,
    table_,
    target_,
    tbody_,
    td_,
    th_,
    thead_,
    title_,
    toHtml,
    toHtmlRaw,
    tr_,
    type_,
    width_,
  )
import Lucid.Base (hoistHtmlT)
import Network.URI
  ( uriAuthority,
    uriPath,
    uriScheme,
  )
import Network.URI qualified as N
import Relude
  ( Bool (False, True),
    Either (Left, Right),
    Eq,
    FilePath,
    IO,
    Identity,
    Int,
    Map,
    Maybe (Just, Nothing),
    ReaderT,
    String,
    Text,
    Type,
    abs,
    any,
    ask,
    catMaybes,
    comparing,
    concat,
    concatMap,
    drop,
    dropWhile,
    elem,
    filter,
    flip,
    foldMap',
    foldl',
    forM_,
    fromMaybe,
    fst,
    id,
    isJust,
    isPrefixOf,
    length,
    listToMaybe,
    map,
    max,
    maxBound,
    maybe,
    maybeAt,
    maybeToList,
    mempty,
    negate,
    not,
    notElem,
    null,
    on,
    otherwise,
    pure,
    repeat,
    replicate,
    reverse,
    runReaderT,
    sequence_,
    show,
    snd,
    sort,
    sortBy,
    sortOn,
    take,
    takeWhile,
    when,
    zip,
    (!!?),
    ($),
    (&&),
    (+),
    (-),
    (.),
    (/=),
    (<),
    (<$>),
    (<=),
    (<>),
    (=<<),
    (==),
    (>),
    (||),
  )
import Relude.Extra.Map (insert, keys, lookup, lookupDefault, toPairs)
import Skylighting qualified as Sky
import System.IO (utf8)
import System.OsPath (OsPath)
import System.OsPath qualified as P
import Text.Blaze.Html.Renderer.Text (renderHtml)

type HtmlWeaver :: Type -> Type
type HtmlWeaver a = HtmlT (ReaderT WeaveConf Identity) a

weaveObject :: Object -> HtmlWeaver ()
weaveObject object = do
  let title' = toHtml $ case lookup "title" object.orgDoc.meta of
        Just title -> title
        Nothing -> "Untitled"
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ title'
      relativeCssPath <- getRelativeCssPath
      link_ [rel_ "stylesheet", href_ relativeCssPath]
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

      let s :: Text
          s =
            """
            window.MathJax = {
              tex: {
                  tags: 'ams'
              },

              options: {
                  ignoreHtmlClass: 'verbatim|code'
              }
            };
            """
      script_ [] s

      script_
        [ id_ "MathJax-script",
          async_ "",
          src_ "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js"
        ]
        ("" :: Text)
    body_ $ do
      weaveNavbar
      weaveToc object.orgDoc.cells
      let cellsWithIdx = zip [0 :: Int ..] object.orgDoc.cells
          rearrangedCells = beforeFootnotes <> sortedFootnotes <> afterFootnotes
          beforeFootnotes = takeWhile (not . isFootnote) cellsWithIdx
          afterFootnotes =
            dropWhile isFootnote
              $ dropWhile (not . isFootnote) cellsWithIdx
          sortedFootnotes =
            sortOn (\(_, cell) -> getSortingKey cell)
              $ takeWhile isFootnote
              $ dropWhile (not . isFootnote) cellsWithIdx
          getSortingKey cell
            | (CFootnote footnote) <- cell,
              Just (n, _) <- lookup footnote.name object.orgDoc.footnoteRefs =
                n
            | otherwise = maxBound :: Int
          isFootnote :: (Int, Cell) -> Bool
          isFootnote (_, cell) = case cell of
            CFootnote {} -> True
            CBlock {}
            CFigure {}
            CHeading {}
            CListItem {}
            CMathFragment {}
            CParagraph {}
            CGlossaryTerm {}
            CTable {}
            CCommand {} -> False
      weaveMain object.orgDoc.meta rearrangedCells
      weaveSidebar . catMaybes $ map getBlock object.orgDoc.cells
      weaveHeadingAncestry rearrangedCells
      relativeJsPath <- getRelativeJsPath
      script_
        [ id_ "lilac-fe-script",
          src_ relativeJsPath
        ]
        ("" :: Text)
  where
    getBlock = \case
      CBlock block -> Just block
      CHeading {}
      CListItem {}
      CParagraph {}
      CGlossaryTerm {}
      CMathFragment {}
      CFigure {}
      CTable {}
      CFootnote {}
      CCommand {} -> Nothing

weaveCell :: (Int, Cell) -> HtmlWeaver ()
weaveCell (cellIdx, cell) = case cell of
  CHeading heading -> weaveHeading cellIdx heading
  CListItem listMarkers -> sequence_ $ map weaveListMarker listMarkers
  CParagraph prose -> p_ [] $ wrapWith cellIdx $ weaveProse prose
  CBlock block -> weaveBlock cellIdx block
  CMathFragment mathFragment -> weaveMathFragmentStandalone cellIdx mathFragment
  CFigure figure -> weaveFigure cellIdx figure
  CTable table -> weaveTable cellIdx table
  CFootnote footnote -> weaveFootnote cellIdx footnote
  CGlossaryTerm glossaryTerm -> weaveGlossaryTerm cellIdx glossaryTerm
  CCommand cmd -> case cmd of
    PrintBibliography -> weaveBibliography cellIdx
    PrintGlossary isGlobal -> weaveGlossary isGlobal

toSafeHtmlId :: Text -> Text
toSafeHtmlId = T.map onlyKeepSomeChars
  where
    onlyKeepSomeChars c
      | isAsciiLower c || isAsciiUpper c = c
      | isDigit c = c
      | T.elem c "-_:." = c
      | otherwise = '-'

weaveDocMeta :: M.Map Text Text -> HtmlWeaver ()
weaveDocMeta m = do
  case lookup "title" m of
    Nothing -> pure ()
    Just title -> do
      h1_ [id_ "top"] $ toHtml title
      hr_ [class_ "title-underline"]
  f "author" $ p_ [class_ "doc-meta-author"]
  f "date" $ p_ [class_ "doc-meta-date"]
  where
    f k element = case lookup k m of
      Nothing -> pure ()
      Just x -> element $ toHtml x

weaveHeading :: Int -> Heading -> HtmlWeaver ()
weaveHeading cellIdx heading = do
  when (heading.section /= [1]) $ toHtmlRaw @Text "</div>"
  let containerId = "heading-container-" <> linkAnchor
  toHtmlRaw $ "<div id=\"" <> containerId <> "\" class=\"heading-container\">"
  hN [id_ linkAnchor] $ do
    wrapWith cellIdx $ do
      a_ [href_ $ "#" <> linkAnchor] $ do
        toHtml sectionText
        toHtmlRaw ("&emsp;&thinsp;" :: Text)
        weaveProse heading.body
  where
    hN = case heading.level of
      1 -> h2_
      2 -> h3_
      3 -> h4_
      4 -> h5_
      _ -> h6_
    sectionText = getSectionNumber heading
    linkAnchor = getLinkAnchor Nothing $ CHeading heading

getLinkAnchor :: Maybe Int -> Cell -> Text
getLinkAnchor localCellIdx = \case
  CHeading heading ->
    T.intercalate
      "-"
      [ "h",
        getSectionNumber heading,
        toSafeHtmlId $ getProseContents heading.body
      ]
  CListItem {} -> ""
  CParagraph {} -> case localCellIdx of
    Just idx -> "c-" <> T.show idx
    Nothing -> ""
  CBlock block ->
    let ref
          | Just name <- lookup "name" block.meta =
              name
          | Just anonBlockNum <- lookup "__anon_block_number" block.meta =
              anonBlockNum
          | otherwise = "anon-block-error"
     in T.intercalate
          "-"
          [ "b",
            toSafeHtmlId ref
          ]
  CMathFragment mathFragment ->
    let ref
          | Just name <- lookup "name" mathFragment.meta,
            Just eqNum <- lookup "__equation_number" mathFragment.meta =
              "eq-" <> eqNum <> "-" <> name
          | Just eqNum <- lookup "__equation_number" mathFragment.meta =
              "eq-" <> eqNum
          | Just eqNum <- lookup "__equation_anon_number" mathFragment.meta =
              "eq-anon-" <> eqNum
          | otherwise = "eq-num-error"
     in toSafeHtmlId ref
  CFigure figure ->
    let ref
          | Just name <- lookup "name" figure.meta,
            Just figureNum <- lookup "__figure_number" figure.meta =
              "fig-" <> figureNum <> "-" <> name
          | Just figureNum <- lookup "__figure_number" figure.meta =
              "fig-" <> figureNum
          | otherwise = "figure-num-error"
     in toSafeHtmlId ref
  CTable table ->
    let ref
          | Just name <- lookup "name" table.meta,
            Just tableNum <- lookup "__table_number" table.meta =
              "tbl-" <> tableNum <> "-" <> name
          | Just tableNum <- lookup "__table_number" table.meta =
              "tbl-" <> tableNum
          | otherwise = "table-num-error"
     in toSafeHtmlId ref
  CFootnote footnote ->
    let ref = "footnote-" <> footnote.name
     in toSafeHtmlId ref
  CGlossaryTerm glossaryTerm -> case localCellIdx of
    Just idx ->
      let ref =
            "glossary-"
              <> "c-"
              <> T.show idx
              <> "-"
              <> toSafeHtmlId (CP.toText @Prose $ stripLinks glossaryTerm.term)
       in toSafeHtmlId ref
    Nothing -> ""
  CCommand {} -> ""

cellIdxAttrs :: Int -> [Attributes]
cellIdxAttrs cellIdx = [id_ ("c-" <> T.show cellIdx), class_ "cell-idx"]

wrapWith :: Int -> HtmlWeaver () -> HtmlWeaver ()
wrapWith cellIdx wrapMe = do
  span_ (cellIdxAttrs cellIdx) $ do
    wrapMe

wrapWithDiv :: Int -> HtmlWeaver () -> HtmlWeaver ()
wrapWithDiv cellIdx wrapMe = do
  div_ (cellIdxAttrs cellIdx) $ do
    wrapMe

weaveProse :: Prose -> HtmlWeaver ()
weaveProse prose = sequence_ $ map weavePToken prose.unProse

weavePToken :: PToken -> HtmlWeaver ()
weavePToken = \case
  PTokenStyledText styledText -> weaveStyledText styledText
  PTokenLink link -> case link.target of
    LinkTargetFootnote {} -> do
      sup_ [class_ "footnote-link"] $ do
        weaveLink False link
    LinkTargetOid {}
    LinkTargetLineLabel {}
    LinkTargetFilePath {}
    LinkTargetURI {} -> weaveLink False link
  PTokenMathFragment mathFragment -> weaveMathFragmentInline mathFragment
  PTokenCitation n -> weaveCitation n
  PTokenDivStart t -> toHtmlRaw ("<div class=\"csl-" <> t <> "\">" :: Text)
  PTokenDivEnd -> toHtmlRaw ("</div>" :: Text)

isExternalLink :: LinkTarget -> Bool
isExternalLink = \case
  LinkTargetOid {}
  LinkTargetLineLabel {}
  LinkTargetFootnote {} -> False
  LinkTargetFilePath {} -> True
  LinkTargetURI uri
    | "" <- uri.uriScheme,
      Nothing <- uri.uriAuthority,
      "#ref-" `isPrefixOf` uri.uriPath ->
        False
    | otherwise -> True

weaveStyledText :: StyledText -> HtmlWeaver ()
weaveStyledText styledText = do
  weaveConf <- ask
  let b = toHtmlRaw $ T.intercalate "<br>" parts
      parts =
        map
          (\x -> renderAsText weaveConf $ toHtml x)
          $ T.splitOn "\n" styledText.body
  if styles == []
    then b
    else span_ [class_ $ T.unwords styles] b
  where
    styles = map (T.toLower . T.show) $ maskToStyles styledText.style

weaveListMarker :: ListMarker -> HtmlWeaver ()
weaveListMarker =
  toHtmlRaw . \case
    LOrderedStart -> "<ol>" :: Text
    LOrderedEnd -> "</ol>"
    LUnorderedStart -> "<ul>"
    LUnorderedEnd -> "</ul>"
    LItemStart -> "<li>"
    LItemEnd -> "</li>"

weaveBlock :: Int -> Block -> HtmlWeaver ()
weaveBlock cellIdx block = do
  weaveConf <- ask
  wrapWithDiv cellIdx $ do
    div_ [class_ "code-block-container", id_ linkAnchor] $ do
      div_ [class_ "code-block-controls"] $ do
        button_ [class_ "toggle-code-block-meta-raw"] $ pure ()
        a_ [href_ $ "#" <> linkAnchor] $ do
          toHtmlRaw ("&#x1F517;" :: String)
      maybeWeaveNameAndTanglePath
      let (rawBorder, bodyBorder)
            | Nothing <- lookup "name" block.meta,
              Nothing <- lookup "tangle" block.meta =
                (" top-rounded", " fully-rounded")
            | otherwise = ("", "")
      div_ [class_ $ "code-block-meta-raw hidden" <> rawBorder] $ do
        sequence_ . map weaveBlockMeta $ M.toList block.meta
      pre_ [class_ $ "code-block-body" <> bodyBorder] $ do
        let bodyText = getBlockContentsWithoutLinks block.body
        case lookup "__src_language" block.meta of
          Just lang -> do
            let colorized = renderAsText weaveConf $ colorizeText lang bodyText
                getLink = \case
                  BTokenLinkTarget lt ->
                    let body' = case lt of
                          LinkTargetOid (LinkOid _oid n) -> n
                          LinkTargetLineLabel n -> n
                          _ -> "weaveBlock error"
                        name' =
                          StyledText
                            { body = body',
                              style = styleMaskFrom []
                            }
                     in Just
                          $ Link
                            { target = lt,
                              name = [name']
                            }
                  BTokenString {}
                  BTokenPadding {} -> Nothing
                links = catMaybes $ map getLink block.body
            toHtmlRaw =<< spliceLinks colorized links
          Nothing -> code_ $ toHtml bodyText
  where
    maybeWeaveNameAndTanglePath
      | Just name <- lookup "name" block.meta,
        Just path <- lookup "tangle" block.meta =
          div_ [class_ "code-block-meta"] $ do
            code_ $ toHtml name
            br_ []
            code_ $ do
              span_ [class_ "bold"] $ do
                toHtmlRaw @Text "&#x1F3AF; "
                toHtml path
      | Just name <- lookup "name" block.meta =
          div_ [class_ "code-block-meta"] $ do
            code_ $ toHtml name
      | Just path <- lookup "tangle" block.meta =
          div_ [class_ "code-block-meta"] $ do
            code_ $ do
              span_ [class_ "bold"] $ do
                toHtmlRaw @Text "&#x1F3AF; "
                toHtml path
      | otherwise = pure ()
    getBlockContentsWithoutLinks :: [BToken] -> Text
    getBlockContentsWithoutLinks = T.concat . map bTokenToText
      where
        bTokenToText :: BToken -> Text
        bTokenToText = \case
          BTokenString text -> text
          BTokenPadding -> ""
          BTokenLinkTarget (LinkTargetOid _) -> "\NUL"
          BTokenLinkTarget (LinkTargetLineLabel _) -> "\NUL"
          BTokenLinkTarget {} -> ""
    linkAnchor = getLinkAnchor Nothing $ CBlock block

weaveBlockMeta :: (Text, Text) -> HtmlWeaver ()
weaveBlockMeta (k, v) = do
  div_ [class_ "block-meta-key-value"] $ do
    span_ [class_ "block-meta-key"] $ do
      toHtml k
    span_ [class_ "block-meta-separator"] $ do
      toHtmlRaw @Text " = "
    span_ [class_ "block-meta-value"] $ do
      toHtml v

colorizeText :: Text -> Text -> HtmlWeaver ()
colorizeText lang txt
  | Just syntax <- Sky.lookupSyntax lang Sky.defaultSyntaxMap,
    Just lines <- foldMap' Just $ Sky.tokenize defaultTokeniserConfig syntax txt =
      toHtmlRaw
        . Text.Blaze.Html.Renderer.Text.renderHtml
        $ Sky.formatHtmlInline Sky.defaultFormatOpts lines
  | otherwise = code_ $ toHtmlRaw txt
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
            : (renderAsText weaveConf $ weaveLink True link)
            : acc,
          links'
        )
  pure
    . T.intercalate ""
    . reverse
    . fst
    . foldl' f (take 1 colorizedStrs, links)
    $ drop 1 colorizedStrs

weaveLink :: Bool -> Link -> HtmlWeaver ()
weaveLink insideBlock link = do
  weaveConf <- ask
  wovenLinkTarget <- weaveLinkTarget link.target
  let attrs =
        [href_ wovenLinkTarget]
          <> linkCategory wovenLinkTarget
          <> lineLabelAttr
      lineLabelAttr =
        case link.target of
          LinkTargetLineLabel name ->
            if insideBlock
              then [id_ ("ib-" <> toSafeHtmlId name)] --
              else []
          _ -> []
      linkName =
        case link.target of
          LinkTargetLineLabel name
            | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
              Just num <- lookup name originObj.orgDoc.lineLabels ->
                [StyledText {body = T.show num, style = styleMaskFrom [Bold]}]
            | otherwise ->
                [StyledText {body = "weaveLink error", style = styleMaskFrom []}]
          _ -> link.name
  a_
    attrs
    $ sequence_
    $ map weaveStyledText linkName
  where
    linkCategory target
      | isExternalLink link.target =
          [ class_ "external-link",
            target_ "_blank",
            rel_ "noopener noreferrer"
          ]
      | "#ref-" `T.isPrefixOf` target = []
      | otherwise = [class_ "internal-link"]

weaveLinkTarget :: LinkTarget -> HtmlWeaver T.Text
weaveLinkTarget linkTarget = do
  weaveConf <- ask
  pure $ case linkTarget of
    LinkTargetOid linkOid
      | Just n <- lookup linkOid weaveConf.archive.symbols,
        n < 0,
        Just objPath <- (sort $ keys weaveConf.archive.objects) !!? (abs n - 1) ->
          if objPath == weaveConf.originObjectPath
            then "#top"
            else getExternalRelPath objPath weaveConf.originObjectPath
      | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
        Just _ <- lookup linkOid originObj.symbols,
        Just globalCellIdx <- lookup linkOid weaveConf.archive.symbols,
        Just (_, internalCell) <- cellIdxToObjCell weaveConf.archive globalCellIdx ->
          "#" <> getLinkAnchor Nothing internalCell
      | Just globalCellIdx <- lookup linkOid weaveConf.archive.symbols,
        Just (_, externalCell) <- cellIdxToObjCell weaveConf.archive globalCellIdx,
        Just (_, externalObjGPath) <- IntMap.lookupLE globalCellIdx weaveConf.archive.offsets ->
          (getExternalRelPath externalObjGPath weaveConf.originObjectPath)
            <> "#"
            <> getLinkAnchor Nothing externalCell
      | otherwise -> showBrokenLink
      where
        showBrokenLink = "#LILAC_BROKEN_LINK-" <> show linkOid
        htmlExt = P.unsafeEncodeUtf "html"
        getExternalRelPath extPath intPath
          | Just toObjPath <- P.encodeUtf $ decodeGPath extPath,
            Just fromObjPath <- P.encodeUtf $ decodeGPath intPath,
            Right str <-
              P.decodeWith utf8 utf8
                $ P.replaceExtension (getRelativePathWithoutSymlinks fromObjPath toObjPath) htmlExt =
              T.pack str
          | otherwise = showBrokenLink
    LinkTargetLineLabel name -> (T.pack "#ib-") <> toSafeHtmlId name
    LinkTargetFootnote name -> (T.pack "#footnote-") <> name
    LinkTargetFilePath path -> T.pack $ N.escapeURIString N.isUnescapedInURI path
    LinkTargetURI uri -> T.pack $ N.uriToString id uri ""

weaveFigure :: Int -> Figure -> HtmlWeaver ()
weaveFigure cellIdx figure = do
  wovenLinkTarget <- weaveLinkTarget figure.link.target
  let captionPrefix =
        "Figure "
          <> lookupDefault "figure-number-error" "__figure_number" figure.meta
  figure_ [id_ linkAnchor] $ do
    figcaption_ [] $ do
      wrapWith cellIdx $ do
        a_ [href_ $ "#" <> linkAnchor, class_ "self-link", class_ "caption-prefix"] $ do
          toHtml captionPrefix
        toHtml $ case figure.caption.unProse of
          [] -> "." :: Text
          _ -> ". " :: Text
        weaveProse figure.caption
    img_
      $ [ id_ linkAnchor,
          src_ wovenLinkTarget,
          class_ "figure-image"
        ]
      <> ( catMaybes
             $ map
               (\(attr, metaKey) -> attr <$> lookup metaKey figure.meta)
               [ (width_, "width"),
                 (height_, "height"),
                 (alt_, "caption"),
                 (title_, "name")
               ]
         )
  where
    linkAnchor = getLinkAnchor Nothing $ CFigure figure

weaveMathFragmentStandalone :: Int -> MathFragment -> HtmlWeaver ()
weaveMathFragmentStandalone cellIdx mathFragment = do
  let (begMarker, endMarker)
        | Just _ <- lookup "__equation_number" mathFragment.meta =
            ("\\begin{equation}", "\\end{equation}")
        | otherwise = ("\\[", "\\]")
  p_ [id_ linkAnchor] $ do
    wrapWith cellIdx $ do
      a_ [href_ $ "#" <> linkAnchor, class_ "self-link"] $ do
        begMarker <> toHtml mathFragment.body <> endMarker
  where
    linkAnchor = getLinkAnchor Nothing $ CMathFragment mathFragment

weaveMathFragmentInline :: MathFragment -> HtmlWeaver ()
weaveMathFragmentInline mathFragment = "\\(" <> toHtml mathFragment.body <> "\\)"

weaveTable :: Int -> Table -> HtmlWeaver ()
weaveTable cellIdx table = do
  let captionPrefix =
        "Table "
          <> lookupDefault "table-number-error" "__table_number" table.meta
  figure_ [id_ linkAnchor] $ do
    figcaption_ [] $ do
      wrapWith cellIdx $ do
        a_ [href_ $ "#" <> linkAnchor, class_ "self-link", class_ "caption-prefix"] $ do
          toHtml captionPrefix
        toHtml $ case table.caption.unProse of
          [] -> "." :: Text
          _ -> ". " :: Text
        weaveProse table.caption
    table_ [] $ do
      forM_ (zip [1 :: Int ..] getColgroups) $ \(groupIdx, numCols) -> do
        let colgroupAttrs =
              if groupIdx > 1
                then [class_ "table-vertical-line-colgroup"]
                else []
        colgroup_ colgroupAttrs $ do
          forM_ [1 .. numCols] $ \colNum -> do
            if (colNum > 1)
              then col_ [class_ "table-vertical-line-col"]
              else col_ []
      thead_ [] $ do
        tr_ [] $ do
          forM_ columnNames $ \prose -> do
            th_ [] $ weaveProse prose
      tbody_ [] $ do
        forM_ (rows visibleRows) $ \row ->
          if row == [Prose []]
            then do
              tr_ [class_ "table-horizontal-line"] $ do
                forM_ columnNames $ \_ -> do
                  td_ [] $ toHtml @Text ""
            else do
              tr_ [class_ "table-body-row"] $ do
                forM_ row $ \prose -> do
                  let tdAttrs =
                        if mostlyNumeric prose
                          then [class_ "table-cell-numeric"]
                          else []
                  td_ tdAttrs $ weaveProse prose
  where
    linkAnchor = getLinkAnchor Nothing $ CTable table
    hideMarkingCharInFirstColumn :: [((Int, Int), Prose)] -> [Prose]
    hideMarkingCharInFirstColumn = \case
      [] -> []
      (((_row, _col), prose) : cells)
        | prose == tableHorizontalLine -> [Prose []]
        | markingCharExistsInFirstColumn -> map snd cells
        | otherwise -> prose : map snd cells
    markingCharExistsInFirstColumn = any (not . visibleColumn) firstColumnCells
    tableCellsFromRow :: Int -> [Prose]
    tableCellsFromRow n =
      hideMarkingCharInFirstColumn
        . M.toList
        $ M.filterWithKey (\(row, _col) _ -> row == n) table.grid
    rowMax = foldl' max 0 . map fst $ keys table.grid
    rows rowNums = map tableCellsFromRow rowNums
    columnNames = tableCellsFromRow headerRowIndex
    firstColumnCells =
      M.toList
        $ M.filterWithKey (\(_row, col) _ -> col == 1) table.grid
    headerRowIndex = fromMaybe 1 $ foldl' f Nothing firstColumnCells
    f acc@(Just _) _ = acc
    f acc cell@((row, _col), _)
      | visibleColumn cell = Just row
      | otherwise = acc
    visibleColumn :: ((Int, Int), Prose) -> Bool
    visibleColumn (_, prose) = case prose.unProse of
      [] -> True
      [PTokenStyledText styledText] ->
        notElem styledText.body $ map T.singleton markingChars
      _ -> True
    visibleRow :: ((Int, Int), Prose) -> Bool
    visibleRow (_, prose) = case prose.unProse of
      [] -> True
      [PTokenStyledText styledText] ->
        notElem styledText.body $ map T.singleton markingCharsInvisible
      _ -> True
    visibleRows = filter (`notElem` invisibleRows) [(headerRowIndex + 1) .. rowMax]
    invisibleRows = foldl' g [] firstColumnCells
    g acc cell@((row, _col), _)
      | visibleRow cell = acc
      | otherwise = row : acc
    getColgroups :: [Int]
    getColgroups =
      maybe
        [rowMax]
        (reverse . foldl' getColgroupStarts [0] . tableCellsFromRow)
        getColgroupsRow
    getColgroupStarts :: [Int] -> Prose -> [Int]
    getColgroupStarts ns prose = case ns of
      [] -> []
      [0] -> [1]
      acc@(latest : rest)
        | proseHasColgroupStartMarker prose -> 1 : acc
        | otherwise -> latest + 1 : rest
    getColgroupsRow = foldl' h Nothing firstColumnCells
    h :: Maybe Int -> ((Int, Int), Prose) -> Maybe Int
    h acc ((row, _col), prose)
      | isJust acc = acc
      | proseHasNonExportMarker prose = Just row
      | otherwise = acc
    proseHas :: Text -> Prose -> Bool
    proseHas prefix prose = case prose.unProse of
      [] -> False
      (firstPToken : _) -> case firstPToken of
        PTokenStyledText styledText ->
          (maskToStyles styledText.style == [])
            && T.isPrefixOf prefix styledText.body
        PTokenLink {}
        PTokenMathFragment {}
        PTokenCitation {}
        PTokenDivStart {}
        PTokenDivEnd {} -> False
    proseHasColgroupStartMarker :: Prose -> Bool
    proseHasColgroupStartMarker = proseHas "<"
    proseHasNonExportMarker :: Prose -> Bool
    proseHasNonExportMarker = proseHas "/"
    mostlyNumeric :: Prose -> Bool
    mostlyNumeric prose =
      let rawText = T.concat $ map extractText prose.unProse
       in isNumericText rawText
    extractText = \case
      PTokenStyledText styledText -> styledText.body
      PTokenLink {}
      PTokenMathFragment {}
      PTokenCitation {}
      PTokenDivStart {}
      PTokenDivEnd {} -> ""

getRelativePathWithoutSymlinks :: OsPath -> OsPath -> OsPath
getRelativePathWithoutSymlinks src dest
  | dirsSrc == dirsDest || isDestChildOfSrc =
      P.joinPath $ remainingDest <> destFile
  | null commonPathPrefix =
      P.joinPath $ replicate (length dirsSrc) goUp <> destFile
  | otherwise =
      P.joinPath
        $ replicate (length dirsSrc - length commonPathPrefix) goUp
        <> drop (length commonPathPrefix) dirsDest
        <> destFile
  where
    goUp = P.unsafeEncodeUtf ".."
    destFile = [P.takeFileName dest]
    dirSrc = P.takeDirectory src
    dirDest = P.takeDirectory dest
    dirsSrc = P.splitDirectories dirSrc
    dirsDest = P.splitDirectories dirDest
    commonPathPrefix = commonPrefix dirsSrc dirsDest
    isDestChildOfSrc = dirsSrc == commonPathPrefix
    remainingDest = drop (length commonPathPrefix) dirsDest

getRelativeCssPath :: HtmlWeaver Text
getRelativeCssPath = do
  weaveConf <- ask
  let result :: Maybe String
      result = do
        objPath <- P.encodeUtf $ decodeGPath weaveConf.originObjectPath
        out <- P.encodeUtf $ decodeGPath weaveConf.outDir
        cssDir <- P.encodeUtf "css"
        css <- P.encodeUtf "default.css"
        let cssPath = P.joinPath [out, cssDir, css]
            relativeCssPath = getRelativePathWithoutSymlinks objPath cssPath
        final <- P.decodeUtf relativeCssPath
        pure final
  pure . T.pack $ fromMaybe "css/default.css" result

getRelativeJsPath :: HtmlWeaver Text
getRelativeJsPath = do
  weaveConf <- ask
  let result :: Maybe String
      result = do
        objPath <- P.encodeUtf $ decodeGPath weaveConf.originObjectPath
        out <- P.encodeUtf $ decodeGPath weaveConf.outDir
        jsDir <- P.encodeUtf "js"
        js <- P.encodeUtf "lilac.js"
        let jsPath = P.joinPath [out, jsDir, js]
            relativeJsPath = getRelativePathWithoutSymlinks objPath jsPath
        final <- P.decodeUtf relativeJsPath
        pure final
  pure . T.pack $ fromMaybe "lilac.js" result

weaveFootnote :: Int -> Footnote -> HtmlWeaver ()
weaveFootnote cellIdx footnote = do
  weaveConf <- ask
  if
    | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
      Just (footnoteNumber, referrerCellIdx) <- lookup footnote.name originObj.orgDoc.footnoteRefs -> do
        p_ [class_ "footnote-definition", id_ ("footnote-" <> footnote.name)] $ do
          wrapWith cellIdx $ do
            span_ [class_ "footnote-number"] . toHtml $ T.show footnoteNumber <> ". "
            weaveProse footnote.body
            weaveFootnoteBacklink referrerCellIdx --
    | otherwise -> pure ()

weaveFootnoteBacklink :: Int -> HtmlWeaver ()
weaveFootnoteBacklink localCellIdx = do
  weaveConf <- ask
  if
    | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
      Just internalCell <- originObj.orgDoc.cells !!? localCellIdx,
      Just name <- cellIdxToName weaveConf.archive 0 localCellIdx -> do
        toHtml @Text " "
        let linkAnchor = "#" <> getLinkAnchor (Just localCellIdx) internalCell
        a_ [href_ linkAnchor, class_ "footnote-backlink"] $ do
          toHtml @Text "↑ "
          weaveProse name
    | otherwise -> pure ()

weaveGlossaryTerm :: Int -> GlossaryTerm -> HtmlWeaver ()
weaveGlossaryTerm cellIdx glossaryTerm = do
  weaveConf <- ask
  let dlStart :: HtmlWeaver ()
      dlStart = toHtmlRaw @Text "<dl>"
      dlEnd :: HtmlWeaver ()
      dlEnd = toHtmlRaw @Text "</dl>"
      isGlossaryTerm idx --
        | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
          Just internalCell <- originObj.orgDoc.cells !!? idx,
          CGlossaryTerm _ <- internalCell =
            True
        | otherwise = False
      isFirstGlossaryTerm = not . isGlossaryTerm $ cellIdx - 1
      isLastGlossaryTerm = not . isGlossaryTerm $ cellIdx + 1
  when (isFirstGlossaryTerm) dlStart --
  wrapWithDiv cellIdx $ do
    div_ [id_ linkAnchor, class_ "glossary-term"] $ do
      let maybeInnerVerticalMargin
            | isFirstGlossaryTerm = []
            | otherwise = [class_ "inner-dt"]
      dt_ maybeInnerVerticalMargin $ do
        weaveProse glossaryTerm.term
      dd_ $ do
        a_ [href_ $ "#" <> linkAnchor, class_ "glossary-term-anchor"] $ do
          toHtmlRaw @Text "&#x1F517;"
        weaveProse glossaryTerm.definition
  when (isLastGlossaryTerm) dlEnd --
  where
    linkAnchor = getLinkAnchor (Just cellIdx) $ CGlossaryTerm glossaryTerm

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix xs ys = map fst . takeWhile (\(x, y) -> (x == y)) $ zip xs ys

renderAsText :: WeaveConf -> HtmlWeaver a -> Text
renderAsText weaveConf weaver =
  TL.toStrict
    . renderText
    $ hoistHtmlT (flip runReaderT weaveConf) weaver

proseToCslJsonText :: Prose -> CslJson Text
proseToCslJsonText prose = foldl' f CslEmpty prose.unProse
  where
    f :: CslJson Text -> PToken -> CslJson Text
    f acc pToken = case pToken of
      PTokenStyledText styledText ->
        let styles = maskToStyles styledText.style
            body = parseCslJson mempty styledText.body
         in acc <> applyStyles body styles
      PTokenLink link ->
        let encoded = CslLink ("LILAC_LINK\NUL" <> unparseLink link) CslEmpty
         in acc <> encoded
      PTokenMathFragment mathFragment ->
        let encoded = CslDiv ("LILAC_MATH_FRAGMENT\NUL" <> unparseMathFragment mathFragment) CslEmpty
         in acc <> encoded
      PTokenCitation {}
      PTokenDivStart {}
      PTokenDivEnd -> acc
    applyStyles :: CslJson Text -> [Style] -> CslJson Text
    applyStyles cslBody styles = foldl' applyStyle cslBody styles
    applyStyle :: CslJson Text -> Style -> CslJson Text
    applyStyle acc = \case
      Bold -> CslBold acc
      Italic -> CslItalic acc
      Underline -> CslUnderline acc
      Verbatim -> CslDiv "LILAC_STYLE_VERBATIM\NUL" acc
      Code -> CslDiv "LILAC_STYLE_CODE\NUL" acc
      Smallcaps -> CslSmallCaps acc
      Subscript -> CslSub acc
      Superscript -> CslSup acc

proseToMaybeCslJsonText :: Prose -> Maybe (CslJson Text)
proseToMaybeCslJsonText prose =
  case proseToCslJsonText prose of
    CslEmpty -> Nothing
    cslJson -> Just cslJson

citationToCiteprocCitation :: Citation -> CP.Citation (CslJson Text)
citationToCiteprocCitation citation =
  CP.Citation
    { CP.citationId = Nothing,
      CP.citationNoteNumber = citation.noteNumber,
      CP.citationPrefix = proseToMaybeCslJsonText citation.prefix,
      CP.citationSuffix = proseToMaybeCslJsonText citation.suffix,
      CP.citationItems = concatMap (citeToCiteprocCitationItems sv) citation.cites
    }
  where
    (sv, _) = citation.styleVariation

citeToCiteprocCitationItems :: Maybe Text -> Cite -> [CP.CitationItem (CslJson Text)]
citeToCiteprocCitationItems sv cite =
  let citationItem =
        CP.CitationItem
          { CP.citationItemId = CP.ItemId cite.id,
            CP.citationItemLabel = term,
            CP.citationItemLocator = coord,
            CP.citationItemType = CP.NormalCite,
            CP.citationItemPrefix = proseToMaybeCslJsonText cite.prefix,
            CP.citationItemSuffix = proseToMaybeCslJsonText cite.suffix,
            CP.citationItemData = Nothing
          }
   in if
        | Just "t" <- sv ->
            [ citationItem
                { CP.citationItemType = CP.AuthorOnly,
                  CP.citationItemSuffix = Nothing
                },
              citationItem
                { CP.citationItemType = CP.SuppressAuthor,
                  CP.citationItemPrefix = Nothing
                }
            ]
        | Just "na" <- sv ->
            [citationItem {CP.citationItemType = CP.SuppressAuthor}]
        | otherwise -> [citationItem]
  where
    (term, coord) = case cite.locator of
      Just (t, c) -> (Just t, Just c)
      Nothing -> (Nothing, Nothing)

cslJsonTextToProse :: CslJson Text -> Prose
cslJsonTextToProse cslJsonText = compressProse . Prose $ f [] cslJsonText
  where
    f :: [Style] -> CslJson Text -> [PToken]
    f s = \case
      CslEmpty -> []
      CslText t -> [PTokenStyledText StyledText {body = t, style = styleMaskFrom s}]
      CslConcat a CslEmpty -> f s a
      CslConcat (CslConcat a b) c -> f s (CslConcat a (CslConcat b c))
      CslConcat a b -> f s a <> f s b
      CslQuoted a -> f s a
      CslItalic a -> f (Italic : s) a
      CslNormal a -> f [] a
      CslBold a -> f (Bold : s) a
      CslUnderline a -> f (Underline : s) a
      CslNoDecoration a -> f (filter (/= Underline) s) a
      CslSmallCaps a -> f (Smallcaps : s) a
      CslBaseline a -> f (filter (`notElem` [Subscript, Superscript]) s) a
      CslSup a -> f (Superscript : s) a
      CslSub a -> f (Subscript : s) a
      CslNoCase a -> f s a
      CslDiv t a
        | t == "LILAC_STYLE_CODE\NUL" -> f (Code : s) a
        | t == "LILAC_STYLE_VERBATIM\NUL" -> f (Verbatim : s) a
        | "LILAC_MATH_FRAGMENT\NUL" `T.isPrefixOf` t ->
            (CP.fromText (T.drop 20 t) :: Prose).unProse
        | otherwise -> [PTokenDivStart t] <> f s a <> [PTokenDivEnd]
      CslLink t a
        | "LILAC_LINK\NUL" `T.isPrefixOf` t ->
            (CP.fromText (T.drop 11 t) :: Prose).unProse
        | "#ref-" `T.isPrefixOf` t ->
            let uri =
                  N.URI
                    { N.uriScheme = "",
                      N.uriAuthority = Nothing,
                      N.uriPath = T.unpack t,
                      N.uriQuery = "",
                      N.uriFragment = ""
                    }
                target = LinkTargetURI uri
                name = compressStyledText . getStyledText $ f s a
             in [PTokenLink Link {target = target, name = name}]
        | otherwise -> case (N.parseURI $ T.unpack t) of
            Nothing -> []
            Just uri ->
              let target = LinkTargetURI uri
                  name = compressStyledText . getStyledText $ f s a
               in [PTokenLink Link {target = target, name = name}]
    getStyledText [] = []
    getStyledText (pToken : rest) = case pToken of
      PTokenStyledText styledText -> styledText : getStyledText rest
      PTokenLink {}
      PTokenMathFragment {}
      PTokenCitation {}
      PTokenDivStart {}
      PTokenDivEnd {} -> []

prepareCitations :: GPath -> Object -> Archive -> IO (CP.Result Prose)
prepareCitations objGPath object archive = do
  bibPathsRelative <- makeRelativeToObjectPath objGPath object.orgDoc.bibPaths
  cslPathRelative <- listToMaybe <$> (makeRelativeToObjectPath objGPath $ maybeToList object.orgDoc.cslPath)
  let bibRefsFromOrgDoc =
        filter neededByCitations
          . concat
          . catMaybes
          $ map (\bp -> lookup bp archive.bibs) bibPathsRelative
      CP.Result citations bibliography warnings =
        f bibRefsFromOrgDoc cslPathRelative
      resultWithProse =
        CP.Result
          { CP.resultCitations = map cslJsonTextToProse citations,
            CP.resultBibliography = map (\(k, cslJsonText) -> (k, cslJsonTextToProse cslJsonText)) bibliography,
            CP.resultWarnings = warnings
          }
  pure resultWithProse
  where
    citeprocOptions =
      CP.CiteprocOptions
        { linkCitations = True,
          linkBibliography = True
        }
    blankResult :: CP.Result (CslJson Text)
    blankResult =
      CP.Result
        { CP.resultCitations = [],
          CP.resultBibliography = [],
          CP.resultWarnings = ["BLANK CITATION RESULT"]
        }
    f bibRefs cslPathRelative
      | Just cslPath' <- cslPathRelative,
        Just rawXML <- lookup cslPath' archive.csls,
        Right citeprocStyle <- parseCitationStyle rawXML =
          CP.citeproc
            citeprocOptions
            citeprocStyle
            Nothing
            bibRefs
            (map citationToCiteprocCitation object.orgDoc.citations)
      | otherwise = blankResult
    neededByCitations bibRef =
      let citeIds =
            concatMap
              (\c -> map (\cite -> cite.id) c.cites)
              object.orgDoc.citations
       in bibRef.referenceId.unItemId `elem` citeIds

weaveCitation :: Int -> HtmlWeaver ()
weaveCitation n = do
  weaveConf <- ask
  let result = weaveConf.citeprocResult
      citations = result.resultCitations
  case maybeAt n citations of
    Just prose -> span_ [class_ "citation"] $ weaveProse prose
    Nothing -> pure ()

weaveBibliography :: Int -> HtmlWeaver ()
weaveBibliography cellIdx = do
  weaveConf <- ask
  let result = weaveConf.citeprocResult
      bibliography = result.resultBibliography
  div_ [class_ "bibliography"] $ do
    wrapWithDiv cellIdx $ do
      forM_ bibliography $ \(key, prose) -> do
        let linkAnchor = "ref-" <> key
        div_ [class_ "bib-ref", id_ linkAnchor] $ do
          weaveProse prose

weaveGlossary :: Bool -> HtmlWeaver ()
weaveGlossary isGlobal = do
  weaveConf <- ask
  if
    | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects -> do
        let terms :: [(Int, GPath)]
            terms
              | isGlobal =
                  map (\((_term, path), localCellIdx) -> (localCellIdx, path))
                    $ sortBy
                      ( comparing (toNormalizedText . CP.fromText . fst . fst)
                          <> comparing (snd . fst)
                      )
                    $ M.toList weaveConf.archive.glossary
              | otherwise =
                  map (\(_term, localCellIdx) -> (localCellIdx, weaveConf.originObjectPath))
                    $ sortBy
                      (comparing (toNormalizedText . CP.fromText . fst)) --
                    $ M.toList originObj.orgDoc.glossary
            terms' = zip (True : repeat False) terms
        when (terms' /= []) $ toHtmlRaw @Text "<dl>"
        forM_ terms' weaveCollectedGlossaryTerm
        when (terms' /= []) $ toHtmlRaw @Text "</dl>"
    | otherwise -> pure ()

weaveCollectedGlossaryTerm :: (Bool, (Int, GPath)) -> HtmlWeaver ()
weaveCollectedGlossaryTerm (isFirstTerm, (localCellIdx, objPath)) = do
  weaveConf <- ask
  if
    | True <- objPath == weaveConf.originObjectPath, --
      Just originObj <-
        lookup weaveConf.originObjectPath weaveConf.archive.objects,
      Just internalCell <- originObj.orgDoc.cells !!? localCellIdx,
      CGlossaryTerm glossaryTerm <- internalCell -> do
        let linkAnchor = getLinkAnchor (Just localCellIdx) internalCell
        dt_ maybeInnerVerticalMargin $ do
          weaveProse glossaryTerm.term
        dd_ $ do
          a_ [href_ $ "#" <> linkAnchor] $ do
            toHtml @Text "↑"
          weaveProse glossaryTerm.definition
    | True <- objPath /= weaveConf.originObjectPath, --
      Just extObj <- lookup objPath weaveConf.archive.objects,
      Just externalCell <- extObj.orgDoc.cells !!? localCellIdx,
      CGlossaryTerm glossaryTerm <- externalCell -> do
        (externalLinkAnchor, externalOrgDocName) <-
          getRelativeOrgDocCellLink objPath localCellIdx externalCell
        dt_ maybeInnerVerticalMargin $ do
          weaveProse glossaryTerm.term
        dd_ $ do
          a_ [href_ externalLinkAnchor] $ do
            span_ [class_ "glossary-external-source"] $ do
              weaveProse
                $ Prose
                  [ PTokenStyledText
                      StyledText
                        { body = "[" <> externalOrgDocName <> "]", --
                          style = styleMaskFrom []
                        }
                  ]
          weaveProse glossaryTerm.definition
    | otherwise -> toHtml @Text "error: glossary term processing error"
  where
    maybeInnerVerticalMargin =
      if isFirstTerm
        then []
        else [class_ "inner-dt"]

getRelativeOrgDocCellLink :: GPath -> Int -> Cell -> HtmlWeaver (Text, Text)
getRelativeOrgDocCellLink objPath externalCellIdx externalCell = do
  (externalOrgDocName, externalOrgDocPath) <- getRelativeOrgDocLink objPath
  let externalLinkAnchor =
        externalOrgDocPath
          <> "#"
          <> (getLinkAnchor (Just externalCellIdx) externalCell)
  pure (externalLinkAnchor, externalOrgDocName)

getRelativeOrgDocLink :: GPath -> HtmlWeaver (Text, Text)
getRelativeOrgDocLink objPath = do
  weaveConf <- ask
  if
    | Just toObjPath <- P.encodeUtf $ decodeGPath objPath,
      Just fromObjPath <- P.encodeUtf $ decodeGPath weaveConf.originObjectPath,
      Right externalOrgDocName <-
        P.decodeWith utf8 utf8
          $ P.replaceExtension
            (getRelativePathWithoutSymlinks fromObjPath toObjPath)
            (P.unsafeEncodeUtf "org"),
      Right externalOrgDocPath <-
        P.decodeWith utf8 utf8
          $ P.replaceExtension
            (getRelativePathWithoutSymlinks fromObjPath toObjPath)
            (P.unsafeEncodeUtf "html") ->
        pure (T.pack externalOrgDocName, T.pack externalOrgDocPath)
    | otherwise -> pure ("ERROR", "???")

toNormalizedText :: Prose -> Text
toNormalizedText prose =
  T.toLower
    . CP.toText @Prose
    . stripStyles
    $ stripLinks prose

weaveToc :: [Cell] -> HtmlWeaver ()
weaveToc cells = do
  nav_ [class_ "toc"] $ do
    p_ [class_ "widget-title"] $ do
      toHtml @Text "Contents"
    let (actions, lastHeadingLevel) = foldl' f ([], 0) cells
    sequence_ $ reverse actions
    when (lastHeadingLevel > 0) $ do
      closeHeadingLevels lastHeadingLevel
  where
    f :: ([HtmlWeaver ()], Int) -> Cell -> ([HtmlWeaver ()], Int)
    f (actions, prevLevel) = \case
      CHeading heading ->
        let weaveTocHeading :: HtmlWeaver ()
            weaveTocHeading = do
              toHtmlRaw ("<li class=\"toc-level-" <> show heading.level <> "\">" :: Text)
              div_ [class_ "toc-entry"] $ do
                span_ [class_ "toc-counter"] $ do
                  toHtml sectionText
                a_ [href_ $ "#" <> linkAnchor] $ do
                  weaveProse heading.body
            sectionText = getSectionNumber heading
            linkAnchor = getLinkAnchor Nothing $ CHeading heading
            action :: HtmlWeaver ()
            action
              | prevLevel < heading.level = do
                  weaveListMarker LOrderedStart
                  weaveTocHeading
              | prevLevel > heading.level = do
                  closeHeadingLevels (prevLevel - heading.level)
                  weaveListMarker LItemEnd
                  weaveTocHeading
              | otherwise = do
                  weaveListMarker LItemEnd
                  weaveTocHeading
         in (action : actions, heading.level)
      CFootnote {}
      CBlock {}
      CFigure {}
      CListItem {}
      CMathFragment {}
      CParagraph {}
      CGlossaryTerm {}
      CTable {}
      CCommand {} -> (actions, prevLevel)
    closeHeadingLevels :: Int -> HtmlWeaver ()
    closeHeadingLevels levelDiff =
      forM_ [1 .. levelDiff] $ \_ -> do
        weaveListMarker LItemEnd
        weaveListMarker LOrderedEnd

genHeadingAncestry :: [Cell] -> Map Text [Text]
genHeadingAncestry = snd . foldl' f ([], mempty)
  where
    f ::
      ([Heading], Map Text [Text]) ->
      Cell ->
      ([Heading], Map Text [Text])
    f ([], m) cell@(CHeading heading) =
      let updatedAncestry = [heading]
          key = getLinkAnchor Nothing cell
          val = [] :: [Text]
       in (updatedAncestry, insert key val m)
    f (ancestry@(a : as), m) cell@(CHeading heading) =
      let updatedAncestry
            | heading.level < a.level =
                heading
                  : (dropWhile (\a' -> heading.level <= a'.level) ancestry)
            | heading.level > a.level = heading : ancestry
            | otherwise = heading : as
          key = getLinkAnchor Nothing cell
          val =
            map
              (getLinkAnchor Nothing . CHeading)
              $ ancestorsOf heading ancestry
       in (updatedAncestry, insert key val m)
    f acc _ = acc
    ancestorsOf :: Heading -> [Heading] -> [Heading]
    ancestorsOf leafHeading ancestry
      | null ancestry = []
      | otherwise = dropWhile (\a' -> leafHeading.level <= a'.level) ancestry

weaveHeadingAncestry :: [(Int, Cell)] -> HtmlWeaver ()
weaveHeadingAncestry rearrangedCells = do
  let headingAncestry = genHeadingAncestry $ map snd rearrangedCells
  script_ [id_ "heading-ancestry", type_ "application/json"] $ do
    toHtmlRaw . TLE.decodeUtf8 $ encode headingAncestry

parseRawProjectConf :: Text -> Either String RawProjectConf
parseRawProjectConf = eitherDecodeStrictText

parseProjectConf :: Text -> Either String ProjectConf
parseProjectConf input = case parseRawProjectConf input of
  Left errMsg -> Left errMsg
  Right rawProjectConf ->
    Right
      ProjectConf
        { name = rawProjectConf.name,
          homepage = N.parseURI $ T.unpack rawProjectConf.homepage,
          repo = N.parseURI $ T.unpack rawProjectConf.repo,
          version =
            if T.null rawProjectConf.version
              then Nothing
              else Just rawProjectConf.version
        }

weaveNavbar :: HtmlWeaver ()
weaveNavbar = do
  div_ [class_ "navbar"] $ do
    div_ [class_ "breadcrumbs"] $ do
      weaveBreadcrumbs

weaveBreadcrumbs :: HtmlWeaver ()
weaveBreadcrumbs = do
  weaveConf <- ask
  weaveProjectName weaveConf.projectConf
  weaveProjectVersion weaveConf.projectConf
  let breadcrumbsTail =
        map osPathDecoder
          . P.splitDirectories
          . P.unsafeEncodeUtf
          $ decodeGPath weaveConf.originObjectPath
      breadcrumbsMiddle = reverse . drop 1 $ reverse breadcrumbsTail
      breadcrumbLast =
        convertObjectExtensionToOrg . take 1 $ reverse breadcrumbsTail
  forM_ breadcrumbsMiddle $ \breadcrumb -> do
    weaveBreadcrumbSeparator
    weaveBreadcrumb breadcrumb
  forM_ breadcrumbLast $ \breadcrumb -> do
    weaveBreadcrumbSeparator
    a_ [href_ "#top"] $ do
      weaveBreadcrumb breadcrumb
  where
    osPathDecoder osp = case P.decodeUtf osp :: Maybe FilePath of
      Nothing -> "LILAC_ERROR_INVALID_OSPATH"
      Just fp -> T.pack fp
    weaveProjectName :: ProjectConf -> HtmlWeaver ()
    weaveProjectName projectConf = case projectConf.homepage of
      Nothing -> do
        weaveBreadcrumb projectConf.name
      Just uri -> do
        let homepageLinkTarget = T.pack $ N.uriToString id uri ""
        a_ [href_ homepageLinkTarget] $ do
          weaveBreadcrumb projectConf.name
    weaveProjectVersion :: ProjectConf -> HtmlWeaver ()
    weaveProjectVersion projectConf = case projectConf.version of
      Nothing -> pure ()
      Just version -> toHtml $ " " <> version
    weaveBreadcrumb :: Text -> HtmlWeaver ()
    weaveBreadcrumb breadcrumb = do
      span_ [class_ "breadcrumb"] $ do
        toHtml breadcrumb
    weaveBreadcrumbSeparator :: HtmlWeaver ()
    weaveBreadcrumbSeparator = do
      toHtml @Text " "
      span_ [class_ "breadcrumb-separator"] $ do
        toHtml @Text "/"
      toHtml @Text " "
    convertObjectExtensionToOrg :: [Text] -> [Text]
    convertObjectExtensionToOrg [] = []
    convertObjectExtensionToOrg (objName : rest) =
      let parts = T.split (== '.') objName
          withoutObjectExtension = drop 1 $ reverse parts
          withOrgExtension = ".org" : withoutObjectExtension
       in (T.concat $ reverse withOrgExtension) : rest

weaveFooter :: HtmlWeaver ()
weaveFooter = do
  footer_ $ do
    div_ [class_ "lilac-stamp"] $ do
      toHtml @Text "Generated by "
      a_
        [ href_ "https://github.com/listx/lilac",
          class_ "external-link",
          target_ "_blank",
          rel_ "noopener noreferrer"
        ]
        $ do
          toHtml @Text "Lilac"

weaveMain :: Map Text Text -> [(Int, Cell)] -> HtmlWeaver ()
weaveMain docMeta rearrangedCells = do
  main_ $ do
    weaveDocMeta docMeta
    forM_ rearrangedCells weaveCell
    p_ [class_ "main-bottom-spacer"] $ toHtml @Text ""
    let isHeading = \case
          CHeading {} -> True
          _ -> False
    when (any isHeading $ map snd rearrangedCells) $ toHtmlRaw @Text "</div>"

weaveSidebar :: [Block] -> HtmlWeaver ()
weaveSidebar blocks = do
  aside_ [class_ "sidebar"] $ do
    weaveTangledBlocksList blocks
    weaveBacklinks
    weaveNamedCellsIndex
    weaveFooter

weaveTangledBlocksList :: [Block] -> HtmlWeaver ()
weaveTangledBlocksList blocks = do
  div_ [class_ "tangled-blocks"] $ do
    p_ [class_ "widget-title"] $ do
      toHtml @Text "Tangled files"
    let tangledBlocks = catMaybes $ map getTangled blocks
    if (null tangledBlocks)
      then do
        p_ [] $ do
          toHtml @Text "None"
      else do
        ol_ [] $ do
          forM_ (sort tangledBlocks) $ \(path, block) -> do
            li_ [] $ do
              let linkAnchor = getLinkAnchor Nothing $ CBlock block
              a_ [href_ $ "#" <> linkAnchor, class_ "verbatim"] $ do
                toHtml path
  where
    getTangled :: Block -> Maybe (Text, Block)
    getTangled b
      | Just path <- lookup "tangle" b.meta =
          Just (path, b)
      | otherwise = Nothing

weaveBacklinks :: HtmlWeaver ()
weaveBacklinks = do
  weaveConf <- ask
  div_ [class_ "backlinks"] $ do
    p_ [class_ "widget-title"] $ do
      toHtml @Text "Backlinks"
    case lookup weaveConf.originObjectPath weaveConf.archive.backlinks of
      Nothing -> do
        p_ [] $ do
          toHtml @Text "None"
      Just backlinksByPath -> do
        ol_ [] $ do
          let sorted =
                sortOn (\(_, links) -> -(length links))
                  $ toPairs backlinksByPath
          sequence_ $ map weaveBacklinksForExternalObj sorted
  where
    weaveBacklinksForExternalObj :: (GPath, [G.Edge]) -> HtmlWeaver ()
    weaveBacklinksForExternalObj (_, []) = toHtmlRaw @Text ""
    weaveBacklinksForExternalObj (externalObjGPath, edges) = do
      weaveConf <- ask
      (externalOrgDocName, externalOrgDocPath) <- getRelativeOrgDocLink externalObjGPath
      let weight = T.show $ length edges
          f :: ([Int], Int) -> HtmlWeaver ()
          f ([], _) = pure ()
          f (externalIdxs@(externalIdx : _), internalIdx) = do
            let (internalLinkAnchor, internalName) =
                  (getInternalLinkAnchor internalIdx, getInternalName internalIdx)
            li_ [] $ do
              if (length externalIdxs > 1)
                then do
                  a_ [href_ internalLinkAnchor] $ do
                    weaveProse internalName
                    toHtml @Text $ " (" <> (T.show $ length externalIdxs) <> ")"
                  ol_ [] $ do
                    sequence_ $ map (linkToExternalCell True) externalIdxs
                else do
                  a_ [href_ internalLinkAnchor] $ do
                    weaveProse internalName
                  span_ [class_ "verbatim"] $ do
                    toHtmlRaw @Text " &#x2190; "
                  linkToExternalCell False externalIdx
          linkToExternalCell :: Bool -> Int -> HtmlWeaver ()
          linkToExternalCell asListItem externalIdx
            | Just (_, externalCell) <-
                cellIdxToObjCell weaveConf.archive externalIdx,
              Just (offset, _) <-
                IntMap.lookupLE externalIdx weaveConf.archive.offsets = do
                let externalName = fromMaybe noName $ cellIdxToName weaveConf.archive offset externalIdx
                    externalCellIdx = externalIdx - offset
                (externalLinkAnchor, _) <- getRelativeOrgDocCellLink externalObjGPath externalCellIdx externalCell
                let externalLink =
                      a_ [href_ externalLinkAnchor] $ do
                        weaveProse externalName
                if asListItem
                  then li_ [] $ externalLink
                  else externalLink
            | otherwise = pure ()
          getInternalLinkAnchor internalIdx
            | Just (_, internalCell) <- cellIdxToObjCell weaveConf.archive internalIdx =
                "#" <> (getLinkAnchor Nothing internalCell)
            | internalIdx < 0 = "#top"
            | otherwise = "#LINK_ERROR"
          getInternalName internalIdx
            | Just name <- cellIdxToName weaveConf.archive 0 internalIdx =
                name
            | internalIdx < 0 = Prose [PTokenStyledText $ StyledText {body = "^ Top", style = styleMaskFrom []}]
            | otherwise = noName
          noName = Prose [PTokenStyledText $ StyledText {body = "???", style = styleMaskFrom []}]
          groupedByTarget :: [G.Edge] -> [([Int], Int)]
          groupedByTarget =
            let coalesce :: [(Int, Int)] -> [([Int], Int)]
                coalesce [] = []
                coalesce group@((_, target) : _) =
                  [(reverse $ map fst group, target)]
             in sortBy
                  ( comparing (negate . length . fst)
                      <> comparing (cellIdxToName weaveConf.archive 0 . snd)
                  )
                  . concatMap coalesce
                  . groupBy ((==) `on` snd)
                  . sortOn snd
      li_ [] $ do
        a_ [href_ externalOrgDocPath] $ do
          toHtml $ externalOrgDocName <> " (" <> weight <> ")"
        ol_ [] $ do
          forM_ (groupedByTarget edges) f

weaveNamedCellsIndex :: HtmlWeaver ()
weaveNamedCellsIndex = do
  weaveConf <- ask
  div_ [class_ "named-cells-index"] $ do
    p_ [class_ "widget-title"] $ do
      let cells = do
            obj <- lookup weaveConf.originObjectPath weaveConf.archive.objects
            pure obj.orgDoc.cells
          namedCells =
            sort
              $ [(prose, cell) | cell <- (fromMaybe [] cells), Just prose <- [getCellName cell]]
          lenNamedCells = T.show $ length namedCells
      toHtml @Text $ "Named cells (" <> lenNamedCells <> ")"
      ol_ [] $ do
        forM_ namedCells $ \(prose, cell) -> do
          li_ [] $ do
            let linkAnchor = "#" <> getLinkAnchor Nothing cell
            a_ [href_ linkAnchor] $ do
              weaveProse prose
