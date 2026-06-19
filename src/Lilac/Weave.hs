module Lilac.Weave
  ( isGtocAncestor,
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
import Data.Aeson (encode)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IntMap.Strict qualified as IntMap
import Data.List (groupBy, partition)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Tree
  ( Forest,
    Tree (Node),
  )
import Lilac.Compile (gciToName, makeRelativeToObjectPath, parseCitationStyle)
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
      ( ancestry,
        backlinks,
        bibs,
        csls,
        glossary,
        gtoc,
        objects,
        offsets,
        symbols
      ),
    BToken (BTokenLinkTarget, BTokenPadding, BTokenString),
    Block (body, meta, parentOid),
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
    GCI (GCI, unGCI),
    GPath,
    GTOC (unGTOC),
    GlossaryTerm (definition, term),
    Heading (Heading, body, level, meta, section),
    LCI (LCI, unLCI),
    Link (Link, name, target),
    LinkOid (LinkOid, name, oid),
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
    Object (orgDoc),
    OrgDoc
      ( bibPaths,
        cells,
        citations,
        cslPath,
        footnoteRefs,
        glossary,
        lineLabels,
        meta,
        oid
      ),
    PToken
      ( PTokenCitation,
        PTokenDivEnd,
        PTokenDivStart,
        PTokenLink,
        PTokenMathFragment,
        PTokenStyledText
      ),
    ProjectConf
      ( bibliography,
        citationStyle,
        homepage,
        name,
        version
      ),
    Prose (Prose, unProse),
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
        projectConf,
        rootRelativePrefix
      ),
    decodeGPath,
    emptyGPath,
    encodeGPath,
    gciToObjCell,
    gciToObjPath,
    getBlock,
    getNamedCells,
    getProseContents,
    getSectionNumber,
    getTangled,
    getTangledBlocks,
    markingChars,
    markingCharsInvisible,
    ptext,
    stext,
    stripLinks,
    stripStyles,
  )
import Lucid
  ( Attributes,
    HtmlT,
    a_,
    alt_,
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
    data_,
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
    ul_,
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
    Char,
    Either (Right),
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
    foldlM,
    forM_,
    fromMaybe,
    fst,
    id,
    inits,
    isJust,
    isPrefixOf,
    length,
    listToMaybe,
    map,
    mapMaybe,
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
    reverse,
    runReaderT,
    sequence_,
    show,
    snd,
    sort,
    sortBy,
    sortOn,
    swap,
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
import System.OsPath qualified as P
import Text.Blaze.Html.Renderer.Text (renderHtml)

type HtmlWeaver :: Type -> Type
type HtmlWeaver a = HtmlT (ReaderT WeaveConf Identity) a

weaveObject :: Object -> HtmlWeaver ()
weaveObject object = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    title_ --
      . toHtml --
      . fromMaybe "Untitled"
      $ lookup "title" object.orgDoc.meta
    cssPath <- getRootRelativePath "css/default.css" --
    link_ [rel_ "stylesheet", href_ cssPath]

    link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
    link_
      [ rel_ "preconnect",
        href_ "https://fonts.gstatic.com",
        crossorigin_ "anonymous"
      ]
    let fontsLink =
          T.intercalate
            ""
            $ ["https://fonts.googleapis.com/css2?display=swap"]
            <> map (\(name, weights) -> name <> weights) fonts
        fonts :: [(Text, Text)]
        fonts =
          [ ( "&family=Source+Code+Pro:ital",
              ",wght@0,200..900;1,200..900"
            ),
            ( "&family=Source+Sans+3:ital",
              ",wght@0,200..900;1,200..900"
            ),
            ( "&family=Source+Serif+4:ital,opsz",
              ",wght@0,8..60,200..900;1,8..60,200..900"
            )
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
    let lciCellPairs = sortFootnoteCells object
    weaveNavbar
    weaveMain object.orgDoc.meta lciCellPairs
    weaveAllTocs lciCellPairs
    weaveHeadingAncestry lciCellPairs
    jsPath <- getRootRelativePath "js/lilac.js" --
    script_
      [ id_ "lilac-fe-script",
        src_ jsPath
      ]
      ("" :: Text)

sortFootnoteCells :: Object -> [(LCI, Cell)]
sortFootnoteCells object =
  beforeFootnotes
    <> sortedFootnotes
    <> afterFootnotes
  where
    lciCellPairs = zip (map LCI [0 :: Int ..]) object.orgDoc.cells
    beforeFootnotes = takeWhile (not . isFootnote) lciCellPairs
    afterFootnotes =
      dropWhile isFootnote
        $ dropWhile (not . isFootnote) lciCellPairs
    sortedFootnotes =
      sortOn (\(_, cell) -> getFootnoteNumber cell)
        $ takeWhile isFootnote
        $ dropWhile (not . isFootnote) lciCellPairs
    getFootnoteNumber cell --
      | (CFootnote footnote) <- cell,
        Just (n, _) <- lookup footnote.name object.orgDoc.footnoteRefs =
          n
      | otherwise = maxBound :: Int
    isFootnote :: (LCI, Cell) -> Bool
    isFootnote (_, cell) = case cell of
      CFootnote {} -> True
      _ -> False

weaveCell :: (LCI, Cell) -> HtmlWeaver ()
weaveCell (lci, cell) = case cell of
  CHeading heading -> weaveHeading lci heading
  CListItem listMarkers -> sequence_ $ map weaveListMarker listMarkers
  CParagraph prose -> p_ [] $ wrapWith lci $ weaveProse prose
  CBlock block
    | Just "index" <- lookup "name" block.meta,
      Just "true" <- lookup "hidden" block.meta ->
        pure () --
    | otherwise -> weaveBlock lci block
  CMathFragment mathFragment -> weaveMathFragmentStandalone lci mathFragment
  CFigure figure -> weaveFigure lci figure
  CTable table -> weaveTable lci table
  CFootnote footnote -> weaveFootnote lci footnote
  CGlossaryTerm glossaryTerm -> weaveGlossaryTerm lci glossaryTerm
  CCommand cmd -> case cmd of
    PrintBibliography -> weaveBibliography lci
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
      h1_ [id_ "top"] $ toHtml title --
      hr_ [class_ "title-underline"]
  f "author" $ p_ [class_ "doc-meta-author"]
  f "date" $ p_ [class_ "doc-meta-date"]
  where
    f k element = case lookup k m of
      Nothing -> pure ()
      Just x -> element $ toHtml x

weaveHeading :: LCI -> Heading -> HtmlWeaver ()
weaveHeading lci heading = do
  when (heading.section /= [1]) $ toHtmlRaw @Text "</div>" --
  let containerId = "heading-container-" <> linkAnchor
  toHtmlRaw
    $ "<div id=\""
    <> containerId
    <> "\" class=\"heading-container\">" --
  hN [id_ linkAnchor] $ do
    wrapWith lci $ do
      a_ [href_ $ "#" <> linkAnchor] $ do
        toHtml $ getSectionNumber heading --
        toHtmlRaw ("&ensp;" :: Text)
        weaveProse heading.body --
  where
    hN = case heading.level of
      1 -> h2_ --
      2 -> h3_
      3 -> h4_
      4 -> h5_
      _ -> h6_
    linkAnchor = getLinkAnchor Nothing $ CHeading heading

getLinkAnchor :: Maybe LCI -> Cell -> Text
getLinkAnchor maybeLCI = \case
  CHeading heading ->
    T.intercalate
      "-"
      [ "h",
        getSectionNumber heading, --
        toSafeHtmlId $ getProseContents heading.body --
      ]
  CListItem {} -> ""
  CParagraph {} -> case maybeLCI of
    Just lci -> "c-" <> T.show lci.unLCI --
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
  CGlossaryTerm glossaryTerm -> case maybeLCI of
    Just lci ->
      let ref =
            "glossary-"
              <> "c-"
              <> T.show lci.unLCI
              <> "-"
              <> toSafeHtmlId (CP.toText @Prose $ stripLinks glossaryTerm.term)
       in toSafeHtmlId ref
    Nothing -> ""
  CCommand {} -> ""

lciAttrs :: LCI -> [Attributes]
lciAttrs lci = [id_ ("c-" <> T.show lci.unLCI), class_ "cell-idx"]

wrapWith :: LCI -> HtmlWeaver () -> HtmlWeaver ()
wrapWith lci wrapMe = do
  span_ (lciAttrs lci) $ do
    wrapMe

wrapWithDiv :: LCI -> HtmlWeaver () -> HtmlWeaver ()
wrapWithDiv lci wrapMe = do
  div_ (lciAttrs lci) $ do
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
  LinkTargetFilePath path -> not . T.isSuffixOf ".html" $ T.pack path
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

weaveBlock :: LCI -> Block -> HtmlWeaver ()
weaveBlock lci block = do
  wrapWithDiv lci $ do
    div_ [class_ "code-block-container", id_ linkAnchor] $ do
      div_ [class_ "code-block-controls"] $ do
        --
        button_
          [ class_ "toggle-code-block-meta-raw",
            data_ "action" "toggle-code-block-meta-raw"
          ]
          $ pure ()
        a_ [href_ $ "#" <> linkAnchor] $ do
          toHtmlRaw ("&#x1F517;" :: String)
      weaveImportantMetadata block --
      let (rawBorder, bodyBorder)
            | Nothing <- lookup "name" block.meta,
              Nothing <- lookup "tangle" block.meta =
                case lookup "collapsed" block.meta of
                  Just "true" -> (" top-rounded", " anonymous bottom-rounded")
                  _ -> (" top-rounded", " anonymous fully-rounded") --
            | otherwise = ("", "")
      div_ [class_ $ "code-block-meta-raw hidden" <> rawBorder] $ do
        --
        sequence_ . map weaveBlockMeta $ M.toList block.meta
      weaveInlineBlockControls block --
      weaveBlockBody block bodyBorder
  where
    linkAnchor = getLinkAnchor Nothing $ CBlock block

weaveImportantMetadata :: Block -> HtmlWeaver ()
weaveImportantMetadata block
  | Nothing <- lookup "name" block.meta,
    Nothing <- lookup "tangle" block.meta =
      pure ()
  | otherwise = do
      div_ [class_ "code-block-meta"] $ do
        if
          | Just name <- lookup "name" block.meta,
            Just path <- lookup "tangle" block.meta -> do
              code_ $ toHtml name
              br_ []
              code_ $ do
                span_ [class_ "bold"] $ do
                  toHtmlRaw @Text "&#x1F3AF; " --
                  toHtml path
          | Just name <- lookup "name" block.meta -> do
              code_ $ toHtml name
          | Just path <- lookup "tangle" block.meta -> do
              code_ $ do
                span_ [class_ "bold"] $ do
                  toHtmlRaw @Text "&#x1F3AF; "
                  toHtml path
        weaveBlockAncestry block

weaveInlineBlockControls :: Block -> HtmlWeaver ()
weaveInlineBlockControls block
  | Just "true" <- lookup "collapsed" block.meta = do
      let border
            | Nothing <- lookup "name" block.meta,
              Nothing <- lookup "tangle" block.meta =
                " fully-rounded"
            | otherwise = " bottom-rounded"
      div_ [class_ $ "code-block-controls-inline" <> border] $ do
        --
        button_
          [ class_ "toggle-code-block-body",
            data_ "action" "toggle-code-block-body"
          ]
          $ pure ()
  | otherwise = pure ()

weaveBlockBody :: Block -> Text -> HtmlWeaver ()
weaveBlockBody block bodyBorder = do
  let maybeCollapsed
        | Just "true" <-
            lookup "collapsed" block.meta =
            " collapsed collapsible" --
        | otherwise = ""
  pre_ [class_ $ "code-block-body" <> bodyBorder <> maybeCollapsed] $ do
    let bodyText = getBlockContentsWithoutLinks block.body
    case lookup "__src_language" block.meta of --
      Just lang -> do
        weaveConf <- ask
        let colorizedBody = renderAsText weaveConf $ colorizeText lang bodyText
            getLink = \case
              BTokenLinkTarget lt ->
                let styledName = case lt of
                      LinkTargetOid (LinkOid _oid n) ->
                        [ StyledText
                            { body = n,
                              style = styleMaskFrom []
                            }
                        ]
                      _ -> []
                 in Just
                      $ Link
                        { target = lt,
                          name = styledName
                        }
              BTokenString {}
              BTokenPadding {} -> Nothing
            links = catMaybes $ map getLink block.body
        toHtmlRaw =<< spliceLinks colorizedBody links
      Nothing -> code_ $ toHtml bodyText

getBlockContentsWithoutLinks :: [BToken] -> Text
getBlockContentsWithoutLinks = T.concat . map bTokenToText
  where
    bTokenToText :: BToken -> Text
    bTokenToText = \case
      BTokenString text -> text
      BTokenPadding -> "" --
      BTokenLinkTarget (LinkTargetOid _) -> "\NUL"
      BTokenLinkTarget (LinkTargetLineLabel _) -> "\NUL"
      BTokenLinkTarget {} -> ""

weaveBlockAncestry :: Block -> HtmlWeaver ()
weaveBlockAncestry block = case lookup "name" block.meta of
  Nothing -> pure () --
  Just name -> do
    weaveConf <- ask
    let archive = weaveConf.archive
        selfLinkOid = LinkOid block.parentOid name
        parentGCIs = do
          selfGCI <- lookup selfLinkOid archive.symbols
          lookup selfGCI archive.ancestry
        parents = mapMaybe f $ fromMaybe [] parentGCIs
        f :: GCI -> Maybe (GCI, [GCI], GPath, Block, Prose)
        f parentGCI = do
          grandParentGCIs <-
            pure . fromMaybe [] $ lookup parentGCI archive.ancestry
          (_, cell) <- gciToObjCell archive parentGCI
          parentBlock <-
            case cell of
              CBlock b -> Just b
              _ -> Nothing
          (offset, path) <- IntMap.lookupLE parentGCI.unGCI archive.offsets
          prose <- gciToName archive offset parentGCI
          pure (parentGCI, grandParentGCIs, path, parentBlock, prose)
        (roots, nonRoots) =
          partition (\(_, ancestors, _, _, _) -> null ancestors) parents
        getName :: (GCI, [GCI], GPath, Block, Prose) -> Text
        getName (_, _, _, b, _) =
          T.toLower . fromMaybe "" $ lookup "name" b.meta
        roots' = sortOn getName roots
        nonRoots' = sortOn getName nonRoots
    forM_ roots' weaveParentBlock
    forM_ nonRoots' weaveParentBlock

weaveParentBlock ::
  (GCI, [GCI], GPath, Block, Prose) ->
  HtmlWeaver ()
weaveParentBlock (gci, ancestors, objPath, _, _) = do
  weaveConf <- ask
  case getLinkToGCI weaveConf gci of
    Nothing -> pure ()
    Just (_externalOrgDocName, linkAnchor, _) ->
      if objPath == weaveConf.originObjectPath
        then do
          a_ [href_ $ "#" <> linkAnchor, class_ "parent-block-arrow"] $ do
            if null ancestors
              then toHtml @Text " ⇑"
              else toHtml @Text " ↑"
        else do
          a_ [href_ linkAnchor, class_ "parent-block-arrow"] $ do
            if null ancestors
              then toHtml @Text " ⇪"
              else toHtml @Text " ⇧"

getLinkToGCI :: WeaveConf -> GCI -> Maybe (Text, Text, Prose)
getLinkToGCI weaveConf gci
  | Just (offset, gpath) <-
      IntMap.lookupLE gci.unGCI weaveConf.archive.offsets,
    Just (_, cell) <- gciToObjCell weaveConf.archive gci,
    Just cellName <- gciToName weaveConf.archive offset gci =
      let lci = LCI $ gci.unGCI - offset
       in if gpath == weaveConf.originObjectPath
            then do
              let linkAnchor = getLinkAnchor (Just lci) cell
              Just ("", linkAnchor, cellName)
            else do
              (linkAnchor, docName) <-
                getRootRelativeOrgDocCellLink weaveConf gpath lci cell
              Just (docName, linkAnchor, cellName)
  | otherwise = Nothing

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
  | Just syntax <- Sky.lookupSyntax targetLang Sky.defaultSyntaxMap,
    Just lines <-
      foldMap' Just
        $ Sky.tokenize defaultTokeniserConfig syntax txt =
      toHtmlRaw
        . Text.Blaze.Html.Renderer.Text.renderHtml
        $ Sky.formatHtmlInline Sky.defaultFormatOpts lines
  | otherwise = code_ $ toHtmlRaw txt
  where
    targetLang
      | lang `elem` ["emacs-lisp", "elisp"] = "lisp" --
      | lang `elem` ["gitconfig"] = "ini" --
      | otherwise = lang
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
  wovenLinkTarget <- weaveLinkTarget insideBlock link.target
  linkDisplayName <- weaveLinkDisplayName link
  let attrs =
        [href_ wovenLinkTarget]
          <> linkCategory wovenLinkTarget
          <> lineLabelAttr
      lineLabelAttr =
        case link.target of
          LinkTargetLineLabel lineLabel ->
            if insideBlock
              then [id_ ("ll-" <> toSafeHtmlId lineLabel.name)] --
              else []
          _ -> []
  a_
    attrs
    $ sequence_
    $ map weaveStyledText linkDisplayName
  where
    linkCategory target
      | isExternalLink link.target =
          [ class_ "external-link"
          ]
      | "#ref-" `T.isPrefixOf` target = [] --
      | otherwise = [class_ "internal-link"]

weaveLinkTarget :: Bool -> LinkTarget -> HtmlWeaver T.Text
weaveLinkTarget insideBlock linkTarget = do
  weaveConf <- ask
  case linkTarget of
    LinkTargetOid linkOid -> weaveLinkTargetLinkOid linkOid
    LinkTargetLineLabel linkOid
      | insideBlock -> pure internalLineLabelTarget --
      | Just originObj <-
          lookup weaveConf.originObjectPath weaveConf.archive.objects,
        originObj.orgDoc.oid == linkOid.oid ->
          pure internalLineLabelTarget --
      | Just gci <- lookup linkOid weaveConf.archive.symbols,
        Just objPath <- gciToObjPath weaveConf.archive gci ->
          pure
            $ getExternalRelPath linkOid objPath
            <> internalLineLabelTarget --
      | otherwise -> pure $ showBrokenLink linkOid
      where
        internalLineLabelTarget =
          (T.pack "#ll-") --
            <> toSafeHtmlId linkOid.name
    LinkTargetFootnote name -> pure $ (T.pack "#footnote-") <> name
    LinkTargetFilePath path ->
      pure . T.pack $ N.escapeURIString N.isUnescapedInURI path
    LinkTargetURI uri -> pure . T.pack $ N.uriToString id uri ""

weaveLinkTargetLinkOid :: LinkOid -> HtmlWeaver T.Text
weaveLinkTargetLinkOid linkOid = do
  weaveConf <- ask
  if
    | Just gci <- lookup linkOid weaveConf.archive.symbols,
      gci.unGCI < 0,
      Just objPath <-
        (sort $ keys weaveConf.archive.objects) !!? (abs gci.unGCI - 1) ->
        pure
          $ if objPath == weaveConf.originObjectPath
            then "#top"
            else getExternalRelPath linkOid objPath
    | Just gci <- lookup linkOid weaveConf.archive.symbols,
      Just (docName, linkAnchor, _) <- getLinkToGCI weaveConf gci ->
        if docName == "" --
          then pure $ "#" <> linkAnchor
          else pure linkAnchor
    | otherwise -> pure $ showBrokenLink linkOid

weaveLinkDisplayName :: Link -> HtmlWeaver [StyledText]
weaveLinkDisplayName link = do
  weaveConf <- ask
  let displayName =
        case link.target of
          LinkTargetLineLabel lineLabel
            | link.name /= [] -> link.name
            | Just gci <- lookup lineLabel weaveConf.archive.symbols,
              Just (obj, _) <- gciToObjCell weaveConf.archive gci,
              Just num <-
                lookup
                  (T.drop 1 $ T.dropEnd 1 lineLabel.name)
                  obj.orgDoc.lineLabels ->
                if
                  | Just originObj <-
                      lookup
                        weaveConf.originObjectPath
                        weaveConf.archive.objects,
                    originObj /= obj,
                    Just (offset, _) <-
                      IntMap.lookupLE
                        gci.unGCI
                        weaveConf.archive.offsets,
                    Just prose <- gciToName weaveConf.archive offset gci ->
                      (extractStyledText prose) --
                        <> [stext " → " [], stext (T.show num) [Bold]]
                  | otherwise ->
                      [stext (T.show num) [Bold]] --
            | otherwise ->
                [stext "weaveLink error (line label display name)" []]
          _ -> link.name --
      extractStyledText :: Prose -> [StyledText]
      extractStyledText prose = reverse $ foldl' f [] prose.unProse
        where
          f acc (PTokenStyledText styledText) = styledText : acc
          f acc _ = acc
  pure displayName

getExternalRelPath :: LinkOid -> GPath -> Text
getExternalRelPath linkOid extPath
  | Just toObjPath <- P.encodeUtf $ decodeGPath extPath,
    Just serveRoot <- P.encodeUtf "/",
    Right str <-
      P.decodeWith utf8 utf8
        . P.combine serveRoot
        $ P.replaceExtension
          toObjPath
          htmlExt =
      T.pack str
  | otherwise = showBrokenLink linkOid
  where
    htmlExt = P.unsafeEncodeUtf "html"

showBrokenLink :: LinkOid -> Text
showBrokenLink linkOid = "#LILAC_BROKEN_LINK-" <> show linkOid

weaveFigure :: LCI -> Figure -> HtmlWeaver ()
weaveFigure lci figure = do
  wovenLinkTarget <- weaveLinkTarget False figure.link.target
  let captionPrefix =
        "Figure "
          <> lookupDefault "figure-number-error" "__figure_number" figure.meta
  figure_ [id_ linkAnchor] $ do
    figcaption_ [] $ do
      wrapWith lci $ do
        a_
          [ href_ $ "#" <> linkAnchor,
            class_ "self-link",
            class_ "caption-prefix"
          ]
          $ toHtml captionPrefix
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

weaveMathFragmentStandalone :: LCI -> MathFragment -> HtmlWeaver ()
weaveMathFragmentStandalone lci mathFragment = do
  let (begMarker, endMarker)
        | Just _ <- lookup "__equation_number" mathFragment.meta =
            ("\\begin{equation}", "\\end{equation}")
        | otherwise = ("\\[", "\\]")
  p_ [id_ linkAnchor] $ do
    wrapWith lci $ do
      a_ [href_ $ "#" <> linkAnchor, class_ "self-link"] $ do
        begMarker <> toHtml mathFragment.body <> endMarker
  where
    linkAnchor = getLinkAnchor Nothing $ CMathFragment mathFragment

weaveMathFragmentInline :: MathFragment -> HtmlWeaver ()
weaveMathFragmentInline mathFragment =
  "\\(" <> toHtml mathFragment.body <> "\\)"

weaveTable :: LCI -> Table -> HtmlWeaver ()
weaveTable lci table = do
  --
  figure_ [id_ linkAnchor] $ do
    weaveTableCaption lci table linkAnchor
    --
    div_ [class_ "table-wrapper"] $ do
      table_ [] $ do
        weaveTableColumnGroups table
        weaveTableRows table
  where
    linkAnchor = getLinkAnchor Nothing $ CTable table

weaveTableCaption :: LCI -> Table -> Text -> HtmlWeaver ()
weaveTableCaption lci table linkAnchor = do
  figcaption_ [] $ do
    wrapWith lci $ do
      a_
        [ href_ $ "#" <> linkAnchor,
          class_ "self-link",
          class_ "caption-prefix"
        ]
        $ do
          let captionPrefix =
                "Table "
                  <> lookupDefault
                    "table-number-error"
                    "__table_number"
                    table.meta
          toHtml captionPrefix
      toHtml $ case table.caption.unProse of
        [] -> "." :: Text
        _ -> ". " :: Text
      weaveProse table.caption

weaveTableColumnGroups :: Table -> HtmlWeaver ()
weaveTableColumnGroups table = do
  forM_ (zip [1 :: Int ..] getColgroups) $ \(groupIdx, numCols) -> do
    colgroup_ [] $ do
      --
      forM_ [1 .. numCols] $ \colNum -> do
        let attrs
              | colNum == 1 && groupIdx > 1 =
                  [class_ "table-vertical-thick-line-colgroup"]
              | colNum > 1 =
                  [class_ "table-vertical-thin-line-col"]
              | otherwise = []
        col_ attrs
  where
    getColgroups :: [Int] --
    getColgroups =
      maybe
        [getMaxCol table]
        ( reverse --
            . foldl' getColgroupSizes [0]
            . getTableCellsFromRow table
        )
        getColgroupsRow
    getColgroupsRow =
      foldl' findSlash Nothing $ getFirstColumnCells table --
      where
        findSlash :: Maybe Int -> ((Int, Int), Prose) -> Maybe Int
        findSlash acc ((row, _col), prose)
          | isJust acc = acc
          | proseHasNonExportMarker prose = Just row
          | otherwise = acc
        proseHasNonExportMarker :: Prose -> Bool
        proseHasNonExportMarker = proseHas "/"
    getColgroupSizes :: [Int] -> Prose -> [Int] --
    getColgroupSizes ns prose = case ns of
      [] -> []
      [0] -> [1] --
      acc@(latest : rest)
        | proseHasColgroupStartMarker prose -> 1 : acc
        | otherwise -> latest + 1 : rest
      where
        proseHasColgroupStartMarker :: Prose -> Bool
        proseHasColgroupStartMarker = proseHas "<" --

getTableCellsFromRow :: Table -> Int -> [Prose]
getTableCellsFromRow table n =
  hideMarkingCharInFirstColumn
    . M.toList
    $ M.filterWithKey (\(row, _col) _ -> row == n) table.grid
  where
    hideMarkingCharInFirstColumn :: [((Int, Int), Prose)] -> [Prose]
    hideMarkingCharInFirstColumn = \case
      [] -> []
      (((_row, _col), prose) : cells)
        | prose == tableHorizontalLine -> [Prose []]
        | markingCharExistsInFirstColumn -> map snd cells
        | otherwise -> prose : map snd cells
    markingCharExistsInFirstColumn =
      any (not . tableCellLacks markingChars) $ getFirstColumnCells table --

getFirstColumnCells :: Table -> [((Int, Int), Prose)]
getFirstColumnCells table =
  M.toList
    $ M.filterWithKey (\(_row, col) _ -> col == 1) table.grid

tableCellLacks :: [Char] -> ((Int, Int), Prose) -> Bool
tableCellLacks chars (_, prose) = case prose.unProse of
  [] -> True
  [PTokenStyledText styledText] ->
    notElem styledText.body $ map T.singleton chars
  _ -> True

getMaxRow :: Table -> Int
getMaxRow table = foldl' max 0 . map fst $ keys table.grid

getMaxCol :: Table -> Int
getMaxCol table = foldl' max 0 . map snd $ keys table.grid

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

weaveTableRows :: Table -> HtmlWeaver ()
weaveTableRows table = do
  let columnNames = getTableCellsFromRow table $ getHeaderRowIndex table
  weaveTableHead columnNames
  weaveTableBody table columnNames

weaveTableHead :: [Prose] -> HtmlWeaver ()
weaveTableHead columnNames = do
  thead_ [] $ do
    tr_ [] $ do
      forM_ columnNames $ \prose -> do
        th_ [] $ weaveProse prose

weaveTableBody :: Table -> [Prose] -> HtmlWeaver ()
weaveTableBody table columnNames = do
  tbody_ [] $ do
    forM_ (getRows visibleRows) $ \row ->
      if row == [Prose []] --
        then do
          tr_ [class_ "table-horizontal-line"] $ do
            forM_ columnNames $ \_ -> do
              td_ [] $ toHtml @Text ""
        else do
          tr_ [class_ "table-body-row"] $ do
            forM_ row $ \prose -> do
              let tdAttrs =
                    if mostlyNumeric prose --
                      then [class_ "table-cell-numeric"]
                      else []
              td_ tdAttrs $ weaveProse prose
  where
    getRows rowNums = map (getTableCellsFromRow table) rowNums
    isVisible :: ((Int, Int), Prose) -> Bool --
    isVisible = tableCellLacks markingCharsInvisible
    visibleRows =
      filter
        (`notElem` invisibleRows)
        [(getHeaderRowIndex table + 1) .. getMaxRow table]
    invisibleRows = foldl' f [] $ getFirstColumnCells table
      where
        f acc tableCell@((row, _col), _)
          | isVisible tableCell = acc
          | otherwise = row : acc
    mostlyNumeric :: Prose -> Bool
    mostlyNumeric prose =
      let rawText = T.concat $ map extractText prose.unProse
          extractText = \case
            PTokenStyledText styledText -> styledText.body
            _ -> ""
       in isNumericText rawText

getHeaderRowIndex :: Table -> Int
getHeaderRowIndex table =
  fromMaybe 1 . foldl' f Nothing $ getFirstColumnCells table
  where
    f acc@(Just _) _ = acc
    f acc tableCell@((row, _col), _)
      | tableCellLacks markingChars tableCell = Just row
      | otherwise = acc

getRootRelativePath :: Text -> HtmlWeaver Text
getRootRelativePath path = do
  weaveConf <- ask
  let rootRelRaw =
        T.dropAround (== '/')
          . T.pack
          . decodeGPath
          $ weaveConf.rootRelativePrefix
      rootRelPrefix
        | rootRelRaw == "" = "/"
        | otherwise = "/" <> rootRelRaw <> "/"
  pure $ rootRelPrefix <> path

weaveFootnote :: LCI -> Footnote -> HtmlWeaver ()
weaveFootnote lci footnote = do
  weaveConf <- ask
  if
    | Just originObj <-
        lookup weaveConf.originObjectPath weaveConf.archive.objects,
      Just (footnoteNumber, referrerLCI) <-
        lookup footnote.name originObj.orgDoc.footnoteRefs -> do
        p_
          [ class_ "footnote-definition",
            id_ ("footnote-" <> footnote.name)
          ]
          $ do
            wrapWith lci $ do
              span_
                [class_ "footnote-number"]
                . toHtml
                $ T.show footnoteNumber
                <> ". "
              weaveProse footnote.body
              weaveFootnoteBacklink referrerLCI --
    | otherwise -> pure ()

weaveFootnoteBacklink :: LCI -> HtmlWeaver ()
weaveFootnoteBacklink lci = do
  weaveConf <- ask
  if
    | Just offset <-
        lookup weaveConf.originObjectPath
          . M.fromList
          . map swap
          $ IntMap.toList weaveConf.archive.offsets,
      Just (_, linkAnchor, name) <-
        getLinkToGCI weaveConf (GCI $ lci.unLCI + offset) -> do
        toHtml @Text " "
        a_ [href_ $ "#" <> linkAnchor, class_ "footnote-backlink"] $ do
          toHtml @Text "↑ "
          weaveProse name
    | otherwise -> pure ()

weaveGlossaryTerm :: LCI -> GlossaryTerm -> HtmlWeaver ()
weaveGlossaryTerm lci glossaryTerm = do
  weaveConf <- ask
  let dlStart :: HtmlWeaver ()
      dlStart = toHtmlRaw @Text "<dl>"
      dlEnd :: HtmlWeaver ()
      dlEnd = toHtmlRaw @Text "</dl>"
      isGlossaryTerm lci' --
        | Just originObj <-
            lookup weaveConf.originObjectPath weaveConf.archive.objects,
          Just internalCell <- originObj.orgDoc.cells !!? lci'.unLCI,
          CGlossaryTerm _ <- internalCell =
            True
        | otherwise = False
      isFirstGlossaryTerm = not . isGlossaryTerm . LCI $ lci.unLCI - 1
      isLastGlossaryTerm = not . isGlossaryTerm . LCI $ lci.unLCI + 1
  when (isFirstGlossaryTerm) dlStart --
  wrapWithDiv lci $ do
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
    linkAnchor = getLinkAnchor (Just lci) $ CGlossaryTerm glossaryTerm

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
        --
        let encoded = CslLink ("LILAC_LINK\NUL" <> unparseLink link) CslEmpty
         in acc <> encoded
      PTokenMathFragment mathFragment ->
        let encoded =
              --
              CslDiv
                ("LILAC_MATH_FRAGMENT\NUL" <> unparseMathFragment mathFragment)
                CslEmpty
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
      --
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
      CP.citationItems =
        concatMap (citeToCiteprocCitationItems sv) citation.cites,
      CP.citationResetPosition = False
    }
  where
    (sv, _) = citation.styleVariation --

citeToCiteprocCitationItems ::
  Maybe Text ->
  Cite ->
  [CP.CitationItem (CslJson Text)]
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
      CslText t ->
        [ PTokenStyledText
            StyledText {body = t, style = styleMaskFrom s}
        ]
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
      CslNoCase a -> f s a --
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

prepareCitations ::
  ProjectConf ->
  GPath ->
  Object ->
  Archive ->
  IO (CP.Result Prose)
prepareCitations projectConf objGPath object archive = do
  bibPathsRelative <- makeRelativeToObjectPath objGPath object.orgDoc.bibPaths
  cslPathRelative <-
    listToMaybe
      <$> ( makeRelativeToObjectPath objGPath
              $ maybeToList object.orgDoc.cslPath
          )
  let finalCslPath
        | Just _ <- cslPathRelative = cslPathRelative
        | Just _ <- projectConf.citationStyle = projectConf.citationStyle
        | otherwise = Nothing
      bibRefsFromOrgDoc =
        filter neededByCitations
          . concat
          . catMaybes
          . map (\bp -> lookup bp archive.bibs)
          $ projectConf.bibliography
          <> bibPathsRelative
      CP.Result citations bibliography warnings =
        f bibRefsFromOrgDoc finalCslPath
      resultWithProse =
        CP.Result
          { CP.resultCitations = map cslJsonTextToProse citations,
            CP.resultBibliography =
              map
                (\(k, cslJsonText) -> (k, cslJsonTextToProse cslJsonText))
                bibliography,
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
    f bibRefs finalCslPath
      | Just cslPath' <- finalCslPath,
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

weaveBibliography :: LCI -> HtmlWeaver ()
weaveBibliography lci = do
  weaveConf <- ask
  let result = weaveConf.citeprocResult
      bibliography = result.resultBibliography
  div_ [class_ "bibliography"] $ do
    wrapWithDiv lci $ do
      forM_ bibliography $ \(key, prose) -> do
        let linkAnchor = "ref-" <> key
        div_ [class_ "bib-ref", id_ linkAnchor] $ do
          weaveProse prose

weaveGlossary :: Bool -> HtmlWeaver ()
weaveGlossary isGlobal = do
  weaveConf <- ask
  if
    | Just originObj <-
        lookup weaveConf.originObjectPath weaveConf.archive.objects -> do
        let terms :: [(LCI, GPath)]
            terms
              | isGlobal =
                  map (\((_term, path), lci) -> (lci, path))
                    $ sortBy
                      ( comparing (toNormalizedText . CP.fromText . fst . fst)
                          <> comparing (snd . fst)
                      )
                    $ M.toList weaveConf.archive.glossary
              | otherwise =
                  map (\(_term, lci) -> (lci, weaveConf.originObjectPath))
                    $ sortBy
                      (comparing (toNormalizedText . CP.fromText . fst)) --
                    $ M.toList originObj.orgDoc.glossary
            terms' = zip (True : repeat False) terms
        when (terms' /= []) $ toHtmlRaw @Text "<dl>"
        forM_ terms' weaveCollectedGlossaryTerm
        when (terms' /= []) $ toHtmlRaw @Text "</dl>"
    | otherwise -> pure ()

weaveCollectedGlossaryTerm :: (Bool, (LCI, GPath)) -> HtmlWeaver ()
weaveCollectedGlossaryTerm (isFirstTerm, (lci, objPath)) = do
  weaveConf <- ask
  if
    | True <- objPath == weaveConf.originObjectPath, --
      Just originObj <-
        lookup weaveConf.originObjectPath weaveConf.archive.objects,
      Just internalCell <- originObj.orgDoc.cells !!? lci.unLCI,
      CGlossaryTerm glossaryTerm <- internalCell -> do
        let linkAnchor = getLinkAnchor (Just lci) internalCell
        dt_ maybeInnerVerticalMargin $ do
          weaveProse glossaryTerm.term
        dd_ $ do
          a_ [href_ $ "#" <> linkAnchor] $ do
            toHtml @Text "↑"
          weaveProse glossaryTerm.definition
    | True <- objPath /= weaveConf.originObjectPath, --
      Just extObj <- lookup objPath weaveConf.archive.objects,
      Just externalCell <- extObj.orgDoc.cells !!? lci.unLCI,
      CGlossaryTerm glossaryTerm <- externalCell,
      Just (externalLinkAnchor, externalOrgDocName) <-
        getRootRelativeOrgDocCellLink
          weaveConf
          objPath
          lci
          externalCell -> do
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

getRootRelativeOrgDocCellLink ::
  WeaveConf ->
  GPath ->
  LCI ->
  Cell ->
  Maybe (Text, Text)
getRootRelativeOrgDocCellLink weaveConf objPath lci externalCell
  | Just (externalOrgDocName, externalOrgDocPath) <-
      getRootRelativeOrgDocLink weaveConf objPath =
      let externalLinkAnchor =
            externalOrgDocPath
              <> "#"
              <> (getLinkAnchor (Just lci) externalCell)
       in Just (externalLinkAnchor, externalOrgDocName)
  | otherwise = Nothing

getRootRelativeOrgDocLink :: WeaveConf -> GPath -> Maybe (Text, Text)
getRootRelativeOrgDocLink weaveConf objPath
  | Just toObjPath <- P.encodeUtf $ decodeGPath objPath,
    Just serveRoot <- P.encodeUtf "/",
    Just rootRelativePrefixOsPath <-
      P.encodeUtf $ decodeGPath weaveConf.rootRelativePrefix,
    Right externalOrgDocName <-
      P.decodeWith utf8 utf8
        $ P.replaceExtension
          toObjPath
          (P.unsafeEncodeUtf "org"),
    Right externalOrgDocPath <-
      P.decodeWith utf8 utf8
        . P.combine serveRoot
        . P.combine rootRelativePrefixOsPath
        $ P.replaceExtension
          toObjPath
          (P.unsafeEncodeUtf "html") =
      Just (T.pack externalOrgDocName, T.pack externalOrgDocPath)
  | otherwise = Nothing

toNormalizedText :: Prose -> Text
toNormalizedText prose =
  T.toLower
    . CP.toText @Prose
    . stripStyles
    $ stripLinks prose

weaveAllTocs :: [(LCI, Cell)] -> HtmlWeaver ()
weaveAllTocs lciCellPairs = do
  nav_ [class_ "tocs"] $ do
    weaveConf <- ask
    weaveGtoc weaveConf.archive.gtoc
    weaveToc $ map snd lciCellPairs

weaveToc :: [Cell] -> HtmlWeaver ()
weaveToc cells = do
  --
  let pageMetricsCells =
        let haveTangled = not . null $ getTangledBlocks cells
            haveNamedCells = not . null $ getNamedCells cells
            defaultMetricsEntries =
              [ CHeading
                  Heading
                    { level = 1,
                      section = [],
                      body = Prose [ptext "Page metrics" []],
                      meta = mempty
                    },
                CHeading
                  Heading
                    { level = 2,
                      section = [],
                      body = Prose [ptext "Backlinks" []],
                      meta = mempty
                    }
              ]
            tangledFilesEntry =
              if haveTangled
                then
                  [ CHeading
                      Heading
                        { level = 2,
                          section = [],
                          body = Prose [ptext "Tangled files" []],
                          meta = mempty
                        }
                  ]
                else []
            namedCellsEntry =
              if haveNamedCells
                then
                  [ CHeading
                      Heading
                        { level = 2,
                          section = [],
                          body = Prose [ptext "Named cells" []],
                          meta = mempty
                        }
                  ]
                else []
         in defaultMetricsEntries <> tangledFilesEntry <> namedCellsEntry
      cellsWithPageMetrics = cells <> pageMetricsCells
  p_ [class_ "widget-title toc-title"] $ do
    toHtml @Text "Page contents"
  nav_ [class_ "toc"] $ do
    div_ [class_ "toc-entries"] $ do
      closeHeadingLevels =<< foldlM f 0 cellsWithPageMetrics --
  where
    f :: Int -> Cell -> HtmlWeaver Int
    f prevLevel = \case
      CHeading heading -> do
        if
          | prevLevel < heading.level -> weaveListMarker LOrderedStart
          | prevLevel > heading.level -> do
              closeHeadingLevels (prevLevel - heading.level) --
              weaveListMarker LItemEnd
          | otherwise -> weaveListMarker LItemEnd
        weaveTocHeading heading
        pure heading.level
      _ -> pure prevLevel
    closeHeadingLevels :: Int -> HtmlWeaver ()
    closeHeadingLevels levels =
      forM_ [1 .. levels] $ \_ -> do
        weaveListMarker LItemEnd
        weaveListMarker LOrderedEnd

weaveTocHeading :: Heading -> HtmlWeaver ()
weaveTocHeading heading = do
  toHtmlRaw
    ( "<li class=\"toc-level-"
        <> show heading.level
        <> "\">" ::
        Text
    )
  div_ [class_ "toc-entry"] $ do
    if null heading.section
      then pure ()
      else do
        span_ [class_ "toc-counter"] $ do
          toHtml sectionText
    a_ [href_ $ "#" <> linkAnchor] $ do
      weaveProse heading.body
  where
    sectionText = getSectionNumber heading
    linkAnchor = getLinkAnchor Nothing $ CHeading heading

weaveGtoc :: GTOC -> HtmlWeaver ()
weaveGtoc gtoc = do
  p_ [class_ "widget-title gtoc-title"] $ do
    toHtml @Text "Site contents" --
  nav_ [class_ "gtoc"] $ do
    let (Node rootNode forest) = gtoc.unGTOC
    ul_ [] $ do
      weaveGtocForest (Node rootNode [] : forest) --

weaveGtocTree :: Tree GPath -> HtmlWeaver ()
weaveGtocTree (Node path children) = do
  weaveConf <- ask
  li_ [] $ do
    let (docName, docPath) =
          case getRootRelativeOrgDocLink weaveConf path of
            Nothing ->
              ( "LILAC_ERROR_DOC_NAME_NOT_FOUND",
                "LILAC_ERROR_DOC_LINK_NOT_FOUND"
              )
            Just res -> res
        docLabel
          | Just obj <- lookup path weaveConf.archive.objects,
            Just title <- lookup "title" obj.orgDoc.meta =
              title
          | otherwise = docName
        boldAttr =
          if path == weaveConf.originObjectPath
            then [class_ "bold"]
            else []
    a_ ([href_ docPath] <> boldAttr) $ do
      toHtml @Text docLabel
    let isAncestor = path `isGtocAncestor` weaveConf.originObjectPath
    when (isAncestor && (not $ null children)) $ do
      ul_ [] $ weaveGtocForest children --

weaveGtocForest :: Forest GPath -> HtmlWeaver ()
weaveGtocForest nodes = forM_ nodes weaveGtocTree

isGtocAncestor :: GPath -> GPath -> Bool
isGtocAncestor path originObjectPath =
  let pathDirOsp =
        reverse
          . drop 1
          . reverse
          . P.splitPath
          . P.unsafeEncodeUtf
          $ decodeGPath path
      originOsp =
        P.splitPath
          . P.unsafeEncodeUtf
          $ decodeGPath originObjectPath
   in pathDirOsp `isPrefixOf` originOsp

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
            | heading.level < a.level --
              =
                heading
                  : (dropWhile (\a' -> heading.level <= a'.level) ancestry)
            | heading.level > a.level = heading : ancestry --
            | otherwise = heading : as --
          key = getLinkAnchor Nothing cell
          val =
            map
              (getLinkAnchor Nothing . CHeading)
              $ ancestorsOf heading ancestry
       in (updatedAncestry, insert key val m)
    f acc _ = acc
    ancestorsOf :: Heading -> [Heading] -> [Heading]
    ancestorsOf leafHeading ancestry --
      | null ancestry = []
      | otherwise = dropWhile (\a' -> leafHeading.level <= a'.level) ancestry

weaveHeadingAncestry :: [(LCI, Cell)] -> HtmlWeaver ()
weaveHeadingAncestry lciCellPairs = do
  let headingAncestry = genHeadingAncestry $ map snd lciCellPairs
      --
      pageMetricsAncestry :: Map Text [Text]
      pageMetricsAncestry =
        M.fromList
          [ ("h--Page-metrics", []),
            ("h--Tangled-files", ["h--Page-metrics"]),
            ("h--Backlinks", ["h--Page-metrics"]),
            ("h--Named-cells", ["h--Page-metrics"])
          ]
  script_ [id_ "heading-ancestry", type_ "application/json"] $ do
    toHtmlRaw . TLE.decodeUtf8 . encode $ headingAncestry <> pageMetricsAncestry

weaveNavbar :: HtmlWeaver ()
weaveNavbar = do
  div_ [class_ "navbar"] $ do
    div_ [class_ "breadcrumbs"] $ do
      weaveBreadcrumbs

weaveBreadcrumbs :: HtmlWeaver ()
weaveBreadcrumbs = do
  weaveConf <- ask
  weaveProjectName weaveConf.projectConf --
  weaveProjectVersion weaveConf.projectConf --
  let breadcrumbsMiddle =
        (\parts -> take (length parts - 2) (drop 1 parts)) --
          . inits --
          . P.splitDirectories
          . P.unsafeEncodeUtf
          $ decodeGPath weaveConf.originObjectPath
      breadcrumbLast =
        osPathDecoder
          . flip P.replaceExtensions (P.unsafeEncodeUtf "org")
          . P.takeFileName
          . P.unsafeEncodeUtf
          $ decodeGPath weaveConf.originObjectPath
  --
  forM_ breadcrumbsMiddle $ \dirs -> do
    let dirName =
          osPathDecoder
            . fromMaybe (P.unsafeEncodeUtf "LILAC_ERROR_EMPTY_OSPATH_DIR")
            . listToMaybe
            . take 1
            $ reverse dirs
        indexPath =
          fromMaybe emptyGPath
            . encodeGPath
            . T.unpack
            . osPathDecoder
            $ P.combine (P.joinPath dirs) (P.unsafeEncodeUtf "index.lobj") --
    weaveBreadcrumbSeparator
    if
      --
      | Just _ <- lookup indexPath weaveConf.archive.objects -> do
          let (_, docPath) =
                fromMaybe ("", "LILAC_ERROR_RELATIVE_LINK_ERROR")
                  $ getRootRelativeOrgDocLink weaveConf indexPath
          a_ [href_ docPath] $ do
            weaveBreadcrumb dirName
      | otherwise -> weaveBreadcrumb dirName
  --
  weaveBreadcrumbSeparator
  a_ [href_ "#top"] $ do
    weaveBreadcrumb breadcrumbLast
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

weaveFooter :: HtmlWeaver ()
weaveFooter = do
  footer_ $ do
    div_ [class_ "lilac-stamp"] $ do
      toHtml @Text "Generated by "
      a_
        [ href_ "https://git.sr.ht/~listx/lilac",
          class_ "external-link",
          target_ "_blank",
          rel_ "noopener noreferrer"
        ]
        $ do
          toHtml @Text "Lilac"

weaveMain :: Map Text Text -> [(LCI, Cell)] -> HtmlWeaver ()
weaveMain docMeta lciCellPairs = do
  main_ [] $ do
    weaveDocMeta docMeta
    forM_ lciCellPairs weaveCell
    let isHeading = \case
          CHeading {} -> True
          _ -> False
    when (any isHeading $ map snd lciCellPairs)
      $ toHtmlRaw @Text "</div>" --
    weavePageMetrics lciCellPairs

weavePageMetrics :: [(LCI, Cell)] -> HtmlWeaver ()
weavePageMetrics lciCellPairs = do
  let blocks = catMaybes $ map (getBlock . snd) lciCellPairs
      tangledBlocks = catMaybes $ map getTangled blocks
  div_
    [ id_ "heading-container-h--Page-metrics",
      class_ "heading-container page-metrics"
    ]
    $ do
      h2_ [id_ "h--Page-metrics"] $ do
        a_ [href_ $ "#h--Page-metrics"] $ do
          toHtml ("Page metrics" :: Text)
  weaveBacklinks
  weaveTangledBlocksList tangledBlocks
  weaveNamedCellsIndex $ map snd lciCellPairs
  weaveFooter

weaveTangledBlocksList :: [(Text, Block)] -> HtmlWeaver ()
weaveTangledBlocksList tangledBlocks = do
  when (not $ null tangledBlocks) $ do
    div_
      [ id_ "heading-container-h--Tangled-files",
        class_ "heading-container tangled-blocks"
      ]
      $ do
        h3_ [id_ "h--Tangled-files"] $ do
          a_ [href_ $ "#h--Tangled-files"] $ do
            toHtml @Text "Tangled files ("
            toHtml . T.show $ length tangledBlocks
            toHtml @Text ")"
        ol_ [] $ do
          forM_ (sort tangledBlocks) $ \(path, block) -> do
            li_ [] $ do
              let linkAnchor = getLinkAnchor Nothing $ CBlock block
              a_ [href_ $ "#" <> linkAnchor, class_ "verbatim"] $ do
                toHtml path

weaveBacklinks :: HtmlWeaver ()
weaveBacklinks = do
  weaveConf <- ask
  --
  let backlinksByPath =
        lookup weaveConf.originObjectPath weaveConf.archive.backlinks
      numBacklinks =
        foldl'
          (\acc list -> acc + length list)
          0
          $ fromMaybe mempty backlinksByPath
  div_
    [ id_ "heading-container-h--Backlinks",
      class_ "heading-container backlinks"
    ]
    $ do
      h3_ [id_ "h--Backlinks"] $ do
        a_ [href_ $ "#h--Backlinks"] $ do
          toHtml @Text "Backlinks ("
          toHtml $ T.show numBacklinks
          toHtml @Text ")"
      when (numBacklinks > 0) $ do
        ol_ [] $ do
          let sorted =
                sortOn (\(_, links) -> -(length links)) --
                  . toPairs
                  $ fromMaybe mempty backlinksByPath
          sequence_ $ map weaveBacklinksForExternalObj sorted

weaveBacklinksForExternalObj :: (GPath, [(GCI, GCI)]) -> HtmlWeaver ()
weaveBacklinksForExternalObj (_, []) = toHtmlRaw @Text ""
weaveBacklinksForExternalObj (externalObjGPath, edges) = do
  weaveConf <- ask
  --
  case getRootRelativeOrgDocLink weaveConf externalObjGPath of
    Nothing -> pure ()
    Just (externalOrgDocName, externalOrgDocPath) -> do
      li_ [] $ do
        a_ [href_ externalOrgDocPath] $ do
          let weight = T.show $ length edges
          toHtml $ externalOrgDocName <> " (" <> weight <> ")"
        ol_ [] $ do
          let archive = weaveConf.archive
          forM_ (groupedByTarget archive edges) (weaveBacklink archive)
  where
    --
    groupedByTarget :: Archive -> [(GCI, GCI)] -> [([GCI], GCI)]
    groupedByTarget archive =
      let coalesce :: [(GCI, GCI)] -> [([GCI], GCI)]
          coalesce [] = []
          coalesce group@((_, target) : _) =
            [(reverse $ map fst group, target)]
       in sortBy --
            ( comparing (negate . length . fst)
                <> comparing (gciToName archive 0 . snd)
            )
            . concatMap coalesce
            . groupBy ((==) `on` snd)
            . sortOn snd

weaveBacklink :: Archive -> ([GCI], GCI) -> HtmlWeaver ()
weaveBacklink _ ([], _) = pure ()
weaveBacklink archive (externalGCIs@(externalGCI : _), internalGCI) = do
  let (internalLinkAnchor, internalName) =
        ( getInternalLinkAnchor internalGCI,
          getInternalName internalGCI
        )
  li_ [] $ do
    if (length externalGCIs > 1)
      then do
        a_ [href_ internalLinkAnchor] $ do
          weaveProse internalName
          toHtml @Text $ " (" <> (T.show $ length externalGCIs) <> ")"
        ol_ [] $ do
          sequence_ $ map (linkToExternalCell True) externalGCIs
      else do
        a_ [href_ internalLinkAnchor] $ do
          weaveProse internalName
        span_ [class_ "verbatim"] $ do
          toHtmlRaw @Text " &#x2190; "
        linkToExternalCell False externalGCI
  where
    getInternalLinkAnchor gci
      | Just (_, internalCell) <- gciToObjCell archive gci =
          "#" <> (getLinkAnchor Nothing internalCell) --
      | gci.unGCI < 0 = "#top"
      | otherwise = "#LINK_ERROR"
    getInternalName gci
      | Just name <- gciToName archive 0 gci =
          name
      | gci.unGCI < 0 = Prose [ptext "^ Top" []]
      | otherwise = Prose [ptext "???" []]
    linkToExternalCell :: Bool -> GCI -> HtmlWeaver ()
    linkToExternalCell asListItem gci = do
      weaveConf <- ask
      case getLinkToGCI weaveConf gci of
        Nothing -> pure ()
        Just (_, externalLinkAnchor, externalCellName) -> do
          let externalLink =
                a_ [href_ externalLinkAnchor] $ do
                  weaveProse externalCellName
          if asListItem
            then li_ [] $ externalLink
            else externalLink

weaveNamedCellsIndex :: [Cell] -> HtmlWeaver ()
weaveNamedCellsIndex cells = do
  let namedCells = getNamedCells cells --
      lenNamedCells = T.show $ length namedCells
  when (length namedCells > 0) $ do
    div_
      [ id_ "heading-container-h--Named-cells",
        class_ "heading-container"
      ]
      $ do
        h3_ [id_ "h--Named-cells"] $ do
          a_ [href_ $ "#h--Named-cells"] $ do
            toHtml @Text $ "Named cells (" <> lenNamedCells <> ")"
        ol_ [] $ do
          forM_ namedCells $ \(prose, cell) -> do
            li_ [] $ do
              let linkAnchor = "#" <> getLinkAnchor Nothing cell
              a_ [href_ linkAnchor] $ do
                weaveProse prose
