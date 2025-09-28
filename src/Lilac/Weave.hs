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
import Data.Aeson (eitherDecodeStrictText)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Graph qualified as G
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lilac.Compile (makeRelativeToObjectPath, parseCitationStyle)
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
        objects,
        offsets,
        symbols
      ),
    BToken (BTokenLink, BTokenPadding, BTokenString),
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
    Citation (cites, noteNumber, prefix, styleVariation, suffix),
    Cite (id, locator, prefix, suffix),
    Figure (caption, link, meta),
    Footnote (body, name),
    GPath,
    Heading (body, level),
    Link (Link, name, target),
    LinkTarget
      ( LinkTargetFilePath,
        LinkTargetFootnote,
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
    OrgDoc (bibPaths, cells, citations, cslPath, footnoteNumbers, meta),
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
        projectConf,
        watch
      ),
    cellIdxToName,
    cellIdxToObjCell,
    decodeGPath,
    getProseContents,
    getSectionText,
    markingChars,
    markingCharsInvisible,
  )
import Lucid
  ( HtmlT,
    a_,
    alt_,
    aside_,
    async_,
    body_,
    br_,
    charset_,
    class_,
    code_,
    col_,
    colgroup_,
    crossorigin_,
    div_,
    doctypehtml_,
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
    Maybe (Just, Nothing),
    ReaderT,
    String,
    Text,
    Type,
    any,
    ask,
    catMaybes,
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
    not,
    notElem,
    null,
    otherwise,
    pure,
    replicate,
    reverse,
    runReaderT,
    sequence_,
    show,
    snd,
    sort,
    sortOn,
    take,
    takeWhile,
    when,
    zip,
    ($),
    (&&),
    (+),
    (-),
    (.),
    (/=),
    (<),
    (<$>),
    (<>),
    (=<<),
    (==),
    (>),
    (||),
  )
import Relude.Extra.Map (keys, lookup, lookupDefault, toPairs)
import Skylighting qualified as Sky
import System.IO (utf8)
import System.OsPath (OsPath)
import System.OsPath qualified as P
import Text.Blaze.Html.Renderer.Text (renderHtml)

type HtmlWeaver :: Type -> Type
type HtmlWeaver a = HtmlT (ReaderT WeaveConf Identity) a

weaveObject :: Object -> HtmlWeaver ()
weaveObject object = do
  weaveConf <- ask
  let title' = toHtml $ case lookup "title" object.orgDoc.meta of
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
      weaveMain object.orgDoc
      weaveSidebar . catMaybes $ map getBlock object.orgDoc.cells
  where
    getBlock = \case
      CBlock block -> Just block
      CHeading {}
      CListItem {}
      CParagraph {}
      CMathFragment {}
      CFigure {}
      CTable {}
      CFootnote {}
      CBibliography {} -> Nothing

weaveCell :: (Int, Cell) -> HtmlWeaver ()
weaveCell (cellIdx, cell) = case cell of
  CHeading heading -> weaveHeading cellIdx heading
  CListItem listMarkers -> sequence_ $ map weaveListMarker listMarkers
  CParagraph prose -> p_ $ weaveProse [cellIdx] prose
  CBlock block -> weaveBlock block
  CMathFragment mathFragment -> weaveMathFragmentStandalone mathFragment
  CFigure figure -> weaveFigure cellIdx figure
  CTable table -> weaveTable cellIdx table
  CFootnote footnote -> weaveFootnote cellIdx footnote
  CBibliography _ -> weaveBibliography

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
weaveHeading cellIdx heading =
  hN [id_ linkAnchor] $ do
    a_ [href_ $ "#" <> linkAnchor] $ do
      toHtml sectionText
      toHtmlRaw ("&emsp;&ensp;" :: Text)
      weaveProse [cellIdx] heading.body
  where
    hN = case heading.level of
      1 -> h2_
      2 -> h3_
      3 -> h4_
      4 -> h5_
      _ -> h6_
    sectionText = getSectionText heading
    linkAnchor = getLinkAnchor Nothing $ CHeading heading

getLinkAnchor :: Maybe Int -> Cell -> Text
getLinkAnchor localCellIdx = \case
  CHeading heading ->
    T.intercalate
      "-"
      [ "h",
        getSectionText heading,
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
  CBibliography {} -> ""

weaveProse :: [Int] -> Prose -> HtmlWeaver ()
weaveProse cellIdxs prose = do
  when (not $ null cellIdxs) $ do
    span_ [id_ ("c-" <> (T.intercalate "-" $ map T.show cellIdxs))] $ do
      pure ()
  sequence_ $ map weavePToken prose.unProse

weavePToken :: PToken -> HtmlWeaver ()
weavePToken = \case
  PTokenStyledText styledText -> weaveStyledText styledText
  PTokenLink link -> case link.target of
    LinkTargetFootnote {} -> do
      sup_ [class_ "footnote-link"] $ do
        weaveLink link
    LinkTargetOid {}
    LinkTargetFilePath {}
    LinkTargetURI {} -> weaveLink link
  PTokenMathFragment mathFragment -> weaveMathFragmentInline mathFragment
  PTokenCitation n -> weaveCitation n
  PTokenDivStart t -> toHtmlRaw ("<div class=\"csl-" <> t <> "\">" :: Text)
  PTokenDivEnd -> toHtmlRaw ("</div>" :: Text)

isExternalLink :: LinkTarget -> Bool
isExternalLink = \case
  LinkTargetOid {} -> False
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
  let b = toHtml $ styledText.body
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

weaveBlock :: Block -> HtmlWeaver ()
weaveBlock block = do
  weaveConf <- ask
  div_ [class_ "code-block-container", id_ linkAnchor] $ do
    div_ [class_ "side-popup"] $ do
      a_ [href_ $ "#" <> linkAnchor] $ do
        toHtmlRaw ("&#x1F517;" :: String)
    maybeWeaveNameAndTanglePath
    let bodyBorder
          | Nothing <- lookup "name" block.meta,
            Nothing <- lookup "tangle" block.meta =
              " fully-rounded"
          | otherwise = ""
    pre_ [class_ $ "code-block-body" <> bodyBorder] $ do
      let bodyText = getBlockContentsWithoutLinks block.body
      case lookup "__src_language" block.meta of
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
          BTokenLink (Link _name (LinkTargetOid _)) -> "\NUL"
          BTokenLink {} -> ""
    linkAnchor = getLinkAnchor Nothing $ CBlock block

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
  wovenLinkTarget <- weaveLinkTarget link.target
  let attrs = [href_ wovenLinkTarget] <> linkCategory wovenLinkTarget
  a_
    attrs
    $ sequence_
    $ map weaveStyledText link.name
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
      | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
        Just cellIdx <- lookup linkOid originObj.symbols,
        Just (_, internalCell) <- cellIdxToObjCell weaveConf.archive cellIdx ->
          "#" <> getLinkAnchor (Just cellIdx) internalCell
      | Just globalCellIdx <- lookup linkOid weaveConf.archive.symbols,
        Just (_, externalCell) <- cellIdxToObjCell weaveConf.archive globalCellIdx,
        Just (_, externalObjGPath) <- IntMap.lookupLE globalCellIdx weaveConf.archive.offsets,
        Just toObjPath <- P.encodeUtf $ decodeGPath externalObjGPath,
        Just fromObjPath <- P.encodeUtf $ decodeGPath weaveConf.originObjectPath,
        Right str <-
          P.decodeWith utf8 utf8
            $ P.replaceExtension (getRelativePathWithoutSymlinks fromObjPath toObjPath) htmlExt ->
          (T.pack str) <> "#" <> getLinkAnchor (Just globalCellIdx) externalCell
      | otherwise -> "#LILAC_BROKEN_LINK-" <> show linkOid
      where
        htmlExt = P.unsafeEncodeUtf "html"
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
      a_ [href_ $ "#" <> linkAnchor, class_ "self-link"] $ do
        toHtml captionPrefix
      toHtml $ case figure.caption.unProse of
        [] -> "." :: Text
        _ -> ": " :: Text
      weaveProse [cellIdx] figure.caption
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

weaveMathFragmentStandalone :: MathFragment -> HtmlWeaver ()
weaveMathFragmentStandalone mathFragment = do
  let (begMarker, endMarker)
        | Just _ <- lookup "__equation_number" mathFragment.meta =
            ("\\begin{equation}", "\\end{equation}")
        | otherwise = ("\\[", "\\]")
  p_ [id_ linkAnchor] $ do
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
      a_ [href_ $ "#" <> linkAnchor, class_ "self-link"] $ do
        toHtml captionPrefix
      toHtml $ case table.caption.unProse of
        [] -> "." :: Text
        _ -> ": " :: Text
      weaveProse [cellIdx] table.caption
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
            th_ [] $ weaveProse [] prose
      tbody_ [] $ do
        forM_ (rows visibleRows) $ \row ->
          if row == [Prose []]
            then do
              tr_ [class_ "table-horizontal-line"] $ do
                td_ [] $ toHtml @Text ""
            else do
              tr_ [] $ do
                forM_ row $ \prose -> do
                  let tdAttrs =
                        if mostlyNumeric prose
                          then [class_ "table-cell-numeric"]
                          else []
                  td_ tdAttrs $ weaveProse [] prose
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

weaveFootnote :: Int -> Footnote -> HtmlWeaver ()
weaveFootnote cellIdx footnote = do
  weaveConf <- ask
  if
    | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
      Just footnoteNumber <- lookup footnote.name originObj.orgDoc.footnoteNumbers -> do
        p_ [class_ "footnote-definition", id_ ("footnote-" <> footnote.name)] $ do
          span_ [class_ "footnote-number"] . toHtml $ T.show footnoteNumber <> ". "
          weaveProse [cellIdx] footnote.body
    | otherwise -> pure ()

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
    Just prose -> span_ [class_ "citation"] $ weaveProse [] prose
    Nothing -> pure ()

weaveBibliography :: HtmlWeaver ()
weaveBibliography = do
  weaveConf <- ask
  let result = weaveConf.citeprocResult
      bibliography = result.resultBibliography
  div_ [class_ "bibliography"] $ do
    forM_ bibliography $ \(key, prose) -> do
      let linkAnchor = "ref-" <> key
      div_ [class_ "bib-ref", id_ linkAnchor] $ do
        weaveProse [] prose

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
              a_ [href_ $ "#" <> linkAnchor] $ do
                weaveProse [] heading.body
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
      CTable {}
      CBibliography {} -> (actions, prevLevel)
    closeHeadingLevels :: Int -> HtmlWeaver ()
    closeHeadingLevels levelDiff =
      forM_ [1 .. levelDiff] $ \_ -> do
        weaveListMarker LItemEnd
        weaveListMarker LOrderedEnd

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

weaveMain :: OrgDoc -> HtmlWeaver ()
weaveMain orgDoc = do
  main_ $ do
    weaveDocMeta orgDoc.meta
    forM_ rearrangedCells weaveCell
    p_ [class_ "main-bottom-spacer"] $ toHtml @Text ""
  where
    cellsWithIdx = zip [0 :: Int ..] orgDoc.cells
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
        Just n <- lookup footnote.name orgDoc.footnoteNumbers =
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
      CTable {}
      CBibliography {} -> False

weaveSidebar :: [Block] -> HtmlWeaver ()
weaveSidebar blocks = do
  aside_ [class_ "sidebar"] $ do
    weaveTangledBlocksList blocks
    weaveBacklinks
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
              a_ [href_ $ "#" <> linkAnchor] $ do
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
          sequence_ $ map (weaveBacklink weaveConf) sorted
  where
    weaveBacklink :: WeaveConf -> (GPath, [G.Edge]) -> HtmlWeaver ()
    weaveBacklink _ (_, []) = toHtmlRaw @Text ""
    weaveBacklink weaveConf (externalObjGPath, edges) = do
      let externalOrgDocName
            | Just toObjPath <-
                P.encodeUtf $ decodeGPath externalObjGPath,
              Just fromObjPath <-
                P.encodeUtf $ decodeGPath weaveConf.originObjectPath,
              Right externalOrgDocName' <-
                P.decodeWith utf8 utf8
                  $ P.replaceExtension
                    (getRelativePathWithoutSymlinks fromObjPath toObjPath)
                    (P.unsafeEncodeUtf "org") =
                externalOrgDocName'
            | otherwise = "???"
          weight = T.show $ length edges
          f (externalIdx, internalIdx)
            | Just (_, externalCell) <-
                cellIdxToObjCell weaveConf.archive externalIdx,
              Just (_, internalCell) <-
                cellIdxToObjCell weaveConf.archive internalIdx,
              Just (offset, _) <-
                IntMap.lookupLE externalIdx weaveConf.archive.offsets,
              Just toObjPath <-
                P.encodeUtf $ decodeGPath externalObjGPath,
              Just fromObjPath <-
                P.encodeUtf $ decodeGPath weaveConf.originObjectPath,
              Right str <-
                P.decodeWith utf8 utf8
                  $ P.replaceExtension
                    (getRelativePathWithoutSymlinks fromObjPath toObjPath)
                    (P.unsafeEncodeUtf "html") = do
                let externalLinkAnchor =
                      T.pack str
                        <> "#"
                        <> (getLinkAnchor (Just externalCellIdx) externalCell)
                    internalLinkAnchor =
                      "#" <> (getLinkAnchor (Just internalCellIdx) internalCell)
                    externalName = fromMaybe "???" $ cellIdxToName weaveConf.archive externalIdx offset
                    internalName = fromMaybe "???" $ cellIdxToName weaveConf.archive internalIdx offset
                    externalCellIdx = externalIdx - offset
                    internalCellIdx = internalIdx - offset
                li_ [] $ do
                  a_ [href_ externalLinkAnchor] $ do
                    toHtml externalName
                  toHtmlRaw @Text " &#x2192; "
                  a_ [href_ internalLinkAnchor] $ do
                    toHtml internalName
            | otherwise = pure ()
      li_ [] $ do
        toHtml $ T.pack externalOrgDocName <> " (" <> weight <> ")"
        ol_ [] $ do
          forM_ (sort edges) f
