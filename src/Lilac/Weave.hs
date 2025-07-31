module Lilac.Weave
  ( commonPrefix,
    getRelativePathWithoutSymlinks,
    renderAsText,
    weaveObject,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lilac.Parse (isNumericText, maskToStyles, tableHorizontalLine)
import Lilac.Types
  ( Archive (objects, symbolPath, symbols),
    BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta),
    Cell
      ( CBlock,
        CFigure,
        CFootnote,
        CHeading,
        CListItem,
        CMathFragment,
        CParagraph,
        CTable
      ),
    Figure (caption, link, meta),
    Footnote (body, name),
    Heading (body, level, section),
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
    OrgDoc (cells, footnotes, meta),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Prose,
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (body, style),
    Table (caption, grid, meta),
    WeaveConf (archive, originObjectPath, watch),
    decodeGPath,
    markingChars,
    markingCharsInvisible,
  )
import Lucid
  ( HtmlT,
    a_,
    alt_,
    async_,
    body_,
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
    link_,
    meta_,
    p_,
    pre_,
    rel_,
    renderText,
    script_,
    span_,
    src_,
    sup_,
    table_,
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
import Network.URI qualified as N
import Relude
  ( Bool (False, True),
    Either (Right),
    Eq,
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
    drop,
    dropWhile,
    filter,
    flip,
    foldMap',
    foldl',
    forM_,
    fromMaybe,
    fst,
    id,
    isJust,
    length,
    map,
    max,
    maxBound,
    maybe,
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
    (<$>),
    (<>),
    (=<<),
    (==),
    (>),
    (||),
  )
import Relude.Extra.Map (keys, lookup, lookupDefault)
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
      weaveDocMeta object.orgDoc.meta
      forM_ rearrangedCells weaveCell
  where
    rearrangedCells = beforeFootnotes <> sortedFootnotes <> afterFootnotes
    beforeFootnotes = takeWhile (not . isFootnote) object.orgDoc.cells
    afterFootnotes =
      dropWhile isFootnote
        $ dropWhile (not . isFootnote) object.orgDoc.cells
    sortedFootnotes =
      sortOn getSortingKey
        $ takeWhile isFootnote
        $ dropWhile (not . isFootnote) object.orgDoc.cells
    getSortingKey cell
      | (CFootnote footnote) <- cell,
        Just (n, _) <- lookup footnote.name object.orgDoc.footnotes =
          n
      | otherwise = maxBound :: Int
    isFootnote = \case
      CFootnote {} -> True
      CBlock {}
      CFigure {}
      CHeading {}
      CListItem {}
      CMathFragment {}
      CParagraph {}
      CTable {} -> False

weaveCell :: Cell -> HtmlWeaver ()
weaveCell = \case
  CHeading heading -> weaveHeading heading
  CListItem listMarkers -> sequence_ $ map weaveListMarker listMarkers
  CParagraph prose -> p_ $ sequence_ $ map weavePToken prose
  CBlock block -> weaveBlock block
  CMathFragment mathFragment -> weaveMathFragmentStandalone mathFragment
  CFigure figure -> weaveFigure figure
  CTable table -> weaveTable table
  CFootnote footnote -> weaveFootnote footnote

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

weaveDocMeta :: M.Map Text Text -> HtmlWeaver ()
weaveDocMeta m = do
  case lookup "title" m of
    Nothing -> pure ()
    Just title -> do
      h1_ [] $ toHtml title
      hr_ []
  f "author" $ p_ [class_ "doc-meta-author"]
  f "date" $ p_ [class_ "doc-meta-date"]
  where
    f k element = case lookup k m of
      Nothing -> pure ()
      Just x -> element $ toHtml x

weaveHeading :: Heading -> HtmlWeaver ()
weaveHeading heading =
  hN [id_ linkAnchor] $ do
    a_ [href_ $ "#" <> linkAnchor] $ do
      toHtml $ sectionText <> " "
      sequence_ $ map weavePToken heading.body
  where
    hN = case heading.level of
      1 -> h2_
      2 -> h3_
      3 -> h4_
      4 -> h5_
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
  CFootnote {} -> ""

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

isExternalLink :: LinkTarget -> Bool
isExternalLink = \case
  LinkTargetOid {} -> False
  LinkTargetFootnote {} -> False
  LinkTargetFilePath {} -> True
  LinkTargetURI {} -> True

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
    let bodyBorder = case lookup "name" block.meta of
          Just _ -> ""
          Nothing -> " fully-rounded"
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
    maybeAssignName =
      case lookup "name" block.meta of
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
  wovenLinkTarget <- weaveLinkTarget link.target
  a_
    [ href_ wovenLinkTarget,
      class_ linkCategory
    ]
    $ sequence_
    $ map weaveStyledText link.name
  where
    linkCategory =
      if isExternalLink link.target
        then "external-link"
        else "internal-link"

weaveLinkTarget :: LinkTarget -> HtmlWeaver T.Text
weaveLinkTarget linkTarget = do
  weaveConf <- ask
  pure $ case linkTarget of
    LinkTargetOid linkOid
      | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
        Just internalCell <- lookup linkOid originObj.symbols ->
          "#" <> getLinkAnchor internalCell
      | Just externalCell <- lookup linkOid weaveConf.archive.symbols,
        Just externalObjGPath <- lookup linkOid weaveConf.archive.symbolPath,
        Just toObjPath <- P.encodeUtf $ decodeGPath externalObjGPath,
        Just fromObjPath <- P.encodeUtf $ decodeGPath weaveConf.originObjectPath,
        Right str <-
          P.decodeWith utf8 utf8
            $ P.replaceExtension (getRelativePathWithoutSymlinks fromObjPath toObjPath) htmlExt ->
          (T.pack str) <> "#" <> getLinkAnchor externalCell
      | otherwise -> "#LILAC_BROKEN_LINK-" <> show linkOid
      where
        htmlExt = P.unsafeEncodeUtf "html"
    LinkTargetFootnote name
      | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
        Just (footnoteNumber, _) <- lookup name originObj.orgDoc.footnotes ->
          (T.pack "#footnote-") <> (T.show footnoteNumber)
      | otherwise -> "#LILAC_BROKEN_LINK"
    LinkTargetFilePath path -> T.pack $ N.escapeURIString N.isUnescapedInURI path
    LinkTargetURI uri -> T.pack $ N.uriToString id uri ""

weaveFigure :: Figure -> HtmlWeaver ()
weaveFigure figure = do
  wovenLinkTarget <- weaveLinkTarget figure.link.target
  let captionPrefix =
        "Figure "
          <> lookupDefault "figure-number-error" "__figure_number" figure.meta
  figure_ [id_ linkAnchor] $ do
    figcaption_ [] $ do
      a_ [href_ $ "#" <> linkAnchor, class_ "self-link"] $ do
        toHtml captionPrefix
      toHtml $ case figure.caption of
        [] -> "." :: Text
        _ -> ": " :: Text
      sequence_ $ map weavePToken figure.caption
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
    linkAnchor = getLinkAnchor $ CFigure figure

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
    linkAnchor = getLinkAnchor $ CMathFragment mathFragment

weaveMathFragmentInline :: MathFragment -> HtmlWeaver ()
weaveMathFragmentInline mathFragment = "\\(" <> toHtml mathFragment.body <> "\\)"

weaveTable :: Table -> HtmlWeaver ()
weaveTable table = do
  let captionPrefix =
        "Table "
          <> lookupDefault "table-number-error" "__table_number" table.meta
  figure_ [id_ linkAnchor] $ do
    figcaption_ [] $ do
      a_ [href_ $ "#" <> linkAnchor, class_ "self-link"] $ do
        toHtml captionPrefix
      toHtml $ case table.caption of
        [] -> "." :: Text
        _ -> ": " :: Text
      sequence_ $ map weavePToken table.caption
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
            th_ [] $ sequence_ $ map weavePToken prose
      tbody_ [] $ do
        forM_ (rows visibleRows) $ \row ->
          if row == [[]]
            then do
              tr_ [class_ "table-horizontal-line"] ""
            else do
              tr_ [] $ do
                forM_ row $ \prose -> do
                  let tdAttrs =
                        if mostlyNumeric prose
                          then [class_ "table-cell-numeric"]
                          else []
                  td_ tdAttrs $ sequence_ $ map weavePToken prose
  where
    linkAnchor = getLinkAnchor $ CTable table
    hideMarkingCharInFirstColumn [] = []
    hideMarkingCharInFirstColumn (((_row, _col), prose) : cells)
      | prose == tableHorizontalLine = [[]]
      | markingCharExistsInFirstColumn = map snd cells
      | otherwise = prose : map snd cells
    markingCharExistsInFirstColumn = any (not . visibleColumn) firstColumnCells
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
    visibleColumn (_, []) = True
    visibleColumn (_, prose) = case prose of
      [PTokenStyledText styledText] ->
        notElem styledText.body $ map T.singleton markingChars
      _ -> True
    visibleRow :: ((Int, Int), Prose) -> Bool
    visibleRow (_, []) = True
    visibleRow (_, prose) = case prose of
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
    getColgroupStarts [] _prose = []
    getColgroupStarts [0] _prose = [1]
    getColgroupStarts acc@(latest : rest) prose
      | proseHasColgroupStartMarker prose = 1 : acc
      | otherwise = latest + 1 : rest
    getColgroupsRow = foldl' h Nothing firstColumnCells
    h :: Maybe Int -> ((Int, Int), Prose) -> Maybe Int
    h acc ((row, _col), prose)
      | isJust acc = acc
      | proseHasNonExportMarker prose = Just row
      | otherwise = acc
    proseHas _ [] = False
    proseHas prefix (firstPToken : _) = case firstPToken of
      PTokenStyledText styledText ->
        (maskToStyles styledText.style == [])
          && T.isPrefixOf prefix styledText.body
      PTokenLink {}
      PTokenMathFragment {} -> False
    proseHasColgroupStartMarker = proseHas "<"
    proseHasNonExportMarker = proseHas "/"
    mostlyNumeric prose =
      let rawText = T.concat $ map extractText prose
       in isNumericText rawText
    extractText pToken = case pToken of
      PTokenStyledText styledText -> styledText.body
      PTokenLink {}
      PTokenMathFragment {} -> ""

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

weaveFootnote :: Footnote -> HtmlWeaver ()
weaveFootnote footnote = do
  weaveConf <- ask
  if
    | Just originObj <- lookup weaveConf.originObjectPath weaveConf.archive.objects,
      Just (footnoteNumber, _) <- lookup footnote.name originObj.orgDoc.footnotes -> do
        p_ [class_ "footnote-definition", id_ ("footnote-" <> T.show footnoteNumber)] $ do
          span_ [class_ "footnote-number"] . toHtml $ T.show footnoteNumber <> ". "
          sequence_ $ map weavePToken footnote.body
    | otherwise -> pure ()

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix xs ys = map fst . takeWhile (\(x, y) -> (x == y)) $ zip xs ys

renderAsText :: WeaveConf -> HtmlWeaver a -> Text
renderAsText weaveConf weaver =
  TL.toStrict
    . renderText
    $ hoistHtmlT (flip runReaderT weaveConf) weaver
