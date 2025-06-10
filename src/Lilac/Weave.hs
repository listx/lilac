module Lilac.Weave (weave, weaveToDisk) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lilac.Parse
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Heading (body, level, meta, section),
    Link (Link, name, target),
    LinkNoweb (LinkNoweb, name, uuid),
    LinkTarget (LinkTargetNoweb, LinkTargetOther),
    ListMarker
      ( LItemEnd,
        LItemStart,
        LOrderedEnd,
        LOrderedStart,
        LUnorderedEnd,
        LUnorderedStart
      ),
    MathFragment (MathFragment, body),
    OrgDoc (cells, meta),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (body, style),
    maskToStyles,
  )
import Lilac.Util qualified as LU
import Relude
  ( Bool (False),
    IO,
    Int,
    Maybe (Just, Nothing),
    Type,
    catMaybes,
    drop,
    foldMap',
    foldl',
    foldlM,
    forM_,
    fst,
    map,
    otherwise,
    pure,
    reverse,
    sequence_,
    take,
    when,
    ($),
    (.),
    (<>),
    (==),
    (||),
  )
import Relude.Extra.Map qualified as EM
import Skylighting qualified as Sky
import System.OsPath qualified as P
import Text.Blaze
  ( (!),
  )
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal qualified as BI

type DomIDs :: Type
type DomIDs = M.Map T.Text Int

weave :: OrgDoc -> Bool -> H.Html
weave od watch = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.title
      $ H.text
      $ case EM.lookup "title" od.meta of
        Just title -> title
        Nothing -> "Untitled"
    when watch
      $ H.script
      ! A.type_ "text/javascript"
      ! A.src "https://livejs.com/live.js"
      $ ""

    H.link ! A.rel "stylesheet" ! A.href "./css/default.css"
    H.link ! A.rel "preconnect" ! A.href "https://fonts.googleapis.com"
    H.link
      ! A.rel "preconnect"
      ! A.href "https://fonts.gstatic.com"
      ! H.customAttribute "crossorigin" "anonymous"
    let fontsLink =
          H.preEscapedTextValue
            $ T.intercalate
              ""
              [ "https://fonts.googleapis.com/css2?display=swap",
                "&family=Source+Code+Pro:ital,wght@0,200..900;1,200..900",
                "&family=Source+Sans+3:ital,wght@0,200..900;1,200..900",
                "&family=Source+Serif+4:ital,opsz,wght@0,8..60,200..900;1,8..60,200..900"
              ]
    H.link ! A.rel "stylesheet" ! A.href fontsLink
  H.body $ do
    _ <- foldlM weaveCell M.empty od.cells
    pure ()

weaveCell ::
  DomIDs ->
  Cell ->
  BI.MarkupM (DomIDs)
weaveCell domIDs cell = do
  newDomIDs <- case cell of
    CHeading heading -> do
      weaveHeading heading
      pure domIDs
    CListItem listMarkers -> do
      sequence_ $ map weaveListMarker listMarkers
      pure domIDs
    CParagraph prose -> do
      H.p $ do
        sequence_ $ map weavePToken prose
      pure domIDs
    CBlock block -> do
      weaveBlock block
      pure domIDs
    CMathFragment _ -> do
      H.p ""
      pure domIDs
    CFigure _ -> do
      H.p ""
      pure domIDs
    CTable _ -> do
      H.p ""
      pure domIDs
  pure newDomIDs

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

weaveHeading :: Heading -> H.Html
weaveHeading heading = hN ! (A.id $ H.textValue desiredDomID) $ do
  H.a ! (A.href . H.textValue $ "#" <> desiredDomID) $ do
    H.text $ sectionText <> " "
    sequence_ $ map weavePToken heading.body
  where
    hN =
      maybeAssignId heading.meta
        $ case heading.level of
          1 -> H.h1
          2 -> H.h2
          3 -> H.h3
          4 -> H.h4
          5 -> H.h5
          _ -> H.h6
    sectionText =
      T.intercalate "."
        . map T.show
        . reverse
        $ heading.section
    desiredDomID =
      T.intercalate
        "-"
        [ "h",
          sectionText,
          toSafeHtmlId $ getProseContents heading.body
        ]

weavePToken :: PToken -> H.Html
weavePToken = \case
  PTokenStyledText styledText -> weaveStyledText styledText
  PTokenLink link ->
    let targetText = case link.target of
          LinkTargetNoweb _ -> "UNSUPPORTED"
          LinkTargetOther t -> t
     in H.a ! (A.href $ H.textValue targetText) $ do
          forM_ link.name weaveStyledText
  PTokenMathFragment _ -> H.text ""

weaveStyledText :: StyledText -> H.Html
weaveStyledText styledText = do
  if styles == []
    then H.text $ styledText.body
    else
      H.span
        ! A.class_ (H.textValue $ T.unwords styles)
        $ H.text
        $ styledText.body
  where
    styles = foldl' styleToClasses [] $ maskToStyles styledText.style
    styleToClasses :: [T.Text] -> Style -> [T.Text]
    styleToClasses acc = \case
      Bold -> "bold" : acc
      Italic -> "italic" : acc
      Underline -> "underline" : acc
      Verbatim -> "verbatim" : acc
      Code -> "code" : acc

weaveListMarker :: ListMarker -> H.Html
weaveListMarker =
  H.preEscapedToMarkup . \case
    LOrderedStart -> "<ol>" :: T.Text
    LOrderedEnd -> "</ol>"
    LUnorderedStart -> "<ul>"
    LUnorderedEnd -> "</ul>"
    LItemStart -> "<li>"
    LItemEnd -> "</li>"

weaveBlock :: Block -> H.Html
weaveBlock block = do
  H.div ! A.class_ "code-block-container" $ do
    maybeAssignName
    let bodyBorder = case EM.lookup "name" block.meta of
          Just _ -> ""
          Nothing -> " fully-rounded"
    H.pre ! A.class_ ("code-block-body" <> bodyBorder) $ do
      let bodyText = getBlockContentsWithoutLinks block.body
      case EM.lookup "__src_language" block.meta of
        Just lang ->
          let colorized = TL.toStrict . renderHtml $ colorizeText lang bodyText
              getLink = \case
                BTokenLink l -> Just l
                BTokenString {}
                BTokenPadding {} -> Nothing
              links = catMaybes $ map getLink block.body
           in H.preEscapedText $ spliceLinks colorized links
        Nothing -> H.code $ H.text bodyText
  where
    maybeAssignName =
      case EM.lookup "name" block.meta of
        Just name ->
          H.div ! A.class_ "code-block-name" $ do
            H.code $ H.text name
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

colorizeText :: T.Text -> T.Text -> H.Html
colorizeText lang txt
  | Just syntax <- Sky.lookupSyntax lang Sky.defaultSyntaxMap,
    Just lines <- foldMap' Just $ Sky.tokenize defaultTokeniserConfig syntax txt =
      Sky.formatHtmlInline Sky.defaultFormatOpts lines
  | otherwise = H.code $ H.text txt
  where
    defaultTokeniserConfig = Sky.TokenizerConfig Sky.defaultSyntaxMap False

spliceLinks :: T.Text -> [Link] -> T.Text
spliceLinks colorized links =
  let colorizedStrs = T.splitOn "\NUL" colorized
      f :: ([T.Text], [Link]) -> T.Text -> ([T.Text], [Link])
      f (acc, []) colorizedStr = (colorizedStr : acc, [])
      f (acc, link : links') colorizedStr =
        ( colorizedStr : (TL.toStrict . renderHtml $ weaveLink link) : acc,
          links'
        )
   in T.intercalate ""
        . reverse
        . fst
        . foldl' f (take 1 colorizedStrs, links)
        $ drop 1 colorizedStrs

weaveLink :: Link -> H.Html
weaveLink link = do
  H.a ! A.href "h" $ sequence_ $ map weaveStyledText link.name

weaveToDisk :: P.OsPath -> OrgDoc -> Bool -> IO ()
weaveToDisk fp od watch =
  LU.writeFileRobustly fp
    . TL.toStrict
    . renderHtml
    $ weave od watch

maybeAssignId :: M.Map T.Text T.Text -> (H.Html -> H.Html) -> (H.Html -> H.Html)
maybeAssignId meta elt = case EM.lookup "ID" meta of
  Just uuidText -> elt ! (A.id $ H.textValue uuidText)
  Nothing -> elt
