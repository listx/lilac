module Lilac.Weave (weave, weaveToDisk) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lilac.Parse
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Heading (body, level, meta, section),
    Link (name, target),
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
    Maybe (Just, Nothing),
    foldMap',
    foldl',
    map,
    otherwise,
    pure,
    reverse,
    sequence_,
    when,
    ($),
    (.),
    (<>),
    (==),
    (>),
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

weave :: OrgDoc -> Bool -> H.Html
weave od watch = H.docTypeHtml $ do
  H.head $ do
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
    sequence_ $ map weaveCell od.cells

weaveCell :: Cell -> H.Html
weaveCell = \case
  CHeading heading -> weaveHeading heading
  CListItem listMarkers -> sequence_ $ map weaveListMarker listMarkers
  CParagraph prose -> H.p $ do
    sequence_ $ map weavePToken prose
  CBlock block -> weaveBlock block
  CMathFragment _ -> H.p ""
  CFigure _ -> H.p ""
  CTable _ -> H.p ""

weaveHeading :: Heading -> H.Html
weaveHeading heading = hN $ do
  let sectionText =
        T.intercalate "."
          . map T.show
          . reverse
          $ heading.section
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

weavePToken :: PToken -> H.Html
weavePToken = \case
  PTokenStyledText styledText -> weaveStyledText styledText
  PTokenLink _ -> H.text ""
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
      let lang = EM.lookup "__src_language" block.meta
      sequence_ $ map (weaveBToken lang) block.body
  where
    maybeAssignName =
      case EM.lookup "name" block.meta of
        Just name ->
          H.div ! A.class_ "code-block-name" $ do
            H.code $ H.text name
        Nothing -> pure ()

weaveBToken :: Maybe T.Text -> BToken -> H.Html
weaveBToken lang = \case
  BTokenString txt -> case lang of
    Just l -> do
      colorizeBlock l txt
      let trailingNewlines = T.takeWhileEnd (== '\n') txt
      when (T.length trailingNewlines > 0)
        . H.code
        $ H.text "\n"
    Nothing -> H.code $ H.text txt
  BTokenPadding -> pure ()
  BTokenLink link -> H.code $ weaveLink link

colorizeBlock :: T.Text -> T.Text -> H.Html
colorizeBlock lang txt
  | Just syntax <- Sky.lookupSyntax lang Sky.defaultSyntaxMap,
    Just lines <- foldMap' Just $ Sky.tokenize defaultTokeniserConfig syntax txt =
      Sky.formatHtmlInline Sky.defaultFormatOpts lines
  | otherwise = H.code $ H.text txt
  where
    defaultTokeniserConfig = Sky.TokenizerConfig Sky.defaultSyntaxMap False

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
