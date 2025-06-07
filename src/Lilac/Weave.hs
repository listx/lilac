module Lilac.Weave (weave, weaveToDisk) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Lilac.Parse
  ( Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Heading (body, level, meta, section),
    OrgDoc (cells, meta),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (body, style),
    maskToStyles,
  )
import Lilac.Util qualified as LU
import Relude
  ( IO,
    Maybe (Just, Nothing),
    foldl',
    map,
    reverse,
    sequence_,
    ($),
    (.),
  )
import Relude.Extra.Map qualified as EM
import System.OsPath qualified as P
import Text.Blaze
  ( (!),
  )
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

weave :: OrgDoc -> H.Html
weave od = H.docTypeHtml $ do
  H.head $ do
    H.title
      $ H.text
      $ case EM.lookup "title" od.meta of
        Just title -> title
        Nothing -> "Untitled"
  H.body $ do
    sequence_ $ map weaveCell od.cells

weaveCell :: Cell -> H.Html
weaveCell = \case
  CHeading heading -> weaveHeading heading
  CListItem _ -> H.p ""
  CParagraph _ -> H.p ""
  CBlock _ -> H.p ""
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
  H.text $ sectionText
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
  H.span ! A.class_ styles $ H.text $ styledText.body
  where
    styles =
      H.textValue
        . T.unwords
        . foldl' styleToClasses []
        $ maskToStyles styledText.style
    styleToClasses :: [T.Text] -> Style -> [T.Text]
    styleToClasses acc = \case
      Bold -> "bold" : acc
      Italic -> "italic" : acc
      Underline -> "underline" : acc
      Verbatim -> "verbatim" : acc
      Code -> "code" : acc

weaveToDisk :: P.OsPath -> OrgDoc -> IO ()
weaveToDisk fp od =
  LU.writeFileRobustly fp
    . T.pack
    . renderHtml
    $ weave od

maybeAssignId :: M.Map T.Text T.Text -> (H.Html -> H.Html) -> (H.Html -> H.Html)
maybeAssignId meta elt = case EM.lookup "ID" meta of
  Just uuidText -> elt ! (A.id $ H.textValue uuidText)
  Nothing -> elt
