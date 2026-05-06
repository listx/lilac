module Lilac.Weave.Css (genCss) where

import Clay
  ( Color,
    Css,
    Size,
    a,
    absolute,
    alignSide,
    aside,
    auto,
    backgroundColor,
    before,
    block,
    body,
    bold,
    border,
    borderBottom,
    borderBottomLeftRadius,
    borderBottomRightRadius,
    borderBottomWidth,
    borderBox,
    borderCollapse,
    borderLeft,
    borderRadius,
    borderRight,
    borderStyle,
    borderTop,
    borderTopLeftRadius,
    borderTopRightRadius,
    bottom,
    boxSizing,
    button,
    center,
    code,
    collapse,
    color,
    column,
    content,
    dd,
    display,
    displayNone,
    displayTable,
    div,
    dl,
    dt,
    em,
    empty,
    figcaption,
    figure,
    fitContent,
    flex,
    flexDirection,
    float,
    floatLeft,
    focus,
    fontFamily,
    fontSize,
    fontStyle,
    fontVariant,
    fontWeight,
    footer,
    fr,
    grid,
    gridTemplateColumns,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    height,
    hidden,
    hover,
    hr,
    html,
    inline,
    inlineBlock,
    italic,
    left,
    li,
    listStyleType,
    main_,
    margin,
    marginBottom,
    marginLeft,
    marginRight,
    marginTop,
    maxWidth,
    minHeight,
    monospace,
    nav,
    none,
    normal,
    ol,
    opacity,
    other,
    outlineWidth,
    overflowX,
    overflowY,
    p,
    padding,
    paddingLeft,
    paddingRight,
    pct,
    position,
    pre,
    pretty,
    pt,
    px,
    relative,
    rem,
    renderWith,
    rgb,
    sansSerif,
    serif,
    sideRight,
    smallCaps,
    solid,
    span,
    stringContent,
    sup,
    table,
    tbody,
    td,
    textAlign,
    textDecoration,
    th,
    thead,
    top,
    tr,
    ul,
    underline,
    vAlignSub,
    vAlignSuper,
    verticalAlign,
    vh,
    visibility,
    visible,
    whiteSpace,
    width,
    (#),
    (&),
    (**),
    (-:),
    (?),
    (|>),
  )
import Clay.Text qualified as ClayText
import Data.Bits
  ( shiftR,
    (.&.),
  )
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Relude
  ( Bool (True),
    Int,
    Maybe (Just),
    String,
    Text,
    Type,
    Void,
    concatMap,
    forM_,
    fromMaybe,
    otherwise,
    pure,
    zip,
    ($),
    (.),
    (<),
    (<>),
    (>),
  )
import Relude.Extra.Map (insert)
import Skylighting qualified as Sky
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (hexadecimal)

genCss :: Text
genCss =
  T.unlines
    [ T.pack $ Sky.styleToCss kateLilac, --
      TL.toStrict $ renderWith pretty [] defaultCss
    ]

kateLilac :: Sky.Style
kateLilac =
  Sky.kate
    { Sky.tokenStyles =
        insert
          Sky.ExtensionTok --
          ( Sky.defStyle
              { Sky.tokenColor = Just (Sky.RGB 0 0x60 0xa4),
                Sky.tokenBold = True
              }
          )
          Sky.kate.tokenStyles
    }

defaultCss :: Css
defaultCss = do
  html ? do
    ev margin $ px 0
    ev padding $ px 0
    backgroundColor colorBg
    color colorText
    fontSerif
  body ? do
    minHeight (vh 100)
    display grid
    gridTemplateColumns [px 300, px 800, other "minmax(300px, 1fr)"]
    "grid-template-rows" -: "auto 1fr"
    "grid-template-areas"
      -: """
         "navbar navbar navbar"
         "toc    main   sidebar"
         """
    yx margin (px 0) auto
    overflowY hidden
  div # ".navbar" ? do
    "grid-area" -: "navbar"
    height $ rem 4
    borderBottom (px 1) solid colorCodeBorder --
    backgroundColor colorBg
    div # ".breadcrumbs" ? do
      fontSans
      fontSize $ rem 1.6
      margin (px 12) 0 (px 10) (px 20)
    span # ".breadcrumb-separator" ? do
      color colorBreadcrumbSeparator

  nav # ".toc" ? do
    "grid-area" -: "toc"
    "height"
      -: """
         calc(100vh - calc(4rem + 1px))
         """
    overflowY auto
    padding (px 0) (px 20) (px 0) (px 20)
    paddingLeft (px 30)
    fontSans
    ol ? do
      marginTop $ px 0
      paddingLeft $ rem 0
      listStyleType none
      li # ".toc-level-1" ? do
        position relative
        before & do
          content $ stringContent "■" --
          marginLeft (rem $ -1.3)
          position absolute
          left (px 0)
      li # ".toc-level-2" ? do
        position relative
        before & do
          content $ stringContent "○" --
          marginLeft (rem $ -1.3)
          position absolute
          left (px 0)
      div ? do
        paddingRight $ px 8
        paddingLeft $ px 20
        marginLeft $ px (-20)
        ".active" & do
          backgroundColor colorTocActive
          borderRight (px 4) solid colorNowebLinkFg
          paddingRight $ px 4
        ".active-ancestor" & do
          backgroundColor colorTocActiveAncestor
    span # ".toc-counter" ? do
      marginRight $ rem 0.3
    borderRight (px 1) solid colorCodeBorder

  nav # ".gtoc" ? do
    fontSans
    ul ? do
      marginTop $ px 0
      paddingLeft $ rem 1

  main_ ? do
    "grid-area" -: "main"
    marginLeft (px 17)
    "height"
      -: """
         calc(100vh - calc(4rem + 1px))
         """
    overflowY auto
    padding (px 0) (px 20) (px 0) (px 20)
    paddingLeft (px 50)
    p # ".main-bottom-spacer" ? do
      yx margin (px 40) 0
    focus & do
      --
      (outlineWidth (px 0))

  aside ? do
    "grid-area" -: "sidebar"
    "height"
      -: """
         calc(100vh - calc(4rem + 1px))
         """
    overflowY auto
    padding (px 0) (px 20) (px 0) (px 20)
    fontSans
    borderLeft (px 1) solid colorCodeBorder
    display flex
    flexDirection column
    ol ? do
      paddingLeft $ rem 1
      marginTop $ px 0
  footer ? do
    marginLeft (px 20)
    marginRight (px 20)
    marginTop auto
    div # ".lilac-stamp" ? do
      textAlign $ alignSide sideRight
      fontSans
      fontSize $ rem 0.9
      marginBottom (px 10) --
  p # ".widget-title" --
    ? do
      fontWeight bold
      marginBottom $ px 0
  div ? do
    --
    ev margin $ px 0

  hr ? do
    border (px 0) solid colorCodeBorder
    borderBottom (px 1) solid colorCodeBorder
    ".title-underline" & do
      marginBottom (rem 1)

  forM_ (zip [(1 :: Int) ..] [h1, h2, h3, h4, h5, h6]) --
    $ \(n, hN) -> do
      hN ? do
        case n of
          1 -> do
            fontSize (pt 36)
            headingPrefix "●" (rem $ -2.8) (px $ -3) --
          2 -> do
            fontSize (pt 26)
            headingPrefix "■" (rem $ -2.5) (px $ -3) --
          3 -> do
            fontSize (pt 22)
            headingPrefix "○" (rem $ -2.25) (px $ -1) --
          4 -> do
            fontSize (pt 18)
          5 -> do
            fontSize (pt 16)
          6 -> do
            fontSize (pt 14)
          _ -> pure ()
        position relative
        fontSans
        marginTop (px 0)
        marginBottom (px 0)
        a ? do
          color $ colorFrom "#000000"
          ":visited" & do
            color colorText
        border (px 1) solid colorBg
        yx padding 0 (px 5)
        marginLeft (px (-7))
        maxWidth fitContent
        ".active" & do
          border (px 1) solid colorActiveBorder
          backgroundColor colorActiveBg
          ev borderRadius (px 5)

  ".bold" & do
    fontWeight bold
  ".italic" & do
    fontStyle italic
  ".underline" & do
    textDecoration underline
  ".verbatim" & do
    fontMono
    whiteSpace ClayText.pre
  ".code" & do
    fontMono
    whiteSpace ClayText.pre
    backgroundColor colorCodeBg
    border (px 1) solid colorCodeBorder
    ev borderRadius (px 4)
    yx padding 0 (em 0.2)
  ".smallcaps" & do
    fontVariant smallCaps
  ".subscript" & do
    verticalAlign vAlignSub
  ".superscript" & do
    verticalAlign vAlignSuper

  a ? do
    color colorNowebLinkFg
    textDecoration none
    ":hover" & do
      textDecoration underline
    ":visited" & do
      color colorNowebLinkVisitedFg
      backgroundColor colorNowebLinkVisitedBg
      textDecoration none
      ":hover" & do
        textDecoration underline
        border (px 1) solid colorNowebLinkVisitedFg
    ".self-link" & do
      color colorText
      ":visited" & do
        color colorText
    ".internal-link" & do
      fontMono
      roundedRectangleLink --
    ".external-link" & do
      color colorNowebLinkFg
      ":visited" & do
        color colorNowebLinkVisitedFg
      "::after" & do
        let rawSvg :: String
            rawSvg =
              """
              <svg xmlns="http://www.w3.org/2000/svg"
                   class="external-link"
                   viewBox="0 0 60 60">

              <path fill="COLOR" d="M36 24c-1.2 0-2 0.8-2 2v12c0 1.2-0.8 2-2 2h-22c-1.2
              0-2-0.8-2-2v-22c0-1.2 0.8-2 2-2h12c1.2 0 2-0.8 2-2s-0.8-2-2-2h-12c-3.4 0-6 2.6-6
              6v22c0 3.4 2.6 6 6 6h22c3.4 0 6-2.6 6-6v-12c0-1.2-0.8-2-2-2z"></path>

              <path fill="COLOR" d="M43.8
              5.2c-0.2-0.4-0.6-0.8-1-1-0.2-0.2-0.6-0.2-0.8-0.2h-12c-1.2 0-2 0.8-2 2s0.8 2 2
              2h7.2l-18.6 18.6c-0.8 0.8-0.8 2 0 2.8 0.4 0.4 0.8 0.6 1.4 0.6s1-0.2
              1.4-0.6l18.6-18.6v7.2c0 1.2 0.8 2 2 2s2-0.8 2-2v-12c0-0.2
              0-0.6-0.2-0.8z"></path>

              </svg>
              """
            basicEncoder = \case
              '"' -> "%22"
              '\n' -> " "
              c -> [c]
            inlineSvg =
              T.intercalate
                (T.append "%23" $ T.drop 1 colorNowebLinkFgTxt)
                . T.splitOn
                  "COLOR"
                . T.pack
                $ concatMap basicEncoder rawSvg
            cssProp =
              "url('data:image/svg+xml;charset=UTF-8,"
                <> inlineSvg
                <> "')"
        "content" -: cssProp
        display inlineBlock
        width (rem 1)
        height (rem 1)
        margin 0 (rem (-0.2)) 0 0

  main_ ? do
    ol ? do
      "list-style" -: "none" --
      "counter-reset" -: "custom-list-counter" --
    ol |> li ? do
      "counter-increment" -: "custom-list-counter" --
      position relative
      before & do
        "content" -: "counter(custom-list-counter) \")\"" --
        position absolute
        left (rem (-2.4))
        width (rem 2)
        textAlign $ alignSide sideRight

  main_ ? do
    ol ? do
      li ? do
        empty & do
          minHeight (rem 2.2)

  ".code-block-container" & do
    display grid
    gridTemplateColumns [px 50, fr 1]
    margin (px 16) 0 (px 16) (px (-50))
    position relative
    fontSize (rem 0.9)
    a ? do
      display inlineBlock
      yx margin (px 2) 0
  forM_
    [".code-block-meta", ".code-block-meta-raw", ".code-block-body"]
    $ \child -> do
      ".code-block-container" |> child ? do
        "grid-column" -: "2"

  ".code-block-controls" & do
    padding (px 2) (px 5) 0 0
    width (px 50)
    textAlign $ alignSide sideRight
    boxSizing borderBox
    top (px 0)
    bottom (px 0)
    position absolute
    "grid-column" -: "1"
    "grid-row" -: "1"
    visibility hidden --
    opacity 0 --
    a # hover ? do
      textDecoration none
    button # ".toggle-code-block-meta-raw" ? do
      "::before" & do
        "content" -: "\"M\""
      marginRight $ px 5
      "border" -: "0"
      "cursor" -: "pointer"
      ev borderRadius (px 2)
      border (px 1) solid colorCodeBorder
  (".code-block-container" # hover) |> ".code-block-controls" ? do
    visibility visible
    opacity 1

  ".code-block-meta" & do
    yx padding 0 (px 8)
    border (px 1) solid colorCodeBorder
    borderBottomWidth 0
    borderTopLeftRadius (px 5) (px 5)
    borderTopRightRadius (px 5) (px 5)
    fontSans
    a ? do
      ".parent-block-arrow" & do
        marginLeft (em 0.25)
  ".active" ** ".code-block-meta" ? do
    fontWeight bold
    border (px 1) solid colorActiveBorder
    borderBottomWidth 0

  div # ".code-block-meta-raw" ? do
    yx padding 0 (px 8)
    fontMono
    border (px 1) solid colorCodeBorder
    borderBottomWidth 0
    ".hidden" & do
      display displayNone
  ".active" ** ".code-block-meta-raw" ? do
    border (px 1) solid colorActiveBorder
    borderBottomWidth 0

  pre ? do
    ev margin $ px 0
  code ? do
    ev margin $ px 0
    a ? do
      roundedRectangleLink
      span ? do
        color colorNowebLinkFg
  ".sourceCode" & do
    ev margin $ px 0
  ".code-block-body" & do
    overflowX auto
    fontMono
    fontSize (rem 0.9)
    backgroundColor colorCodeBg
    color colorCodeFg
    borderBottomLeftRadius (px 5) (px 5)
    borderBottomRightRadius (px 5) (px 5)
    border (px 1) solid colorCodeBorder
    yx padding (px 2) (px 8)
  ".active" ** ".code-block-body" ? do
    border (px 1) solid colorActiveBorder
    backgroundColor colorActiveBg
  ".code-block-body" ** ".sourceCode" ** ".active" ? do
    border (px 1) solid colorActiveBorder
    backgroundColor colorActiveBg
    "::after" & do
      "content" -: "\" 👈\""

  ".fully-rounded" & do
    ev borderRadius (px 5)
  ".top-rounded" & do
    borderTopLeftRadius (px 5) (px 5)
    borderTopRightRadius (px 5) (px 5)

  ".figure-image" & do
    display block
    yx margin (px 0) auto
  figcaption ? do
    yx margin (px 10) auto
    yx padding 0 (px 5)
    textAlign center
    fontSans
    maxWidth (pct 80)
    display displayTable
    border (px 1) solid colorBg
    ev borderRadius (px 5)
  ".caption-prefix" & do
    fontWeight bold
  figure # ".active" ? do
    figcaption ? do
      border (px 1) solid colorActiveBorder
      backgroundColor colorActiveBg
      ev borderRadius (px 5)

  table ? do
    fontSans
    borderCollapse collapse
    border (px 2) solid colorCodeBorder
    yx margin (px 0) auto
    th ? do
      yx padding (px 4) (px 8)
    td ? do
      yx padding (px 4) (px 8)
      ".table-cell-numeric" & do
        textAlign $ alignSide sideRight
    thead ? do
      backgroundColor colorCodeBg
    tbody ? do
      tr ? do
        ".table-horizontal-line" & do
          borderTop (px 2) solid colorCodeBorder
          ev padding (px 0)
          td ? do
            backgroundColor colorCodeBorder
            yx padding (px 0) (px 0)
        ".table-body-row" & do
          borderTop (px 1) solid colorCodeBorder
  ".table-vertical-thick-line-colgroup" & do
    borderLeft (px 2) solid colorCodeBorder --
  ".table-vertical-thin-line-col" & do
    --
    borderLeft (px 1) solid colorCodeBorder

  ".bibliography" & do
    marginTop (rem 1)
  ".bib-ref" & do
    marginTop (rem 0.5)
  ".csl-left-margin" & do
    float floatLeft
  ".csl-right-inline" & do
    margin 0 0 0 (rem 2)

  sup ? do
    ".footnote-link" & do
      a ? do
        borderStyle none
        ev borderRadius (px 0)
        yx padding 0 (px 0)
        yx margin (px 0) 0
        fontStyle normal
        ".internal-link" & do
          backgroundColor $ colorFrom "#ffffff"
          ":hover" & do
            borderStyle none
            textDecoration underline
          ":visited" & do
            ":hover" & do
              borderStyle none
              textDecoration underline
  p # ".footnote-definition" ? do
    border (px 1) solid colorBg
    ev borderRadius (px 5)
    marginLeft (px (-7))
    yx padding 0 (px 5)
    ".active" & do
      border (px 1) solid colorActiveBorder
      backgroundColor colorActiveBg
      ev borderRadius (px 5)
    a # ".footnote-backlink" ? do
      display inlineBlock

  dl ? do
    marginTop $ rem 1
  dt ? do
    display inlineBlock
    fontWeight bold
    fontSans
    ".inner-dt" & do
      marginTop $ rem 1
  dd ? do
    display inline
    marginLeft (rem 0.5)
    "::after" & do
      "content" -: "\"\\a\"" --
      whiteSpace ClayText.pre
    a # ".glossary-term-anchor" ? do
      fontSize (em 0.7)
      ":hover" & do
        textDecoration none
  div # ".glossary-term" ? do
    border (px 1) solid colorBg
    yx padding (px 0) (px 5)
    ".active" & do
      border (px 1) solid colorActiveBorder
      backgroundColor colorActiveBg
      ev borderRadius (px 5)
  span # ".glossary-external-source" ? do
    fontSans

  ".doc-meta-author" & do
    fontStyle italic
    ev margin (px 2)
  ".doc-meta-date" & do
    fontStyle italic
    ev margin (px 2)

roundedRectangleLink :: Css
roundedRectangleLink = do
  color colorNowebLinkFg
  backgroundColor colorNowebLinkBg
  ev borderRadius (px 5)
  yx padding (px 1) (px 3)
  border (px 1) solid colorCodeBorder
  ":hover" & do
    border (px 1) solid colorNowebLinkFg
    textDecoration none
  ":visited" & do
    ":hover" & do
      border (px 1) solid colorNowebLinkVisitedFg
      textDecoration none

yx ::
  (Size sz -> Size sz -> Size sz -> Size sz -> Css) ->
  Size sz ->
  Size sz ->
  Css
yx f x y = f x y x y

ev :: (Size sz -> Size sz -> Size sz -> Size sz -> Css) -> Size sz -> Css
ev f x = f x x x x

colorFrom :: Text -> Color
colorFrom input = fromMaybe (rgb 255 0 0) $ MP.parseMaybe hexColorParser input

hexColorParser :: ColorParser Color
hexColorParser = do
  _ <- char '#'
  n <- hexadecimal
  let c
        | n > 0xffffff = rgb 255 255 255
        | n < 0 = rgb 0 0 0
        | otherwise = rgb rr gg bb
        where
          rr = 0xff .&. shiftR n 16
          gg = 0xff .&. shiftR n 8
          bb = 0xff .&. n
  pure c

colorBg :: Color
colorBg = colorFrom "#ffffff"

colorText :: Color
colorText = colorFrom "#000000"

colorCodeFg :: Color
colorCodeFg = colorFrom "#1f1c1b"

colorCodeBg :: Color
colorCodeBg = colorFrom "#f7f7f7"

colorCodeBorder :: Color
colorCodeBorder = colorFrom "#cccccc"

colorBreadcrumbSeparator :: Color
colorBreadcrumbSeparator = colorFrom "#888888"

colorNowebLinkFg :: Color
colorNowebLinkFg = colorFrom "#2161ac"

colorNowebLinkFgTxt :: Text
colorNowebLinkFgTxt = "#2161ac"

colorNowebLinkBg :: Color
colorNowebLinkBg = colorFrom "#d8f6ff"

colorNowebLinkVisitedFg :: Color
colorNowebLinkVisitedFg = colorFrom "#7d3c98"

colorNowebLinkVisitedBg :: Color
colorNowebLinkVisitedBg = colorFrom "#f8e9ff"

colorTocActive :: Color
colorTocActive = colorFrom "#eafaff"

colorTocActiveAncestor :: Color
colorTocActiveAncestor = colorFrom "#ededed"

colorActiveBg :: Color
colorActiveBg = colorFrom "#f1ffef"

colorActiveBorder :: Color
colorActiveBorder = colorFrom "#008000"

type ColorParser :: Type -> Type
type ColorParser = MP.Parsec Void Text

fontSerif :: Css
fontSerif = do
  fontFamily ["Source Serif 4"] [serif]

fontSans :: Css
fontSans = do
  fontFamily ["Source Sans 3"] [sansSerif]

fontMono :: Css
fontMono = do
  fontFamily ["Source Code Pro"] [monospace]
  fontSize (em 0.9)

headingPrefix :: Text -> Size sz -> Size sz -> Css
headingPrefix prefix spacing fromTop =
  before & do
    content $ stringContent prefix
    marginLeft spacing
    position absolute
    top fromTop
    left (px 0)
