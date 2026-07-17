module Lilac.Weave.Css (genCss) where

import Clay
  ( Color,
    Css,
    Size,
    a,
    absolute,
    alignSide,
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
    fixed,
    float,
    floatLeft,
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
    lineHeight,
    listStyleType,
    main_,
    margin,
    marginBottom,
    marginLeft,
    marginRight,
    marginTop,
    maxHeight,
    maxWidth,
    minHeight,
    monospace,
    nav,
    none,
    normal,
    ol,
    opacity,
    overflow,
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
    px,
    relative,
    rem,
    renderWith,
    rgb,
    sansSerif,
    serif,
    sideCenter,
    sideLeft,
    sideRight,
    smallCaps,
    solid,
    span,
    sticky,
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
    unitless,
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
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Relude
  ( Bool (True),
    ByteString,
    Int,
    Maybe (Just),
    String,
    Text,
    Type,
    Void,
    concatMap,
    decodeUtf8,
    forM_,
    fromMaybe,
    otherwise,
    pure,
    toString,
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
      TL.toStrict $ renderWith pretty [] defaultCss,
      rawCss
    ]

rawCss :: Text
rawCss =
  "/* rawCss */\n"
    <> decodeUtf8 @Text @ByteString $(embedFile "src/Lilac/Weave/Css/raw.css")

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
    ev margin $ rem 0
    ev padding $ rem 0
    backgroundColor colorBg
    color colorText
    fontSerif
    fontSize $ px 16 --
  body ? do
    minHeight (vh 100)
    display grid
    gridTemplateColumns [rem 16.25, rem 50]
    "grid-template-rows" -: "auto 1fr"
    "grid-template-areas"
      -: "\"navbar navbar\"\n\"tocs   main\""
    yx margin (rem 0) auto
    overflowY auto
  div # ".navbar" ? do
    "display" -: "flex"
    "grid-area" -: "navbar"
    height $ rem 3
    "width" -: "100%"
    top $ rem 0
    position fixed
    "z-index" -: "1000"
    borderBottom (px 1) solid colorCodeBorder --
    backgroundColor colorBg
    button # ".tocs-toggle-button" ? do
      display displayNone

    div # ".breadcrumbs" ? do
      fontSans
      fontSize $ rem 1.6
      margin (rem 0.375) 0 0 (rem 0.625)
    span # ".breadcrumb-separator" ? do
      color colorBreadcrumbSeparator

  nav # "#tocs" ? do
    "grid-area" -: "tocs"
    "height"
      -: "calc(100vh - 3rem)"
    width $ rem 16.25
    fontSans
    position fixed
    "top" -: "3rem"
    borderRight (px 1) solid colorCodeBorder
    overflow hidden
    display grid
    "grid-template-rows" -: "auto auto auto 1fr"
    "grid-template-areas"
      -: "\"gtoc-title\" \"gtoc\" \"toc-title\" \"toc\""
    "z-index" -: "990"
    backgroundColor colorBg

  ".toc-title" ? do
    "grid-area" -: "toc-title"
    margin (rem 0) (rem 0.875) (rem 0.25) (rem 0.875)
  nav # ".toc" ? do
    "grid-area" -: "toc"
    overflowY auto
    div # ".toc-entries" ? do
      paddingLeft $ rem 1.875
      ol ? do
        marginTop $ rem 0
        paddingLeft $ rem 0
        listStyleType none
        li # ".toc-level-1" ? do
          position relative
          before & do
            content $ stringContent "■" --
            marginLeft (rem $ -1.3)
            position absolute
            left (rem 0)
            top (rem $ -0.125)
            fontSize (rem 1.5)
            fontFamily ["Arial"] [sansSerif]
            lineHeight $ unitless 1
        li # ".toc-level-2" ? do
          position relative
          before & do
            content $ stringContent "○" --
            marginLeft (rem $ -1.3)
            position absolute
            left (rem 0)
            top (rem $ -0.125)
            fontSize (rem 1.5)
            fontFamily ["Arial"] [sansSerif]
            lineHeight $ unitless 1
        div ? do
          paddingLeft $ rem 1.875
          paddingRight $ rem 0.625
          marginLeft $ rem (-1.875)
          ".active" & do
            backgroundColor colorTocActive
          ".active-ancestor" & do
            backgroundColor colorTocActiveAncestor
      span # ".toc-counter" ? do
        marginRight $ rem 0.3

  ".gtoc-title" ? do
    "grid-area" -: "gtoc-title"
    yx margin (rem 0.25) (rem 0.875)
  nav # ".gtoc" ? do
    "grid-area" -: "gtoc"
    maxHeight $ vh 50
    paddingLeft $ rem 1
    overflowY auto
    fontSans
    ul ? do
      marginTop $ rem 0
      marginBottom $ rem 0.5
      paddingLeft $ rem 0.875

  main_ ? do
    "grid-area" -: "main"
    marginLeft (rem 1)
    height auto
    overflowY visible
    padding (rem 0) (rem 1.25) (rem 0) (rem 1.25)
    paddingLeft (rem 3.125)
    "position" -: "relative"
    "top" -: "3rem"

  forM_ [".backlinks", ".tangled-blocks", ".named-cells"] $ \metric -> do
    div # metric ? do
      fontSans
      ol ? do
        ev margin $ rem 0
        paddingLeft $ rem 0.9375
  footer ? do
    div # ".lilac-stamp" ? do
      fontSans
      fontStyle italic
      fontSize $ rem 0.9
      marginTop (rem 2.5)
      marginBottom (rem 0.625) --
  p # ".widget-title" --
    ? do
      fontWeight bold
      borderBottom (px 1) solid colorCodeBorder
  div ? do
    --
    ev margin $ rem 0

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
            fontSize (rem 3)
            headingPrefix "●" (rem 4) (rem $ -2.8) (rem $ -0.1875) --
          2 -> do
            fontSize (rem 2.4)
            headingPrefix "■" (rem 3) (rem $ -2.5) (rem $ -0.1875) --
          3 -> do
            fontSize (rem 2)
            headingPrefix "○" (rem 2.35) (rem $ -2.25) (rem $ -0.0625) --
          4 -> do
            fontSize (rem 1.6)
          5 -> do
            fontSize (rem 1.4)
          6 -> do
            fontSize (rem 1.2)
          _ -> pure ()
        position relative
        fontSans
        marginTop (rem 0)
        marginBottom (rem 0)
        a ? do
          color $ colorFrom "#000000"
          ":visited" & do
            color colorText
        border (px 1) solid colorBg
        yx padding 0 (rem 0.3125)
        marginLeft (rem (-0.4375))
        maxWidth fitContent
        ".active" & do
          border (px 1) solid colorActiveBorder
          backgroundColor colorActiveBg
          ev borderRadius (rem 0.3125)

  ".bold" & do
    fontWeight bold
  ".italic" & do
    fontStyle italic
  ".underline" & do
    textDecoration underline
  ".verbatim" & do
    fontMono
    "white-space" -: "pre-wrap"
    "word-break" -: "break-word" --
  ".code" & do
    fontMono
    "white-space" -: "pre-wrap"
    "word-break" -: "break-word"
    backgroundColor colorCodeBg
    border (px 1) solid colorCodeBorder
    ev borderRadius (rem 0.25)
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
    "word-break" -: "break-word"
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
              toString
                $ decodeUtf8
                  @Text
                  @ByteString
                  $(embedFile "src/Lilac/Weave/Css/external-link.svg")
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
      li ? do
        empty & do
          minHeight (rem 2.2)

  ".code-block-container" & do
    display grid
    gridTemplateColumns [rem 3.125, fr 1]
    margin (rem 1) 0 (rem 1) (rem (-3.125))
    position relative
    fontSize (rem 0.9)
    a ? do
      display inlineBlock
      yx margin (rem 0.125) 0
  forM_
    [ ".code-block-meta",
      ".code-block-meta-raw",
      ".code-block-controls-inline",
      ".code-block-body"
    ]
    $ \child -> do
      ".code-block-container" |> child ? do
        "grid-column" -: "2"

  ".code-block-controls" & do
    padding (rem 0.125) (rem 0.3125) 0 0
    width (rem 3.125)
    textAlign $ alignSide sideRight
    boxSizing borderBox
    top (rem 0)
    bottom (rem 0)
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
      marginRight $ rem 0.3125
      "border" -: "0"
      "cursor" -: "pointer"
      ev borderRadius (rem 0.125)
      border (px 1) solid colorCodeBorder
  (".code-block-container" # hover) |> ".code-block-controls" ? do
    visibility visible
    opacity 1

  ".code-block-controls-inline" & do
    yx padding (rem 0.25) (rem 0.5)
    border (px 1) solid colorCodeBorder
    fontSans
    button # ".toggle-code-block-body" ? do
      "::before" & do
        "content" -: "\"Expand contents\"" --
      "border" -: "0"
      "cursor" -: "pointer"
      ev borderRadius (rem 0.125)
      border (px 1) solid colorCodeBorder
    button # ".toggle-code-block-body.action-collapse" ? do
      "::before" & do
        "content" -: "\"Collapse contents\"" --
  ".active" ** ".code-block-controls-inline" ? do
    border (px 1) solid colorActiveBorder

  ".code-block-meta" & do
    yx padding 0 (rem 0.5)
    border (px 1) solid colorCodeBorder
    borderBottomWidth 0
    borderTopLeftRadius (rem 0.3125) (rem 0.3125)
    borderTopRightRadius (rem 0.3125) (rem 0.3125)
    fontSans
    a ? do
      ".parent-block-arrow" & do
        marginLeft (em 0.25)
  ".active" ** ".code-block-meta" ? do
    fontWeight bold
    border (px 1) solid colorActiveBorder
    borderBottomWidth 0

  div # ".code-block-meta-raw" ? do
    yx padding 0 (rem 0.5)
    fontMono
    fontSize $ rem 0.9
    border (px 1) solid colorCodeBorder
    borderBottomWidth 0
    ".hidden" & do
      display displayNone
  ".active" ** ".code-block-meta-raw" ? do
    border (px 1) solid colorActiveBorder
    borderBottomWidth 0

  pre ? do
    ev margin $ rem 0
  code ? do
    ev margin $ rem 0
    a ? do
      roundedRectangleLink
      span ? do
        color colorNowebLinkFg
    "white-space" -: "pre-wrap"
    "word-break" -: "break-word"
  ".sourceCode" & do
    ev margin $ rem 0
  ".code-block-body" & do
    overflowX visible
    fontMono
    fontSize (rem 0.9)
    backgroundColor colorCodeBg
    color colorCodeFg
    borderBottomLeftRadius (rem 0.3125) (rem 0.3125)
    borderBottomRightRadius (rem 0.3125) (rem 0.3125)
    border (px 1) solid colorCodeBorder
    yx padding (rem 0.125) (rem 0.5)
  ".active" ** ".code-block-body" ? do
    border (px 1) solid colorActiveBorder
    backgroundColor colorActiveBg
  --
  ".collapsed" ? do
    display displayNone
  ".code-block-body" ** ".sourceCode" ** ".active" ? do
    border (px 1) solid colorActiveBorder
    backgroundColor colorActiveBg
    "::after" & do
      "content" -: "\" 👈\""

  ".fully-rounded" & do
    ev borderRadius (rem 0.3125)
  ".top-rounded" & do
    borderTopLeftRadius (rem 0.3125) (rem 0.3125)
    borderTopRightRadius (rem 0.3125) (rem 0.3125)
  --
  ".bottom-rounded" & do
    borderBottomLeftRadius (rem 0.3125) (rem 0.3125)
    borderBottomRightRadius (rem 0.3125) (rem 0.3125)
  --
  ".hide-border-bottom" & do
    borderBottom (rem 0) solid colorCodeBorder
  ".active" ** ".hide-border-bottom" ? do
    borderBottom (rem 0) solid colorCodeBorder

  figure ? do
    yx margin (rem 1) 0
  ".figure-image" & do
    display block
    yx margin (rem 0) auto
  figcaption ? do
    yx margin (rem 0.625) auto
    yx padding 0 (rem 0.3125)
    textAlign center
    fontSans
    maxWidth (pct 80)
    display displayTable
    border (px 1) solid colorBg
    ev borderRadius (rem 0.3125)
  ".caption-prefix" & do
    fontWeight bold
  figure # ".active" ? do
    figcaption ? do
      border (px 1) solid colorActiveBorder
      backgroundColor colorActiveBg
      ev borderRadius (rem 0.3125)

  ".table-wrapper" & do
    border (px 2) solid colorCodeBorder
    yx margin (rem 0) auto
    width fitContent
  table ? do
    fontSans
    borderCollapse collapse
    th ? do
      position sticky --
      "top" -: "3rem" --
      backgroundColor colorCodeBg
      yx padding (rem 0.25) (rem 0.5)
    td ? do
      yx padding (rem 0.25) (rem 0.5)
      ".table-cell-numeric" & do
        textAlign $ alignSide sideRight
    thead ? do
      backgroundColor colorCodeBg
    tbody ? do
      tr ? do
        ".table-horizontal-line" & do
          borderTop (px 3) solid colorCodeBorder
          ev padding (rem 0)
          td ? do
            backgroundColor colorCodeBorder
            yx padding (rem 0) (rem 0)
        ".table-body-row" & do
          borderTop (px 2) solid colorCodeBorder
  ".table-vertical-thick-line-colgroup" & do
    borderLeft (px 4) solid colorCodeBorder --
  ".table-vertical-thin-line-col" & do
    --
    borderLeft (px 2) solid colorCodeBorder
  ".table-cell-text-align-left" & do
    textAlign $ alignSide sideLeft
  ".table-cell-text-align-center" & do
    textAlign $ alignSide sideCenter
  ".table-cell-text-align-right" & do
    textAlign $ alignSide sideRight

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
        ev borderRadius (rem 0)
        yx padding 0 (rem 0)
        yx margin (rem 0) 0
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
    ev borderRadius (rem 0.3125)
    marginLeft (rem (-0.4375))
    yx padding 0 (rem 0.3125)
    ".active" & do
      border (px 1) solid colorActiveBorder
      backgroundColor colorActiveBg
      ev borderRadius (rem 0.3125)
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
      fontSize (rem 0.7)
      ":hover" & do
        textDecoration none
  div # ".glossary-term" ? do
    border (px 1) solid colorBg
    yx padding (rem 0) (rem 0.3125)
    ".active" & do
      border (px 1) solid colorActiveBorder
      backgroundColor colorActiveBg
      ev borderRadius (rem 0.3125)
  span # ".glossary-external-source" ? do
    fontSans

  ".doc-meta-author" & do
    fontStyle italic
    ev margin (rem 0.125)
  ".doc-meta-date" & do
    fontStyle italic
    ev margin (rem 0.125)

roundedRectangleLink :: Css
roundedRectangleLink = do
  color colorNowebLinkFg
  backgroundColor colorNowebLinkBg
  ev borderRadius (rem 0.25)
  yx padding (rem 0.0625) (rem 0.1875)
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

headingPrefix :: Text -> Size sz -> Size sz -> Size sz -> Css
headingPrefix prefix fSize spacing fromTop =
  before & do
    content $ stringContent prefix
    marginLeft spacing
    position absolute
    top fromTop
    left (rem 0)
    fontSize fSize
    fontFamily ["Arial"] [sansSerif]
    lineHeight $ unitless 1
