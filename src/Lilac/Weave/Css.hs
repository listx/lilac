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
    center,
    code,
    collapse,
    color,
    column,
    display,
    div,
    em,
    figcaption,
    flex,
    flexDirection,
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
    indent,
    inlineBlock,
    italic,
    li,
    listStyleType,
    main_,
    margin,
    marginBottom,
    marginLeft,
    marginRight,
    marginTop,
    minHeight,
    monospace,
    nav,
    none,
    normal,
    ol,
    overflowX,
    overflowY,
    p,
    padding,
    paddingLeft,
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
    sup,
    table,
    tbody,
    td,
    textAlign,
    textDecoration,
    textIndent,
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
    width,
    (#),
    (&),
    (-:),
    (?),
    (|>),
  )
import Data.Bits
  ( shiftR,
    (.&.),
  )
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Relude
  ( Bool (True),
    Int,
    Maybe (Just, Nothing),
    String,
    Text,
    Type,
    Void,
    concatMap,
    forM_,
    fromIntegral,
    otherwise,
    pure,
    when,
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
    [ T.pack $ Sky.styleToCss kateLilac,
      TL.toStrict $ renderWith pretty [] defaultCss
    ]

kateLilac :: Sky.Style
kateLilac =
  Sky.kate
    { Sky.tokenStyles =
        insert
          Sky.ExtensionTok
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
    noMargin
    ev padding $ px 0
    backgroundColor colorBg
    color colorText
    fontSerif
  body ? do
    minHeight (vh 100)
    display grid
    gridTemplateColumns [px 300, px 800, fr 1]
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
    borderBottom (px 1) solid colorCodeBorder
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
    fontSans
    ol ? do
      marginTop $ px 0
      paddingLeft $ em 1.5
      textIndent (indent . em $ -1.5)
      "counter-reset" -: "toc-section"
      listStyleType none
    li ? do
      "::before" & do
        "counter-increment" -: "toc-section"
        "content" -: "counters(toc-section, \".\") \" \""
        fontWeight bold
    borderRight (px 1) solid colorCodeBorder

  main_ ? do
    "grid-area" -: "main"
    marginLeft (px 20)
    "height"
      -: """
         calc(100vh - calc(4rem + 1px))
         """
    overflowY auto
    padding (px 0) (px 20) (px 0) (px 20)
    paddingLeft (px 24)
    p # ".main-bottom-spacer" ? do
      yx margin (px 40) 0

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
    div # ".tangled-blocks" ? do
      ol ? do
        paddingLeft $ rem 1
        marginTop $ px 0
    div # ".backlinks" ? do
      ul ? do
        paddingLeft $ rem 1
        marginTop $ px 0

  footer ? do
    marginLeft (px 20)
    marginRight (px 20)
    marginTop auto
    div # ".lilac-stamp" ? do
      textAlign $ alignSide sideRight
      fontSans
      fontSize $ rem 0.8
      marginBottom (px 10)
  div ? do
    noMargin
  p # ".widget-title" ? do
    fontWeight bold
    marginBottom $ px 0

  h1 ? do
    fontSize (pt 36)
    marginTop (px 0)
    marginBottom (pt $ -16)
  hr ? do
    border (px 0) solid colorCodeBorder
    borderBottom (px 2) solid colorCodeBorder
    ".title-underline" & do
      marginBottom (rem 2)
  h2 ? do
    fontSize (pt 26)
  h3 ? do
    fontSize (pt 22)
  h4 ? do
    fontSize (pt 18)
  h5 ? do
    fontSize (pt 16)
  h6 ? do
    fontSize (pt 14)
  forM_ (zip [(1 :: Int) ..] [h1, h2, h3, h4, h5, h6]) $ \(n, hN) -> do
    hN ? do
      fontSans
      marginTop (px 0)
      when (n > 1) $ marginBottom (px 0)
      a ? do
        color $ rgbHex 0
        ":visited" & do
          color colorText

  ".bold" & do
    fontWeight bold
  ".italic" & do
    fontStyle italic
  ".underline" & do
    textDecoration underline
  ".verbatim" & do
    fontMono
    fontSize (em 0.9)
  ".code" & do
    fontMono
    fontSize (em 0.9)
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
      roundedRectangleLink
    ".external-link" & do
      color colorNowebLinkFg
      ":visited" & do
        color colorNowebLinkVisitedFg
      "::after" & do
        let rawSvg :: String
            rawSvg =
              """
              <svg xmlns="http://www.w3.org/2000/svg" class="external-link" viewBox="0 0 60 60">
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
        width (em 1)
        height (em 1)
        margin 0 (em (-0.2)) 0 0

  pre ? do
    noMargin
  code ? do
    noMargin
    a ? do
      roundedRectangleLink
  ".sourceCode" & do
    noMargin
  ".fully-rounded" & do
    ev borderRadius (px 5)
  ".code-block-container" & do
    yx margin (px 16) 0
    position relative
    fontSize (rem 0.8)
    a ? do
      fontSize (rem 0.8)
      display inlineBlock
      yx margin (px 2) 0
  ".code-block-meta" & do
    yx padding 0 (px 8)
    border (px 1) solid colorCodeBorder
    borderBottomWidth 0
    borderTopLeftRadius (px 5) (px 5)
    borderTopRightRadius (px 5) (px 5)
    fontSans
  ".code-block-body" & do
    overflowX auto
    margin 0 0 (em 1) 0
    fontMono
    backgroundColor colorCodeBg
    color colorCodeFg
    borderBottomLeftRadius (px 5) (px 5)
    borderBottomRightRadius (px 5) (px 5)
    border (px 1) solid colorCodeBorder
    yx padding (px 2) (px 8)
  (".code-block-container" # hover) |> ".side-popup" ? do
    display block
    a # hover ? do
      textDecoration none
  ".side-popup" & do
    marginLeft (px $ -50)
    padding (px 2) (px 5) 0 0
    width (px 50)
    textAlign $ alignSide sideRight
    boxSizing borderBox
    top (px 0)
    bottom (px 0)
    position absolute
    display none

  ".figure-image" & do
    display block
    yx margin (px 0) auto
  figcaption ? do
    yx margin (px 10) auto
    textAlign center
    fontStyle italic
    width (pct 80)

  table ? do
    fontSans
    borderCollapse collapse
    border (px 1) solid colorCodeBorder
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
          borderTop (px 1) solid colorCodeBorder
          ev padding (px 0)
  ".table-vertical-line-colgroup" & do
    borderLeft (px 3) solid colorCodeBorder
  ".table-vertical-line-col" & do
    borderLeft (px 1) solid colorCodeBorder

  ".bibliography" & do
    marginTop (rem 1)
  ".bib-ref" & do
    marginTop (rem 0.5)
  ".csl-left-margin" & do
    float floatLeft
  ".csl-right-inline" & do
    margin 0 0 0 (em 2)

  sup ? do
    ".footnote-link" & do
      a ? do
        borderStyle none
        ev borderRadius (px 0)
        yx padding 0 (px 0)
        yx margin (px 0) 0
        fontStyle normal
        ".internal-link" & do
          backgroundColor $ rgbHex 0xffffff
          ":hover" & do
            borderStyle none
            textDecoration underline
          ":visited" & do
            ":hover" & do
              borderStyle none
              textDecoration underline

  ".doc-meta-author" & do
    fontStyle italic
    ev margin (px 2)
  ".doc-meta-date" & do
    fontStyle italic
    ev margin (px 2)

roundedRectangleLink :: Css
roundedRectangleLink = do
  fontSize (em 0.9)
  color colorNowebLinkFg
  backgroundColor colorNowebLinkBg
  ev borderRadius (px 5)
  yx padding 0 (px 3)
  border (px 1) solid colorCodeBorder
  ":hover" & do
    border (px 1) solid colorNowebLinkFg
    textDecoration none
  ":visited" & do
    ":hover" & do
      border (px 1) solid colorNowebLinkVisitedFg
      textDecoration none

noMargin :: Css
noMargin = ev margin $ px 0

yx :: (Size sz -> Size sz -> Size sz -> Size sz -> Css) -> Size sz -> Size sz -> Css
yx f x y = f x y x y

ev :: (Size sz -> Size sz -> Size sz -> Size sz -> Css) -> Size sz -> Css
ev f x = f x x x x

rgbHex :: Int -> Color
rgbHex rgb'
  | rgb' > 0xffffff = rgbHex 0xffffff
  | rgb' < 0 = rgbHex 0
  | otherwise = rgb rr gg bb
  where
    rr = fromIntegral $ shiftR rgb' 16 .&. 0xFF
    gg = fromIntegral $ shiftR rgb' 8 .&. 0xFF
    bb = fromIntegral $ rgb' .&. 0xFF

colorFrom :: Text -> Color
colorFrom input = case MP.parseMaybe isHexColorParser input of
  Nothing -> rgbHex 0xff0000
  Just color' -> color'

isHexColorParser :: Parser Color
isHexColorParser = do
  _ <- char '#'
  n <- hexadecimal
  pure $ rgbHex n

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

type Parser :: Type -> Type
type Parser = MP.Parsec Void Text

fontSerif :: Css
fontSerif = do
  fontFamily ["Source Serif 4"] [serif]

fontSans :: Css
fontSans = do
  fontFamily ["Source Sans 3"] [sansSerif]

fontMono :: Css
fontMono = do
  fontFamily ["Source Code Pro"] [monospace]
