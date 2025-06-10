module Lilac.Weave.Css (genCss) where

import Clay
  ( Color,
    Css,
    Size,
    a,
    auto,
    backgroundColor,
    body,
    bold,
    border,
    borderBottomLeftRadius,
    borderBottomRightRadius,
    borderBottomWidth,
    borderRadius,
    borderTopLeftRadius,
    borderTopRightRadius,
    code,
    color,
    display,
    div,
    em,
    fitContent,
    fontFamily,
    fontSize,
    fontStyle,
    fontWeight,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    height,
    html,
    inlineBlock,
    italic,
    margin,
    minWidth,
    monospace,
    none,
    overflowY,
    p,
    padding,
    pre,
    pretty,
    pt,
    px,
    renderWith,
    rgb,
    sansSerif,
    scroll,
    serif,
    solid,
    span,
    textDecoration,
    underline,
    width,
    (&),
    (-:),
    (?),
  )
import Data.Bits
  ( shiftR,
    (.&.),
  )
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Relude
  ( Bool (True),
    Int,
    Maybe (Just),
    String,
    concatMap,
    forM_,
    fromIntegral,
    otherwise,
    ($),
    (.),
    (<),
    (<>),
    (>),
  )
import Skylighting qualified as Sky

genCss :: T.Text
genCss =
  T.unlines
    [ T.pack $ Sky.styleToCss kateLilac,
      TL.toStrict $ renderWith pretty [] defaultCss
    ]

kateLilac :: Sky.Style
kateLilac =
  Sky.kate
    { Sky.tokenStyles =
        M.insert
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
    backgroundColor $ rgbHex hexBg
    color $ rgbHex hexText
  body ? do
    overflowY scroll
    fontSerif
    width $ px 800
    vh margin (px 0) auto
  forM_ [h1, h2, h3, h4, h5, h6] $ \hN -> do
    hN ? do
      fontSans
      a ? do
        color $ rgbHex 0
  pre ? do
    noMargin
  div ? do
    noMargin
  code ? do
    noMargin
    a ? do
      color $ rgbHex hexNowebLinkFg
      border (px 1) solid $ rgbHex hexNowebLinkFg
      backgroundColor $ rgbHex hexNowebLinkBg
      fontStyle italic
      ev borderRadius (px 5)
      vh padding 0 (px 5)
      vh margin (px 2) 0
      display inlineBlock
      ":hover" & do
        color $ rgbHex hexNowebLinkHoverFg
        border (px 1) solid $ rgbHex hexNowebLinkHoverFg
        backgroundColor $ rgbHex hexNowebLinkHoverBg
        textDecoration none
  p ? do
    span ? do
      ".code" & do
        fontSize (pt 10)
  a ? do
    color $ rgbHex hexNowebLinkFg
    textDecoration none
    ":hover" & do
      textDecoration underline
    ".external-link" & do
      color $ rgbHex hexNowebLinkFg
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
                (T.append "%23" $ T.drop 1 hexNowebLinkFgTxt)
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
  ".sourceCode" & do
    noMargin
  ".code-block-container" & do
    minWidth fitContent
  ".code-block-name" & do
    vh padding 0 (px 8)
    border (px 1) solid $ rgbHex hexCodeBorder
    borderBottomWidth 0
    borderTopLeftRadius (px 5) (px 5)
    borderTopRightRadius (px 5) (px 5)
    fontSans
  ".code-block-body" & do
    margin 0 0 (em 1) 0
    fontMono
    backgroundColor $ rgbHex hexCodeBg
    color $ rgbHex hexCodeFg
    borderBottomLeftRadius (px 5) (px 5)
    borderBottomRightRadius (px 5) (px 5)
    border (px 1) solid $ rgbHex hexCodeBorder
    vh padding (px 2) (px 8)
    minWidth fitContent
  ".bold" & do
    fontWeight bold
  ".italic" & do
    fontStyle italic
  ".underline" & do
    textDecoration underline
  ".verbatim" & do
    fontMono
  ".code" & do
    fontMono
    codeStyle
  ".sans" & do
    fontSans
  ".fully-rounded" & do
    ev borderRadius (px 5)
  where
    hexBg = 0xffffff :: Int
    hexText = 0x000000 :: Int
    hexCodeFg = 0x1f1c1b :: Int
    hexCodeBg = 0xf7f7f7 :: Int
    hexCodeBorder = 0xcccccc :: Int
    hexNowebLinkFg = 0x2161ac :: Int
    hexNowebLinkFgTxt = "#2161ac" :: T.Text
    hexNowebLinkBg = 0xd8f6ff :: Int
    hexNowebLinkHoverFg = 0x7d3c98 :: Int
    hexNowebLinkHoverBg = 0xf8e9ff :: Int
    fontSerif = do
      fontFamily ["Source Serif 4"] [serif]
    fontSans = do
      fontFamily ["Source Sans 3"] [sansSerif]
    fontMono = do
      fontFamily ["Source Code Pro"] [monospace]
    codeStyle = do
      backgroundColor $ rgbHex hexCodeBg
      border (px 1) solid $ rgbHex hexCodeBorder
      ev borderRadius (px 4)
      vh padding 0 (em 0.2)

noMargin :: Css
noMargin = ev margin $ px 0

vh :: (Size sz -> Size sz -> Size sz -> Size sz -> Css) -> Size sz -> Size sz -> Css
vh f x y = f x y x y

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
