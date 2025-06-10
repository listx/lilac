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
    forM_,
    fromIntegral,
    otherwise,
    ($),
    (<),
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
        textDecoration none
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
      textDecoration none
      ":hover" & do
        color $ rgbHex hexNowebLinkHoverFg
        border (px 1) solid $ rgbHex hexNowebLinkHoverFg
        backgroundColor $ rgbHex hexNowebLinkHoverBg
  p ? do
    span ? do
      ".code" & do
        fontSize (pt 10)
  a ? do
    color $ rgbHex hexNowebLinkFg
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
