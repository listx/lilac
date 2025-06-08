module Lilac.Weave.Css (genCss) where

import Clay
  ( Color,
    Css,
    Size,
    auto,
    backgroundColor,
    block,
    body,
    bold,
    border,
    borderBottomLeftRadius,
    borderBottomRightRadius,
    borderRadius,
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
    italic,
    lineHeight,
    margin,
    minWidth,
    monospace,
    overflowY,
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
    textDecoration,
    underline,
    whiteSpace,
    width,
    (&),
    (-:),
    (?),
  )
import Clay.Text qualified as CT
import Data.Bits
  ( shiftR,
    (.&.),
  )
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Relude
  ( Int,
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
    [ T.pack $ Sky.styleToCss Sky.kate,
      TL.toStrict $ renderWith pretty [] defaultCss
    ]

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
    h1 ? do
      fontSans
    h2 ? do
      fontSans
    h3 ? do
      fontSans
    h4 ? do
      fontSans
    h5 ? do
      fontSans
    h6 ? do
      fontSans
  pre ? do
    "all" -: "unset"
    display block
    whiteSpace CT.pre
    noMargin
  div ? do
    noMargin
  code ? do
    noMargin
  ".sourceCode" & do
    noMargin
  ".code-block-body" & do
    noMargin
    fontMono
    fontSize (pt 10)
    lineHeight (pt 14)
    backgroundColor $ rgbHex hexCodeBg
    color $ rgbHex hexCodeFg
    borderBottomLeftRadius (px 5) (px 5)
    borderBottomRightRadius (px 5) (px 5)
    border (px 1) solid $ rgbHex hexCodeBorder
    ev padding (px 5)
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
    codeStyle
  where
    hexBg = 0xffffff :: Int
    hexText = 0x000000 :: Int
    hexCodeFg = 0x1f1c1b :: Int
    hexCodeBg = 0xf7f7f7 :: Int
    hexCodeBorder = 0xcccccc :: Int
    fontSerif = do
      fontFamily ["Source Serif 4"] [serif]
    fontSans = do
      fontFamily ["Source Sans 3"] [sansSerif]
    fontMono = do
      fontFamily ["Source Code Pro"] [monospace]
    codeStyle = do
      fontMono
      fontSize (pt 10)
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
