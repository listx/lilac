module Lilac.Weave.Css (genCss) where

import Clay
  ( Color,
    Css,
    Size,
    auto,
    backgroundColor,
    body,
    bold,
    border,
    borderRadius,
    color,
    em,
    fontFamily,
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
    margin,
    monospace,
    overflowY,
    padding,
    pretty,
    px,
    renderWith,
    rgb,
    sansSerif,
    scroll,
    serif,
    solid,
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

genCss :: T.Text
genCss = TL.toStrict $ renderWith pretty [] defaultCss

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
      width $ px 900
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
    hexCodeBg = 0xf7f7f7 :: Int
    hexCodeBorder = 0xcccccc :: Int
    fontSerif = fontFamily ["Source Serif 4"] [serif]
    fontSans = fontFamily ["Source Sans 3"] [sansSerif]
    fontMono = fontFamily ["Source Code Pro"] [monospace]
    codeStyle = do
      fontMono
      backgroundColor $ rgbHex hexCodeBg
      border (px 1) solid $ rgbHex hexCodeBorder
      ev borderRadius (px 4)
      vh padding 0 (em 0.2)

noMargin :: Css
noMargin = ev margin $ px 0

vh :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
vh f x y = f x y x y

ev :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
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
