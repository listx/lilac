module Lilac.Parse
  ( defaultOrgDoc,
    defaultOrgDocWithMeta,
    defaultOrgParserState,
    orgDocParser,
    parseOrgDoc,
    styleMaskFrom,
    OrgDoc (..),
    Cell (..),
    Heading (..),
    Link (..),
    LinkType (..),
    MathFragment (..),
    PToken (..),
    Style (..),
    StyledText (..),
    StyleMask (..),
    Block (..),
    BToken (..),
  )
where

import Data.Bits (complement, setBit, testBit, (.&.), (.^.), (.|.))
import Data.Bits.Bitwise (toListLE)
import Data.Char (isAsciiLower, isSpace)
import Data.Graph qualified as G
import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Relude
import Relude.Extra.Map qualified as EM
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude qualified (show)

type OrgDoc :: Type
data OrgDoc = OrgDoc
  { cells :: [Cell],
    code :: [G.Graph],
    codeBlocks :: IM.IntMap Block,
    meta :: M.Map T.Text T.Text
  }
  deriving stock (Eq, Show)

type Cell :: Type
data Cell
  = CHeading Heading
  | CParagraph Prose
  | CBlock Block
  | CListItem ListItem
  | CMathFragment MathFragment
  | CFigure Figure
  | CTable Table
  deriving stock (Eq, Show)

type Heading :: Type
data Heading = Heading
  { level :: Int,
    body :: Prose,
    meta :: M.Map T.Text T.Text
  }
  deriving stock (Eq, Show)

type Prose :: Type
type Prose = [PToken]

type PToken :: Type
data PToken
  = PTokenStyledText StyledText
  | PTokenLink Link
  | PTokenMathFragment MathFragment
  deriving stock (Eq, Show)

type StyledText :: Type
data StyledText = StyledText
  { body :: T.Text,
    style :: StyleMask
  }
  deriving stock (Eq, Show)

type Style :: Type
data Style = Bold | Italic | Underline | Verbatim | Code | Strikethrough
  deriving stock (Enum, Eq, Ord, Show)

type Link :: Type
data Link = Link
  { typ :: LinkType,
    target :: T.Text,
    name :: [StyledText]
  }
  deriving stock (Eq, Show)

type LinkType :: Type
data LinkType
  = LinkNoweb
  | LinkHeading
  | LinkIdProp
  | LinkNameKeyword
  | LinkFootnote
  | LinkExternal
  deriving stock (Eq, Show)

type Block :: Type
data Block = Block
  { meta :: M.Map T.Text T.Text,
    body :: [BToken]
  }
  deriving stock (Eq, Show)

type BToken :: Type
data BToken
  = BTokenString T.Text
  | BTokenLink Link
  deriving stock (Eq, Show)

type ListItem :: Type
data ListItem = ListItem
  { level :: Int,
    ordered :: Bool,
    body :: Prose
  }
  deriving stock (Eq, Show)

type MathFragment :: Type
data MathFragment = MathFragment
  { meta :: M.Map T.Text T.Text,
    body :: T.Text
  }
  deriving stock (Eq, Show)

type Figure :: Type
data Figure = Figure
  { meta :: M.Map T.Text T.Text,
    link :: Link
  }
  deriving stock (Eq, Show)

type Table :: Type
data Table = Table
  { meta :: M.Map T.Text T.Text,
    grid :: M.Map (Int, Int) Prose,
    gridSize :: (Int, Int)
  }
  deriving stock (Eq, Show)

parseOrgDoc :: T.Text -> Either T.Text OrgDoc
parseOrgDoc input = case MP.runParser
  (runStateT orgDocParser defaultOrgParserState)
  ""
  input of
  Left err -> Left . T.pack $ MP.errorBundlePretty err
  Right (orgDoc, _orgParserState) -> Right orgDoc

type OrgParser :: Type -> Type
type OrgParser = StateT OrgParserState (MP.Parsec Void T.Text)

type OrgParserState :: Type
data OrgParserState = OrgParserState
  { styleMask :: StyleMask
  }
  deriving stock (Eq, Show)

type StyleMask :: Type
newtype StyleMask = StyleMask Word8
  deriving stock (Eq)

instance Show StyleMask where
  show (StyleMask a) = "styleMaskFrom " <> x
    where
      x =
        Prelude.show
          . catMaybes
          . map
            ( \(i, b) ->
                if b
                  then (Just $ (toEnum i :: Style))
                  else Nothing
            )
          . zip [0 ..]
          $ toListLE a

styleMaskFrom :: [Style] -> StyleMask
styleMaskFrom ss = StyleMask $ foldl' f (0 :: Word8) ss
  where
    f :: Word8 -> Style -> Word8
    f acc s = setBit acc (fromEnum s)

maskToStyles :: StyleMask -> [Style]
maskToStyles (StyleMask mask) = foldl' f [] $ zip [0 ..] $ toListLE mask
  where
    f :: [Style] -> (Int, Bool) -> [Style]
    f acc (idx, b) =
      if b
        then (toEnum idx) : acc
        else acc

toStyleMask :: (MonadFail m) => [Char] -> m StyleMask
toStyleMask cs = pure =<< stylesToStyleMask =<< charToStyles cs

charToStyles :: (MonadFail m) => [Char] -> m [Style]
charToStyles chars = foldlM f [] chars
  where
    f :: (MonadFail m) => [Style] -> Char -> m [Style]
    f acc c = case EM.lookup c styleMap of
      Just style -> pure (style : acc)
      Nothing -> fail $ "programmer error: unrecognized style marker " <> (show c)

stylesToStyleMask :: (MonadFail m) => [Style] -> m StyleMask
stylesToStyleMask ss = pure . StyleMask =<< foldlM f (0 :: Word8) ss
  where
    f :: (MonadFail m) => Word8 -> Style -> m Word8
    f acc s
      | testBit acc sInt = fail $ "stylesToStyleMask: redundant style marker: " <> (show s)
      | otherwise = pure $ setBit acc sInt
      where
        sInt = fromEnum s

styleMaskIntersect :: StyleMask -> StyleMask -> StyleMask
styleMaskIntersect (StyleMask a) (StyleMask b) = StyleMask $ a .&. b

styleMaskAdd :: StyleMask -> StyleMask -> StyleMask
styleMaskAdd (StyleMask a) (StyleMask b) = StyleMask $ a .|. b

styleMaskSubtract :: StyleMask -> StyleMask -> StyleMask
styleMaskSubtract (StyleMask a) (StyleMask b) = StyleMask $ a .^. b

invalidDeactivations :: StyleMask -> StyleMask -> StyleMask
invalidDeactivations (StyleMask a) (StyleMask b) = StyleMask $ (complement a) .&. b

isVerbatim :: StyleMask -> Bool
isVerbatim (StyleMask a) = testBit a (fromEnum Verbatim)

isCode :: StyleMask -> Bool
isCode (StyleMask a) = testBit a (fromEnum Code)

defaultOrgParserState :: OrgParserState
defaultOrgParserState =
  OrgParserState
    { styleMask = styleMaskFrom []
    }

spaceConsumer :: OrgParser ()
spaceConsumer =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    blockCommentParser
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = space1
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = L.skipLineComment "# "
    blockCommentParser :: OrgParser ()
    blockCommentParser = L.skipBlockComment "#+begin_comment\n" "#+end_comment\n"

symbol :: T.Text -> OrgParser T.Text
symbol = L.symbol spaceConsumer

lexeme :: OrgParser a -> OrgParser a
lexeme = L.lexeme spaceConsumer

spaceConsumerNoNewline :: OrgParser ()
spaceConsumerNoNewline =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    empty
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = void $ some (char ' ' <|> char '\t')
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = (L.skipLineComment "# ")

symbolNoNewline :: T.Text -> OrgParser T.Text
symbolNoNewline = L.symbol spaceConsumerNoNewline

lexemeNoNewline :: OrgParser a -> OrgParser a
lexemeNoNewline = L.lexeme spaceConsumerNoNewline

spaceConsumerNoDoubleNewline :: OrgParser ()
spaceConsumerNoDoubleNewline =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    empty
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = void $ some f
    f :: OrgParser ()
    f = do
      MP.notFollowedBy (string "\n\n")
      void $ char ' ' <|> char '\t' <|> char '\n'
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = (L.skipLineComment "# ")

symbolNoDoubleNewline :: T.Text -> OrgParser T.Text
symbolNoDoubleNewline = L.symbol spaceConsumerNoDoubleNewline

lexemeNoDoubleNewline :: OrgParser a -> OrgParser a
lexemeNoDoubleNewline = L.lexeme spaceConsumerNoDoubleNewline

orgDocParser :: OrgParser OrgDoc
orgDocParser = do
  _ <- optional spaceConsumer
  meta <- docMetaParser
  cells <- many cellParser
  MP.eof
  pure
    $ OrgDoc
      { cells = cells,
        code = [],
        codeBlocks = IM.fromList [],
        meta = meta
      }

defaultOrgDoc :: OrgDoc
defaultOrgDoc =
  OrgDoc
    { cells = [],
      code = [],
      codeBlocks = IM.fromList [],
      meta = M.fromList []
    }

defaultOrgDocWithMeta :: [(T.Text, T.Text)] -> OrgDoc
defaultOrgDocWithMeta metas =
  OrgDoc
    { cells = [],
      code = [],
      codeBlocks = IM.fromList [],
      meta = M.fromList metas
    }

docMetaEntryParser :: OrgParser (T.Text, T.Text)
docMetaEntryParser = do
  avoidParsingBlockMetadata
  _ <- string "#+"
  key <- MP.takeWhile1P (Just "(doc meta) keyword letter") isKeywordLetter
  _ <- symbol ":"
  value <- MP.takeWhile1P (Just "value letter") (\c -> c /= '\n')
  pure (key, value)
  where
    isKeywordLetter c = or $ map ($ c) [isAsciiLower, (== '_')]
    avoidParsingBlockMetadata :: OrgParser ()
    avoidParsingBlockMetadata =
      MP.notFollowedBy
        $ MP.choice
          [ string "#+begin_",
            string "#+name: ",
            string "#+header: "
          ]

docMetaParser :: OrgParser (M.Map T.Text T.Text)
docMetaParser = do
  metaKVs <- many (lexeme docMetaEntryParser)
  pure $ M.fromList metaKVs

cellParser :: OrgParser Cell
cellParser =
  lexeme
    $ MP.choice
      [ headingParser,
        blockParser,
        paragraphParser
      ]

headingParser :: OrgParser Cell
headingParser = do
  stars <- MP.takeWhile1P (Just "heading star") (\c -> c == '*')
  _ <- char ' '
  prose <- MP.some $ pTokenParser spaceConsumer'
  _ <- char '\n'
  failIfLeftoverActiveStyles
  drawers <- drawersParser
  pure
    $ CHeading
      Heading
        { level = T.length stars,
          body = compressProse prose,
          meta = drawers
        }
  where
    spaceConsumer' :: OrgParser T.Text
    spaceConsumer' = MP.takeWhile1P (Just "non-newline whitespace") isNonNewlineWhitespace
    isNonNewlineWhitespace c = isSpace c && c /= '\n'

pTokenParser :: OrgParser T.Text -> OrgParser PToken
pTokenParser spaceConsumer' = do
  MP.choice
    [ PTokenLink <$> linkParser,
      PTokenMathFragment <$> mathFragmentInlineParser,
      PTokenStyledText <$> styledTextParser,
      PTokenStyledText <$> trailingWhitespaceParser spaceConsumer'
    ]

trailingWhitespaceParser :: OrgParser T.Text -> OrgParser StyledText
trailingWhitespaceParser spaceConsumer' = do
  orgParserState <- get

  let mask = orgParserState.styleMask

  _ <- spaceConsumer'

  pure
    StyledText
      { body = " ",
        style = mask
      }

styleMap :: M.Map Char Style
styleMap =
  M.fromList
    [ ('*', Bold),
      ('/', Italic),
      ('_', Underline),
      ('=', Verbatim),
      ('~', Code),
      ('+', Strikethrough)
    ]

failIfLeftoverActiveStyles :: OrgParser ()
failIfLeftoverActiveStyles = do
  orgParserState <- get
  let (StyleMask mask) = orgParserState.styleMask
  when (mask /= 0) $ do
    fail
      $ "leftover active styles: "
      ++ (show $ maskToStyles $ StyleMask mask)

styledTextParser :: OrgParser StyledText
styledTextParser = do
  orgParserState <- get

  let mask = orgParserState.styleMask
  startingMask <-
    toStyleMask
      =<< T.unpack
      <$> MP.takeWhileP
        (Just "starting style marker")
        isStyleMarker

  let (StyleMask overlappingBits) = styleMaskIntersect mask startingMask
  when (overlappingBits /= 0)
    $ fail
    $ "redundant style activations: "
    <> (show $ maskToStyles $ StyleMask overlappingBits)

  let activeMask = styleMaskAdd mask startingMask

  text <- textParser activeMask

  endingMask <-
    toStyleMask
      =<< T.unpack
      <$> MP.takeWhileP
        (Just "ending style marker")
        isStyleMarker

  let (StyleMask overzealousBits) = invalidDeactivations activeMask endingMask
  when (overzealousBits /= 0)
    $ fail
    $ "overzealous style deactivations: "
    <> (show $ maskToStyles $ StyleMask overzealousBits)

  put orgParserState {styleMask = styleMaskSubtract activeMask endingMask}

  pure
    StyledText
      { body = text,
        style = activeMask
      }
  where
    isStyleMarker c = M.member c styleMap
    isProseWord c = M.notMember c styleMap && (not $ isSpace c)
    isNotVerbatimMarker c = c /= '='
    isNotCodeMarker c = c /= '~'
    textParser :: StyleMask -> OrgParser T.Text
    textParser styleMask
      | isVerbatim styleMask = MP.takeWhile1P (Just "verbatim text") isNotVerbatimMarker
      | isCode styleMask = MP.takeWhile1P (Just "code") isNotCodeMarker
      | otherwise = MP.takeWhile1P (Just "prose word") isProseWord

compressProse :: [PToken] -> [PToken]
compressProse = reverse . dropTrailingWhitespace . foldl' f []
  where
    f :: [PToken] -> PToken -> [PToken]
    f [] token = token : []
    f
      ((t1@(PTokenStyledText (StyledText body1 style1))) : rest)
      t2@(PTokenStyledText (StyledText body2 style2))
        | elem Code styles = t2 : t1 : rest
        | elem Verbatim styles = t2 : t1 : rest
        | style1 /= style2 = t2 : t1 : rest
        | otherwise =
            (PTokenStyledText $ StyledText (T.concat [body1, body2]) style1) : rest
        where
          styles = concatMap maskToStyles [style1, style2]
    f acc token = token : acc
    dropTrailingWhitespace [] = []
    dropTrailingWhitespace (((PTokenStyledText (StyledText " " _))) : rest) = rest
    dropTrailingWhitespace tokens = tokens

linkParser :: OrgParser Link
linkParser = do
  MP.choice [linkExternalParser]

linkExternalParser :: OrgParser Link
linkExternalParser = do
  _ <- string "["
  target <-
    MP.between
      (string "[")
      (string "]")
      ( MP.label "external link target"
          $ MP.some
          $ MP.anySingleBut ']'
      )
  name <-
    MP.optional
      $ MP.between
        (string "[")
        (string "]")
        ( MP.label "external link name"
            $ MP.some
            $ MP.anySingleBut ']'
        )
  _ <- string "]"
  pure
    Link
      { typ = LinkExternal,
        target = T.pack target,
        name =
          [ StyledText
              { body = T.pack (fromMaybe target name),
                style = styleMaskFrom []
              }
          ]
      }

mathFragmentInlineParser :: OrgParser MathFragment
mathFragmentInlineParser = do
  _ <- string "\\("
  latex <- MP.someTill (MP.label "LaTeX math input" MP.anySingle) (string "\\)")
  pure
    MathFragment
      { meta = M.fromList [],
        body = T.pack latex
      }

drawersParser :: OrgParser (Map T.Text T.Text)
drawersParser = do
  kvs <- many (lexeme drawerParser)
  pure $ M.fromList kvs

drawerParser :: OrgParser (T.Text, T.Text)
drawerParser = do
  _ <- char ':'
  drawerName <- MP.takeWhile1P (Just "drawer name") isDrawerLetter
  _ <- char ':'
  _ <- char '\n'
  drawerContents <-
    MP.manyTill
      (MP.label "drawer contents" MP.anySingle)
      (string ":END:\n")
  pure (drawerName, T.pack drawerContents)
  where
    isDrawerLetter c = and $ map ($ c) [(/= '\n'), (/= ':'), (/= ' ')]

paragraphParser :: OrgParser Cell
paragraphParser = do
  prose <- MP.some $ pTokenParser spaceConsumer'
  failIfLeftoverActiveStyles
  pure . CParagraph $ compressProse prose
  where
    spaceConsumer' :: OrgParser T.Text
    spaceConsumer' = do
      MP.notFollowedBy (string "\n\n")
      MP.takeWhile1P (Just "whitespace (newlines allowed)") isSpace

blockMetaEntryParser :: OrgParser (T.Text, T.Text)
blockMetaEntryParser = do
  _ <- string "#+header: :"
  key <- MP.takeWhile1P (Just "(block meta) keyword letter") isKeywordLetter
  _ <- char ' '
  value <- MP.takeWhile1P (Just "value letter") (\c -> c /= '\n')
  _ <- char '\n'
  pure (key, value)
  where
    isKeywordLetter c = or $ map ($ c) [isAsciiLower, (== '-')]

metaNameParser :: OrgParser (T.Text, T.Text)
metaNameParser = do
  _ <- string "#+name: "
  name <- T.pack <$> MP.someTill (MP.label "name of block" MP.anySingle) (char '\n')
  pure ("name", name)

blockParser :: OrgParser Cell
blockParser = do
  blockName <- MP.optional metaNameParser
  metaKVs <- many (lexeme blockMetaEntryParser)
  _ <- string "#+begin_"
  blockType <- MP.takeWhile1P (Just "block type") isAsciiLower
  srcLang <- MP.optional $ do
    _ <- MP.skipSome $ char ' '
    src <- MP.takeWhile1P (Just "src language") isAsciiLower
    pure ("__src_language", src)
  when (blockType == "src" && srcLang == Nothing)
    $ fail "block type src requires a language specifier"
  _ <- char '\n'
  let nowebOff =
        (== Just "off")
          . EM.lookup "noweb"
          $ M.fromList metaKVs
  bTokens <-
    MP.choice
      [ zeroBTokensParser blockType,
        someBTokensParser blockType (not nowebOff)
      ]
  pure
    . CBlock
    $ Block
      { meta =
          M.fromList
            $ (("__block_type", blockType) : metaKVs)
            <> concatMap maybeToList [blockName, srcLang],
        body = compressBTokens bTokens
      }
  where
    zeroBTokensParser :: T.Text -> OrgParser [BToken]
    zeroBTokensParser blockType = do
      _ <- string $ T.concat ["#+end_", blockType, "\n"]
      pure []
    someBTokensParser blockType nowebOn =
      MP.someTill
        (bTokenParser (blockType == "src") nowebOn)
        (string $ T.concat ["\n#+end_", blockType, "\n"])

bTokenParser :: Bool -> Bool -> OrgParser BToken
bTokenParser isSrcBlock nowebOn = MP.choice parsers
  where
    baseParsers :: [OrgParser BToken]
    baseParsers =
      [ BTokenString <$> (MP.takeWhile1P (Just "block token") (not . isSpace)),
        BTokenString <$> (MP.takeWhile1P (Just "spaces inside block") isSpace)
      ]
    parsers =
      if isSrcBlock && nowebOn
        then ((BTokenLink <$> MP.try nowebLinkParser) : baseParsers)
        else baseParsers

nowebLinkParser :: OrgParser Link
nowebLinkParser = do
  linkName <-
    MP.between
      (string "<<")
      (string ">>")
      ( MP.takeWhile1P
          (Just "Noweb reference name")
          isNowebReferenceChar
      )
  pure
    $ Link
      { typ = LinkNoweb,
        target = linkName,
        name =
          [ StyledText
              { body = linkName,
                style = styleMaskFrom []
              }
          ]
      }
  where
    isNowebReferenceChar c = c /= '\n' && c /= '>'

compressBTokens :: [BToken] -> [BToken]
compressBTokens = reverse . foldl' f []
  where
    f :: [BToken] -> BToken -> [BToken]
    f [] token = token : []
    f ((BTokenString text1) : rest) (BTokenString text2) =
      (BTokenString $ T.append text1 text2) : rest
    f acc token = token : acc
