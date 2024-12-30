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
    ListMarker (..),
    Level (..),
  )
where

import Data.Bits (complement, setBit, testBit, (.&.), (.^.), (.|.))
import Data.Bits.Bitwise (toListLE)
import Data.Char (isAsciiLower, isDigit, isSpace)
import Data.Graph qualified as G
import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Relude
  ( Bool (False, True),
    Char,
    Either (Left, Right),
    Enum,
    Eq,
    Int,
    Map,
    Maybe (Just, Nothing),
    MonadFail,
    Ord,
    Show,
    StateT,
    String,
    Type,
    Void,
    Word8,
    and,
    catMaybes,
    concatMap,
    drop,
    elem,
    fail,
    filter,
    find,
    fmap,
    foldl',
    foldlM,
    fromEnum,
    fromMaybe,
    get,
    isJust,
    isNothing,
    length,
    many,
    map,
    mapM,
    maybeToList,
    modify,
    not,
    null,
    optional,
    or,
    otherwise,
    pure,
    put,
    readMaybe,
    reverse,
    runStateT,
    show,
    take,
    toEnum,
    void,
    when,
    zip,
    ($),
    (&&),
    (*),
    (+),
    (++),
    (.),
    (/=),
    (<),
    (<$>),
    (<>),
    (<|>),
    (=<<),
    (==),
    (>>),
  )
import Relude.Extra.Map qualified as EM
import Relude.Unsafe (fromJust)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char, string)
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
  | CListItem [ListMarker]
  | CParagraph Prose
  | CBlock Block
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

type Level :: Type
data Level = Level
  { indent :: Int,
    number :: Maybe Int
  }
  deriving stock (Eq, Show)

type ListMarker :: Type
data ListMarker
  = LOrderedStart
  | LOrderedEnd
  | LUnorderedStart
  | LUnorderedEnd
  | LItemStart
  | LItemEnd
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
  { styleMask :: StyleMask,
    listLevels :: [Level],
    listMarkers :: [ListMarker],
    continuationIndent :: Int
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
    { styleMask = styleMaskFrom [],
      listLevels = [],
      listMarkers = [],
      continuationIndent = 0
    }

skipNewlinesAndComments :: OrgParser ()
skipNewlinesAndComments =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    blockCommentParser
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = void $ MP.takeWhile1P (Just "newline") (== '\n')
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = L.skipLineComment "# "
    blockCommentParser :: OrgParser ()
    blockCommentParser = L.skipBlockComment "#+begin_comment\n" "#+end_comment\n"

lexeme :: OrgParser a -> OrgParser a
lexeme = L.lexeme skipNewlinesAndComments

orgDocParser :: OrgParser OrgDoc
orgDocParser = do
  _ <- optional skipNewlinesAndComments
  meta <- docMetaParser
  cells <- many cellParser
  let meaningfulCell :: Cell -> Bool
      meaningfulCell (CListItem []) = False
      meaningfulCell _ = True
  MP.eof
  pure
    $ OrgDoc
      { cells = filter meaningfulCell cells,
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
  _ <- MP.label "(doc meta) separator" $ string ": "
  _ <- many $ char ' '
  value <- MP.takeWhile1P (Just "(doc meta) value letter") (\c -> c /= '\n')
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
      [ MP.try listItemStartParser,
        listItemsEndParser,
        resetContinuationIndent headingParser,
        resetContinuationIndent blockParser,
        resetContinuationIndent paragraphParser,
        detectInvalidIndentation
      ]
  where
    resetContinuationIndent :: OrgParser a -> OrgParser a
    resetContinuationIndent p = do
      a <- p
      modify (\s -> s {continuationIndent = 0})
      pure a

detectInvalidIndentation :: OrgParser Cell
detectInvalidIndentation = do
  _ <- MP.hidden $ MP.takeWhile1P (Just "indentation") (== ' ')
  void
    $ MP.fancyFailure
    . Set.singleton
    . MP.ErrorFail
    $ "invalid indentation"
  pure
    $ CHeading
      Heading
        { level = 1,
          body = [],
          meta = M.fromList []
        }

headingParser :: OrgParser Cell
headingParser = do
  stars <- MP.takeWhile1P (Just "heading star") (\c -> c == '*')
  _ <- char ' '
  prose <- proseParser spaceConsumer
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
    spaceConsumer :: OrgParser T.Text
    spaceConsumer = MP.takeWhile1P (Just "space") (== ' ')

proseParser :: OrgParser T.Text -> OrgParser [PToken]
proseParser spaceConsumer = do
  a <- MP.choice baseParsers
  b <- MP.many $ MP.choice (spacePTokenParser : baseParsers)
  pure $ a : b
  where
    spacePTokenParser = PTokenStyledText <$> whitespaceCompressor spaceConsumer
    baseParsers =
      [ PTokenLink <$> linkParser,
        PTokenMathFragment <$> mathFragmentInlineParser,
        PTokenStyledText <$> styledTextParser
      ]

whitespaceCompressor :: OrgParser T.Text -> OrgParser StyledText
whitespaceCompressor spaceConsumer = do
  orgParserState <- get

  let mask = orgParserState.styleMask

  _ <- spaceConsumer

  ws <-
    MP.lookAhead
      $ MP.choice
        [ MP.eof >> pure "",
          MP.anySingle >> pure " "
        ]

  pure
    StyledText
      { body = ws,
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
compressProse = reverse . dropUnnecessary . foldl' f []
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
    dropUnnecessary [] = []
    dropUnnecessary (((PTokenStyledText (StyledText " " _))) : rest) = rest
    dropUnnecessary (((PTokenStyledText (StyledText "" _))) : rest) = rest
    dropUnnecessary tokens = tokens

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
  orgParserState <- get
  prose <- proseParser $ spaceConsumer orgParserState.continuationIndent
  failIfLeftoverActiveStyles
  pure . CParagraph $ compressProse prose
  where
    spaceConsumer :: Int -> OrgParser T.Text
    spaceConsumer expectedIndentLen =
      MP.choice
        [ newlineAndIndent expectedIndentLen,
          spaces
        ]
    newlineAndIndent :: Int -> OrgParser T.Text
    newlineAndIndent expectedIndentLen = do
      MP.notFollowedBy (string "\n\n")
      _ <- string "\n"
      _ <-
        if (expectedIndentLen == 0)
          then MP.notFollowedBy (char ' ')
          else MP.eof <|> indent expectedIndentLen
      pure " "
    indent :: Int -> OrgParser ()
    indent expectedIndentLen = do
      _ <-
        MP.label "indentation for paragraph continuation"
          $ MP.count expectedIndentLen (char ' ')
      MP.notFollowedBy (char ' ')
      MP.notFollowedBy (char '\n')
      pure ()
    spaces :: OrgParser T.Text
    spaces = MP.takeWhile1P (Just "space") (\c -> c == ' ')

listItemsEndParser :: OrgParser Cell
listItemsEndParser = do
  orgParserState <- get
  when (null orgParserState.listMarkers) $ do
    fail "no pending ListMarker objects to close off"
  when (orgParserState.continuationIndent /= 0) $ do
    fail "not at beginning of line"
  (levelsToDrop, continuationIndent) <- zeroIndent <|> someIndent
  let markersToDrop = levelsToDrop * 2
  markers <-
    mapM getEndingMarker
      $ take markersToDrop orgParserState.listMarkers
  put
    orgParserState
      { listLevels = drop levelsToDrop orgParserState.listLevels,
        listMarkers = drop markersToDrop orgParserState.listMarkers,
        continuationIndent = continuationIndent
      }
  pure $ CListItem markers
  where
    getEndingMarker :: (MonadFail m) => ListMarker -> m ListMarker
    getEndingMarker = \case
      LOrderedStart -> pure LOrderedEnd
      LUnorderedStart -> pure LUnorderedEnd
      LItemStart -> pure LItemEnd
      lm ->
        fail
          $ "programmer error: list marker "
          <> (show lm)
          <> " is not supported"
    zeroIndent :: OrgParser (Int, Int)
    zeroIndent = do
      orgParserState <- get
      _ <- MP.eof <|> MP.notFollowedBy (char ' ')
      pure (length orgParserState.listLevels, 0)
    someIndent :: OrgParser (Int, Int)
    someIndent = do
      orgParserState <- get
      spaces <- MP.takeWhile1P (Just "indentation") (== ' ')
      _ <- MP.notFollowedBy (char ' ')
      let existingLevels = orgParserState.listLevels
          levelsToDropPerIndentationIncrement :: [(Int, Int)]
          levelsToDropPerIndentationIncrement =
            zip [0 ..]
              . map getBodyIndent
              $ existingLevels
          indent = T.length spaces
          matchingIndent =
            find
              (\(_, bodyIndent) -> indent == bodyIndent)
              levelsToDropPerIndentationIncrement
      case matchingIndent of
        Nothing -> fail "indentation does not fit into existing levels"
        Just x -> pure x

listHeadIndentLevelParser :: OrgParser Level
listHeadIndentLevelParser = do
  minimalIndentation
  spaces <- MP.takeWhile1P (Just "indentation") (== ' ')
  listHead <- orderedHeadParser <|> unorderedHeadParser
  let level =
        Level
          { indent = T.length spaces,
            number = listHead
          }
  afterListHead level
  pure level
  where
    orderedHeadParser :: OrgParser (Maybe Int)
    orderedHeadParser = do
      num <- MP.takeWhile1P (Just "ordered list number") isDigit
      _ <- char ')'
      let n = readMaybe (T.unpack num) :: Maybe Int
      when (isNothing n) $ do
        fail $ "could not parse " <> show (T.unpack num) <> " as Int"
      pure n
    unorderedHeadParser :: OrgParser (Maybe Int)
    unorderedHeadParser = do
      _ <- char '-'
      pure Nothing
    afterListHead :: Level -> OrgParser ()
    afterListHead level =
      void
        $ MP.choice
          [ MP.eof,
            void $ string "\n",
            void $ seeingContentAfterListHead level
          ]
    seeingContentAfterListHead :: Level -> OrgParser ()
    seeingContentAfterListHead level = do
      _ <- char ' '
      MP.notFollowedBy (char ' ')
      modify (\s -> s {continuationIndent = getBodyIndent level})
    minimalIndentation :: OrgParser ()
    minimalIndentation =
      void
        . MP.try
        . MP.lookAhead
        . MP.label "minimal indentation"
        $ MP.count 2 (char ' ')

listItemStartParser :: OrgParser Cell
listItemStartParser = do
  orgParserState <- get
  let levels = orgParserState.listLevels
      markers = orgParserState.listMarkers
  level <- listHeadIndentLevelParser
  listMarkers <-
    if
      | nests level.indent levels -> listItemNester level levels markers
      | otherwise -> case fits level levels of
          Right True -> listItemFitter level levels
          Right False -> fail "list item: invalid indentation"
          Left msg -> fail msg
  pure $ CListItem listMarkers
  where
    fits :: Level -> [Level] -> Either String Bool
    fits level levels = case find (\l -> l.indent == level.indent) levels of
      Nothing -> Right False
      Just lvl
        | isJust level.number && isJust lvl.number ->
            let expectedNum = fmap (+ 1) lvl.number
             in if level.number == expectedNum
                  then Right True
                  else
                    Left
                      $ "expected incremented list head "
                      <> (show . fromJust $ expectedNum)
                      <> " but got "
                      <> (show . fromJust $ level.number)
        | isNothing level.number && isNothing lvl.number -> Right True
        | isJust lvl.number -> Left "expected ORDERED list item at this level"
        | otherwise -> Left "expected UNORDERED list item at this level"
    nests :: Int -> [Level] -> Bool
    nests _ [] = True
    nests indent (level : _) = indent == getBodyIndent level

getBodyIndent :: Level -> Int
getBodyIndent level = level.indent + maybeNum + punctuation + space
  where
    punctuation :: Int
    punctuation = 1
    space :: Int
    space = 1
    maybeNum :: Int
    maybeNum = case level.number of
      Just n -> length $ (show n :: String)
      Nothing -> 0

listItemNester :: Level -> [Level] -> [ListMarker] -> OrgParser [ListMarker]
listItemNester level levels markers = do
  when (null levels && level.indent /= 2) $ do
    fail "starting indent must be 2 spaces"
  case level.number of
    Just 0 -> pure ()
    Just 1 -> pure ()
    Just n -> fail $ "starting number must be 0 or 1; got " <> show n
    Nothing -> pure ()
  let newMarker = case level.number of
        Just _ -> LOrderedStart
        Nothing -> LUnorderedStart
  orgParserState <- get
  put
    orgParserState
      { listLevels = level : levels,
        listMarkers = LItemStart : newMarker : markers
      }
  pure $ case level.number of
    Just _ -> [LOrderedStart, LItemStart]
    Nothing -> [LUnorderedStart, LItemStart]

listItemFitter :: Level -> [Level] -> OrgParser [ListMarker]
listItemFitter level levels' = foldlM f [] levels'
  where
    f :: [ListMarker] -> Level -> OrgParser [ListMarker]
    f acc existingLevel
      | existingLevel.indent == level.indent = do
          modify (\s -> s {listLevels = level : drop 1 s.listLevels})
          pure $ acc <> [LItemEnd, LItemStart]
      | existingLevel.indent < level.indent = pure acc
      | otherwise = do
          orgParserState <- get
          let levels = orgParserState.listLevels
              markers = orgParserState.listMarkers
          put
            orgParserState
              { listLevels = drop 1 levels,
                listMarkers = drop 2 markers
              }
          let newMarkers = case existingLevel.number of
                Nothing -> [LItemEnd, LUnorderedEnd]
                Just _ -> [LItemEnd, LOrderedEnd]
          pure $ acc <> newMarkers

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
