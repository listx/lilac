module Lilac.Parse
  ( defaultOrgDoc,
    defaultOrgParserConf,
    genHeadingSection,
    genHeadingOidStack,
    isNumericText,
    maskToStyles,
    orgDocParser,
    parseOrgDoc,
    tableHorizontalLine,
    validateOidText,
  )
where

import Data.Bits (setBit, testBit, (.^.))
import Data.Bits.Bitwise (toListLE)
import Data.Char
  ( GeneralCategory (CurrencySymbol),
    generalCategory,
    isAsciiLower,
    isDigit,
    isSpace,
  )
import Data.List (groupBy)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Lilac.Types
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (Block, body, meta, parentOid),
    Cell
      ( CBibliography,
        CBlock,
        CFigure,
        CFootnote,
        CHeading,
        CListItem,
        CMathFragment,
        CParagraph,
        CTable
      ),
    Citation (Citation, cites, prefix, styleVariation, suffix),
    Cite (Cite, id, locator, prefix, suffix),
    Figure (Figure, caption, link, meta),
    Footnote (Footnote, body, name),
    Heading (Heading, body, level, meta, section),
    Level (Level, indent, number),
    Link (Link, name, target),
    LinkOid (LinkOid, name, oid),
    LinkTarget
      ( LinkTargetFilePath,
        LinkTargetFootnote,
        LinkTargetOid,
        LinkTargetURI
      ),
    ListMarker
      ( LItemEnd,
        LItemStart,
        LOrderedEnd,
        LOrderedStart,
        LUnorderedEnd,
        LUnorderedStart
      ),
    MathFragment (MathFragment, body, meta),
    OID,
    OrgDoc
      ( OrgDoc,
        bibPaths,
        cells,
        citations,
        cslPath,
        footnoteNumbers,
        meta,
        oid
      ),
    OrgParser,
    OrgParserConf (maxHeadingLevel, maxListLevel),
    PToken (PTokenCitation, PTokenLink, PTokenMathFragment, PTokenStyledText),
    Prose (Prose, unProse),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyleMask (StyleMask),
    StyledText (StyledText, body, style),
    Table (Table, caption, grid, meta),
    defaultOrgParserConf,
    defaultOrgParserState,
    fromText,
    markingChars,
    nilOid,
    protocolsEmacs,
    protocolsWeb,
    styleMaskFrom,
  )
import Lilac.Types as OrgParserState
  ( OrgParserState
      ( OrgParserState,
        anonBlockNumber,
        anonEquationNumber,
        bibPaths,
        bracketBalance,
        cellNames,
        citations,
        continuationIndent,
        cslPath,
        docOid,
        equationNumber,
        figureNumber,
        footnoteDefinitions,
        footnoteNumber,
        footnoteNumbers,
        headingLevel,
        headingOidStack,
        headingSection,
        insideCitation,
        insideCitePrefix,
        insideCodeBlock,
        insideHeading,
        insideLinkDisplayName,
        listLevels,
        listMarkers,
        orgParserConf,
        prevCellIsFootnote,
        styleMask,
        tableNumber,
        tanglePaths
      ),
  )
import Network.URI qualified as N
import Relude
  ( Bool (False, True),
    Char,
    Either (Left, Right),
    FilePath,
    Int,
    Map,
    Maybe (Just, Nothing),
    MonadFail,
    Ord,
    String,
    Text,
    abs,
    catMaybes,
    concatMap,
    drop,
    elem,
    fail,
    filter,
    find,
    flip,
    fmap,
    foldl',
    forM_,
    fromEnum,
    fromList,
    fromMaybe,
    fst,
    get,
    isJust,
    isNothing,
    length,
    many,
    map,
    mapM_,
    maybeToList,
    modify,
    not,
    notElem,
    null,
    optional,
    or,
    otherwise,
    pure,
    put,
    readMaybe,
    repeat,
    replicate,
    reverse,
    runStateT,
    show,
    take,
    toEnum,
    toList,
    unwords,
    void,
    when,
    zip,
    ($),
    (&&),
    (*),
    (+),
    (-),
    (.),
    (/=),
    (<),
    (<$>),
    (<=),
    (<>),
    (<|>),
    (=<<),
    (==),
    (>),
    (>=),
    (>>),
    (||),
  )
import Relude.Extra.Map
  ( delete,
    insert,
    keys,
    lookup,
    member,
    notMember,
  )
import Relude.Unsafe (fromJust)
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as L

skipNewlinesAndComments :: OrgParser ()
skipNewlinesAndComments =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    blockCommentParser
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = do
      _ <- MP.takeWhile1P (Just "newline") (== '\n')
      orgParserState <- get
      _ <-
        MP.optional
          . MP.label "indentation for next thing"
          $ MP.count orgParserState.continuationIndent (char ' ')
      pure ()
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = L.skipLineComment "# "
    blockCommentParser :: OrgParser ()
    blockCommentParser = L.skipBlockComment "#+begin_comment\n" "#+end_comment\n"

lexeme :: OrgParser a -> OrgParser a
lexeme = L.lexeme skipNewlinesAndComments

parseOrgDoc :: Text -> Either Text OrgDoc
parseOrgDoc input = case MP.runParser
  (runStateT orgDocParser defaultOrgParserState)
  ""
  input of
  Left err -> Left . T.pack $ MP.errorBundlePretty err
  Right (orgDoc, _orgParserState) -> Right orgDoc

orgDocParser :: OrgParser OrgDoc
orgDocParser = do
  _ <- optional skipNewlinesAndComments
  (docOid, cslPath, bibPaths, meta) <- docMetaParser
  _ <- lilacExtensionsParser
  cells <- many cellParser
  MP.eof
  footnoteNumbers <- reconcileFootnotes
  citations <- collectCitations
  resetState
  let meaningfulCell :: Cell -> Bool
      meaningfulCell = \case
        CListItem [] -> False
        CHeading {}
        CListItem {}
        CParagraph {}
        CBlock {}
        CMathFragment {}
        CFigure {}
        CTable {}
        CFootnote {}
        CBibliography {} -> True
  pure
    $ OrgDoc
      { oid = docOid,
        cells = filter meaningfulCell cells,
        footnoteNumbers = footnoteNumbers,
        cslPath = cslPath,
        bibPaths = bibPaths,
        citations = citations,
        meta = deleteRedundantKeys meta ["cite_export"]
      }

defaultOrgDoc :: OID -> [(Text, Text)] -> OrgDoc
defaultOrgDoc oid metas =
  OrgDoc
    { oid = oid,
      cells = [],
      footnoteNumbers = M.empty,
      cslPath = Nothing,
      bibPaths = [],
      citations = [],
      meta = fromList metas'
    }
  where
    metas' =
      if oid == nilOid
        then ("ID", "00000000-0000-0000-0000-000000000000") : metas
        else metas

docMetaEntryParser :: OrgParser (Text, Text)
docMetaEntryParser = do
  avoidParsingBlockMetadata
  avoidParsingLilacExtensions
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
      mapM_
        MP.notFollowedBy
        $ map
          string
          [ "#+begin_",
            "#+name: ",
            "#+header: ",
            "#+print_bibliography:"
          ]
    avoidParsingLilacExtensions :: OrgParser ()
    avoidParsingLilacExtensions = MP.notFollowedBy $ string "#+lilac_"

docMetaParser :: OrgParser (OID, Maybe FilePath, [FilePath], Map Text Text)
docMetaParser = do
  topLevelDrawerProps <- propertiesDrawerParser
  _ <- optional skipNewlinesAndComments
  metaKVsList <- many $ lexeme docMetaEntryParser
  let (bibPathsText, remainingKVs) = collectValsFor "bibliography" metaKVsList
      metaKVs = fromList remainingKVs
      bibPaths = map T.unpack bibPathsText
  let conflictingKeys = keys $ M.intersection topLevelDrawerProps metaKVs
  when (not $ null conflictingKeys)
    $ fail
    $ "conflicting keys in the top-level PROPERTIES drawer and document metadata key/values: "
    <> (show conflictingKeys)
  case lookup "ID" topLevelDrawerProps of
    Nothing -> fail $ "required document ID property not found"
    Just oidText -> do
      oid <- validateOidText oidText
      cslPath <- case lookup "cite_export" metaKVs of
        Just maybeCslPath -> validateCslPath maybeCslPath
        Nothing -> pure Nothing
      modify $ \s ->
        s
          { OrgParserState.bibPaths = bibPaths,
            OrgParserState.cslPath = cslPath,
            docOid = oid
          }
      pure (oid, cslPath, bibPaths, topLevelDrawerProps <> metaKVs)

lilacExtensionsParser :: OrgParser ()
lilacExtensionsParser = do
  void
    . many
    $ MP.choice
      [ maxHeadingLevelParser,
        maxListLevelParser
      ]

maxHeadingLevelParser :: OrgParser ()
maxHeadingLevelParser = do
  orgParserState <- get
  _ <- string $ "#+lilac_maxHeadingLevel: "
  _ <- many $ char ' '
  val <- signedIntParser
  let opc = orgParserState.orgParserConf {maxHeadingLevel = val}
  put orgParserState {orgParserConf = opc}

maxListLevelParser :: OrgParser ()
maxListLevelParser = do
  orgParserState <- get
  _ <- string $ "#+lilac_maxListLevel: "
  _ <- many $ char ' '
  val <- signedIntParser
  let opc = orgParserState.orgParserConf {maxListLevel = val}
  put orgParserState {orgParserConf = opc}

signedIntParser :: OrgParser Int
signedIntParser = L.signed parserBetweenSignAndNumber number
  where
    number :: OrgParser Int
    number = L.lexeme skipNewlinesAndComments $ L.decimal
    parserBetweenSignAndNumber :: OrgParser ()
    parserBetweenSignAndNumber = pure ()

cellParser :: OrgParser Cell
cellParser = do
  cell <-
    lexeme
      $ MP.choice
        [ toggleFootnote False $ listItemStartParser,
          toggleFootnote False $ listItemsEndParser,
          toggleFootnote False $ resetContinuationIndent headingParser,
          toggleFootnote False $ resetContinuationIndent blockParser,
          toggleFootnote False $ resetContinuationIndent figureParser,
          toggleFootnote False $ resetContinuationIndent tableParser,
          toggleFootnote False $ resetContinuationIndent mathFragmentStandaloneParser,
          toggleFootnote True $ resetContinuationIndent footnoteDefinitionParser,
          toggleFootnote False $ resetContinuationIndent bibliographyLocationParser,
          toggleFootnote False $ resetContinuationIndent paragraphParser,
          detectInvalidIndentation
        ]
  modify (\s -> s {bracketBalance = 0})
  pure cell
  where
    resetContinuationIndent :: OrgParser a -> OrgParser a
    resetContinuationIndent p = do
      a <- p
      modify (\s -> s {continuationIndent = 0})
      pure a
    toggleFootnote :: Bool -> OrgParser a -> OrgParser a
    toggleFootnote b p = do
      a <- p
      modify (\s -> s {prevCellIsFootnote = b})
      pure a

detectInvalidIndentation :: OrgParser Cell
detectInvalidIndentation = do
  _ <- MP.hidden $ MP.takeWhile1P (Just "indentation") (== ' ')
  void
    $ MP.fancyFailure
    . Set.singleton
    . MP.ErrorFail
    $ "invalid indentation"
  pure $ CListItem []

headingParser :: OrgParser Cell
headingParser = do
  orgParserState <- get
  _ <- MP.try $ MP.lookAhead (starsParser >> char ' ')
  let opc = orgParserState.orgParserConf
  stars <- starsParser
  when (opc.maxHeadingLevel < 0) $ do
    fail
      $ "headings prohibited because lilac_maxHeadingLevel < 0"
  let lenStars = T.length stars
      currentHeadingLevel = orgParserState.headingLevel
  when (opc.maxHeadingLevel > 0 && lenStars > opc.maxHeadingLevel) $ do
    fail
      $ "found "
      <> show lenStars
      <> " asterisks found for this heading, but "
      <> "lilac_maxHeadingLevel = "
      <> show opc.maxHeadingLevel
  failIfHeadingNestsWithoutDirectParent currentHeadingLevel lenStars
  _ <- char ' '
  modify (\s -> s {insideHeading = True})
  prose <- proseParser spaceConsumer
  _ <- char '\n'
  failIfLeftoverActiveStyles
  props <- propertiesDrawerParser
  let newHeadingSection = genHeadingSection lenStars orgParserState.headingSection
      newHeadingOidStack =
        genHeadingOidStack
          (lookup "ID" props)
          lenStars
          orgParserState.headingOidStack
  put
    orgParserState
      { headingLevel = lenStars,
        headingSection = newHeadingSection,
        headingOidStack = newHeadingOidStack,
        insideHeading = False
      }
  pure
    $ CHeading
      Heading
        { level = lenStars,
          section = newHeadingSection,
          body = compressProse prose,
          meta = props
        }
  where
    spaceConsumer :: OrgParser Text
    spaceConsumer = MP.takeWhile1P (Just "space") (== ' ')
    starsParser :: OrgParser Text
    starsParser = MP.takeWhile1P (Just "heading star") (\c -> c == '*')

failIfHeadingNestsWithoutDirectParent :: Int -> Int -> OrgParser ()
failIfHeadingNestsWithoutDirectParent current found =
  when (found > current + 1) $ do
    fail
      $ "headings may not skip levels (expected to see level "
      <> show current
      <> " or "
      <> (show $ current + 1)
      <> ", not "
      <> show found
      <> ")"

genHeadingSection :: Int -> [Int] -> [Int]
genHeadingSection level prevSection
  | null prevSection = 1 : (take (level - 1) $ repeat 0)
  | length prevSection == level = bumpDeepestSection prevSection
  | length prevSection > level =
      bumpDeepestSection
        $ drop (length prevSection - level) prevSection
  | otherwise =
      let levelDiff = (level - length prevSection - 1)
       in (1 : (take levelDiff $ repeat 0)) <> prevSection
  where
    bumpDeepestSection :: [Int] -> [Int]
    bumpDeepestSection [] = []
    bumpDeepestSection (x : xs) = x + 1 : xs

genHeadingOidStack :: Maybe Text -> Int -> [Maybe OID] -> [Maybe OID]
genHeadingOidStack maybeId level old
  | length old == 0 = [newId]
  | length old == level = newId : drop 1 old
  | length old < level =
      let nothings = take (levelDiff - 1) $ repeat Nothing
       in (newId : nothings) <> old
  | otherwise = newId : (drop (abs levelDiff + 1) old)
  where
    levelDiff = level - length old
    newId = fromText =<< maybeId

propertiesDrawerParser :: OrgParser (Map Text Text)
propertiesDrawerParser = propertiesParser <|> (pure $ fromList [])

propertiesParser :: OrgParser (Map Text Text)
propertiesParser =
  fromList
    <$> MP.between
      (lexeme $ string ":PROPERTIES:\n")
      (string ":END:\n")
      (many $ lexeme propertiesEntryParser)

propertiesEntryParser :: OrgParser (Text, Text)
propertiesEntryParser = do
  MP.notFollowedBy $ string ":END:"
  _ <- char ':'
  key <- MP.label "property key" . MP.some $ MP.noneOf ['\n', ':', ' ']
  _ <- char ':'
  _ <- MP.takeWhile1P (Just "space") (== ' ')
  value <-
    MP.manyTill
      (MP.label "property value" MP.anySingle)
      (char '\n')
  pure (T.pack key, T.pack value)

proseParser :: OrgParser Text -> OrgParser Prose
proseParser spaceConsumer = do
  orgParserState <- get
  let avoidCitations =
        orgParserState.insideHeading || orgParserState.insideCitation
  a <- MP.choice $ baseParsers avoidCitations
  b <-
    MP.many
      $ MP.choice (spacePTokenParser : baseParsers avoidCitations)
  pure . Prose . filter nonEmpty $ a : b
  where
    spacePTokenParser = PTokenStyledText <$> whitespaceCompressor spaceConsumer
    baseParsers avoidCitations
      | avoidCitations =
          [ PTokenLink <$> linkParser,
            PTokenMathFragment <$> mathFragmentInlineParser,
            PTokenStyledText <$> styleMarkerParser,
            PTokenStyledText <$> styledTextParser
          ]
      | otherwise =
          [ PTokenCitation <$> citationParser,
            PTokenLink <$> linkParser,
            PTokenMathFragment <$> mathFragmentInlineParser,
            PTokenStyledText <$> styleMarkerParser,
            PTokenStyledText <$> styledTextParser
          ]
    nonEmpty :: PToken -> Bool
    nonEmpty = \case
      PTokenStyledText styledText -> not $ T.null styledText.body
      PTokenLink {}
      PTokenMathFragment {}
      PTokenCitation {} -> True

proseSpaceConsumer :: Int -> OrgParser Text
proseSpaceConsumer expectedIndentLen = do
  _ <- beLessGreedy
  MP.choice
    [ newlineAndIndentParser expectedIndentLen,
      spaces
    ]
  where
    spaces :: OrgParser Text
    spaces = MP.takeWhile1P (Just "prose space") (\c -> c == ' ')
    beLessGreedy :: OrgParser ()
    beLessGreedy = do
      orgParserState <- get
      let toAvoid =
            [ (orgParserState.insideCitePrefix, string " " >> citeIdParser)
            ]
      forM_
        toAvoid
        $ \(condition, p) -> when condition $ MP.notFollowedBy p

whitespaceCompressor :: OrgParser Text -> OrgParser StyledText
whitespaceCompressor spaceConsumer = do
  orgParserState <- get

  let mask = orgParserState.styleMask

  _ <- spaceConsumer

  ws <-
    MP.lookAhead
      $ MP.choice
        [ MP.eof >> pure "",
          footnoteReferenceParser >> pure "",
          MP.anySingle >> pure " "
        ]

  pure
    StyledText
      { body = ws,
        style = mask
      }

styleMarkers :: Map Char Style
styleMarkers =
  fromList
    [ ('*', Bold),
      ('/', Italic),
      ('_', Underline),
      ('=', Verbatim),
      ('~', Code)
    ]

failIfLeftoverActiveStyles :: OrgParser ()
failIfLeftoverActiveStyles = do
  orgParserState <- get
  let (StyleMask mask) = orgParserState.styleMask
  when (mask /= 0) $ do
    fail
      $ "leftover active styles: "
      <> (show $ maskToStyles $ StyleMask mask)

styleMarkerParser :: OrgParser StyledText
styleMarkerParser = do
  orgParserState <- get

  let mask = orgParserState.styleMask
  c <- MP.satisfy $ isStyleMarker mask
  styleMask <- toStyleMask c

  let newMask = styleMaskFlip mask styleMask

  put orgParserState {styleMask = newMask}
  pure
    StyledText
      { body = "",
        style = newMask
      }

isStyleMarker :: StyleMask -> Char -> Bool
isStyleMarker styleMask c
  | isVerbatim styleMask = c == '='
  | isCode styleMask = c == '~'
  | otherwise = member c styleMarkers

styledTextParser :: OrgParser StyledText
styledTextParser = do
  orgParserState <- get

  let mask = orgParserState.styleMask

  text <- textParser mask

  pure
    StyledText
      { body = text,
        style = mask
      }

textParser :: StyleMask -> OrgParser Text
textParser styleMask
  | isVerbatim styleMask =
      MP.takeWhile1P (Just "verbatim text") isNotVerbatimMarker
  | isCode styleMask =
      MP.takeWhile1P (Just "code") isNotCodeMarker
  | otherwise = do
      ws <-
        MP.some
          $ MP.choice
            [ MP.label "normal characters" normalChars,
              escapedSymbolParser styleMask
            ]
      pure $ T.concat ws
  where
    isNotVerbatimMarker c = c /= '='
    isNotCodeMarker c = c /= '~'
    normalChars :: OrgParser Text
    normalChars = do
      cs <- MP.some $ normalChar
      pure $ T.pack cs
    normalChar :: OrgParser Char
    normalChar = do
      orgParserState <- get
      let toAvoid =
            [ (orgParserState.insideCitation, string ";"),
              (orgParserState.insideCitePrefix, citeIdParser),
              ( orgParserState.insideCitation
                  && (orgParserState.bracketBalance == 0),
                string "]"
              ),
              (orgParserState.insideLinkDisplayName, string "]]"),
              ( not orgParserState.insideCitation
                  && not orgParserState.insideCitePrefix
                  && not orgParserState.insideLinkDisplayName,
                string "[["
              )
            ]
      forM_
        toAvoid
        $ \(condition, p) -> when condition $ MP.notFollowedBy p
      c <- MP.satisfy isNormalChar
      let bracketBalanceDiff
            | c == '[' = 1
            | c == ']' = -1
            | otherwise = 0
      modify (\s -> s {bracketBalance = s.bracketBalance + bracketBalanceDiff})
      pure c
    isNormalChar c =
      notMember c styleMarkers
        && (not $ isSpace c)
        && (not $ c == '\\')

escapedSymbolParser :: StyleMask -> OrgParser Text
escapedSymbolParser styleMask = do
  _ <- char '\\'
  s <-
    MP.choice
      [ do
          c <- MP.satisfy (\c -> isStyleMarker styleMask c || c == '\\')
          pure $ T.singleton c,
        string "vert{}"
      ]
  if s == "vert{}"
    then pure "|"
    else pure s

maskToStyles :: StyleMask -> [Style]
maskToStyles (StyleMask mask) = foldl' f [] $ zip [0 ..] $ toListLE mask
  where
    f :: [Style] -> (Int, Bool) -> [Style]
    f acc (idx, b) =
      if b
        then (toEnum idx) : acc
        else acc

toStyleMask :: (MonadFail m) => Char -> m StyleMask
toStyleMask cs = pure =<< styleToStyleMask =<< charToStyle cs

charToStyle :: (MonadFail m) => Char -> m Style
charToStyle c = case lookup c styleMarkers of
  Just style -> pure style
  Nothing -> fail $ "programmer error: unrecognized style marker " <> (show c)

styleToStyleMask :: (MonadFail m) => Style -> m StyleMask
styleToStyleMask s = pure $ StyleMask (setBit 0 $ fromEnum s)

styleMaskFlip :: StyleMask -> StyleMask -> StyleMask
styleMaskFlip (StyleMask a) (StyleMask b) = StyleMask $ a .^. b

isVerbatim :: StyleMask -> Bool
isVerbatim (StyleMask a) = testBit a (fromEnum Verbatim)

isCode :: StyleMask -> Bool
isCode (StyleMask a) = testBit a (fromEnum Code)

compressProse :: Prose -> Prose
compressProse prose =
  Prose
    . reverse
    . dropUnnecessary
    $ foldl' f [] prose.unProse
  where
    f :: [PToken] -> PToken -> [PToken]
    f [] token = token : []
    f
      ((t1@(PTokenStyledText (StyledText body1 style1))) : rest)
      t2@(PTokenStyledText (StyledText body2 style2))
        | style1 == style2 =
            (PTokenStyledText $ StyledText (T.concat [body1, body2]) style1) : rest
        | otherwise = t2 : t1 : rest
    f acc token = token : acc
    dropUnnecessary [] = []
    dropUnnecessary (((PTokenStyledText (StyledText " " _))) : rest) = rest
    dropUnnecessary (((PTokenStyledText (StyledText "" _))) : rest) = rest
    dropUnnecessary tokens = tokens

assertUniqueCellName :: Text -> OrgParser ()
assertUniqueCellName cellName = do
  orgParserState <- get
  when (Set.member cellName orgParserState.cellNames)
    . fail
    $ "cell name "
    <> show cellName
    <> " is not unique"
  put
    orgParserState
      { cellNames = Set.insert cellName orgParserState.cellNames
      }

linkParser :: OrgParser Link
linkParser = do
  failIfVerbatimOrCode

  MP.choice
    [ MP.try oidLinkParser,
      MP.try protocolWebLinkParser,
      MP.try fileLinkParser,
      MP.try footnoteLinkParser,
      MP.try rawLinkParser
    ]

linkDisplayNameParser :: OrgParser [StyledText]
linkDisplayNameParser = do
  orgParserState <- get
  modify (\s -> s {insideLinkDisplayName = True})
  linkDisplayName <-
    MP.between
      (string "[")
      (string "]]")
      (proseParser' $ proseSpaceConsumer orgParserState.continuationIndent)
  modify (\s -> s {insideLinkDisplayName = False})
  pure linkDisplayName
  where
    proseParser' :: OrgParser Text -> OrgParser [StyledText]
    proseParser' spaceConsumer = do
      a <- MP.choice baseParsers
      b <- MP.many $ MP.choice (spaceParser : baseParsers)
      pure . filter nonEmpty $ a : b
      where
        spaceParser = whitespaceCompressor spaceConsumer
        baseParsers =
          [ styleMarkerParser,
            styledTextParser
          ]
        nonEmpty :: StyledText -> Bool
        nonEmpty styledText = not $ T.null styledText.body

failIfVerbatimOrCode :: OrgParser ()
failIfVerbatimOrCode = do
  orgParserState <- get
  let mask = orgParserState.styleMask
  when (isVerbatim mask || isCode mask)
    $ fail "aborting because verbatim or code style is active"

protocolWebLinkParser :: OrgParser Link
protocolWebLinkParser = do
  maybeUri <-
    MP.between
      (string "[[")
      (string "]")
      uriParser

  orgParserState <- get
  let mask = orgParserState.styleMask
      implicitName = maybeUri
      implicitNameStyledText =
        StyledText
          { body = implicitName,
            style = mask
          }
  displayName <-
    MP.choice
      [ MP.try $ string "]" >> pure [implicitNameStyledText],
        MP.try linkDisplayNameParser
      ]
  case N.parseURI (T.unpack maybeUri) of
    Nothing -> fail $ "(protocol web link) not a valid URI: " <> (T.unpack maybeUri)
    Just uri ->
      pure
        $ Link
          { target = LinkTargetURI uri,
            name = displayName
          }
  where
    uriParser :: OrgParser Text
    uriParser = do
      protocol <- MP.choice $ map (MP.try . string) protocolsWeb
      rest <- MP.takeWhile1P (Just "name character") (/= ']')
      pure $ protocol <> rest

fileLinkParser :: OrgParser Link
fileLinkParser = do
  maybeFilePath <-
    MP.between
      (string "[[")
      (string "]")
      filePathParser

  orgParserState <- get
  let mask = orgParserState.styleMask
      implicitName = maybeFilePath
      implicitNameStyledText =
        StyledText
          { body = implicitName,
            style = mask
          }
      filePath = T.unpack maybeFilePath
  displayName <-
    MP.choice
      [ MP.try $ string "]" >> pure [implicitNameStyledText],
        MP.try linkDisplayNameParser
      ]
  if
    | Just osPath <- P.encodeUtf filePath,
      P.isValid osPath ->
        pure
          $ Link
            { target = LinkTargetFilePath filePath,
              name = displayName
            }
    | otherwise -> fail $ "not a valid file path: " <> filePath
  where
    filePathParser :: OrgParser Text
    filePathParser = do
      _ <- string "file:"
      fp <- MP.takeWhile1P (Just "name character") (/= ']')
      pure fp

rawLinkParser :: OrgParser Link
rawLinkParser = do
  _ <- MP.try . MP.lookAhead $ do
    _ <- MP.takeWhile1P (Just "(raw link) protocol") isAlphaChar
    _ <- char ':'
    pure ()
  candidate <- MP.some $ MP.try remainderChar
  case N.parseURI candidate of
    Nothing -> fail $ "(raw link) not a valid URI: " <> candidate
    Just uri ->
      pure
        Link
          { target = LinkTargetURI uri,
            name =
              [ StyledText
                  { body = T.pack candidate,
                    style = styleMaskFrom []
                  }
              ]
          }
  where
    remainderChar :: OrgParser Char
    remainderChar = do
      c <- MP.satisfy N.isAllowedInURI
      when (isPunctuation c) $ do
        MP.notFollowedBy (char ' ')
        MP.notFollowedBy (char '\n')
      pure c

mathFragmentInlineParser :: OrgParser MathFragment
mathFragmentInlineParser = do
  failIfVerbatimOrCode

  _ <- string "\\("
  latex <- MP.someTill (MP.label "LaTeX math input" MP.anySingle) (string "\\)")
  pure
    MathFragment
      { meta = fromList [],
        body = T.pack latex
      }

paragraphParser :: OrgParser Cell
paragraphParser = do
  orgParserState <- get
  prose <- proseParser $ proseSpaceConsumer orgParserState.continuationIndent
  failIfLeftoverActiveStyles
  pure . CParagraph $ compressProse prose

newlineAndIndentParser :: Int -> OrgParser Text
newlineAndIndentParser expectedIndentLen = do
  MP.notFollowedBy cellBoundary
  _ <- string "\n"
  maybeIndent
  pure " "
  where
    maybeIndent =
      if (expectedIndentLen == 0)
        then nonSpace
        else MP.eof <|> exactIndent
    nonSpace :: OrgParser ()
    nonSpace = MP.notFollowedBy (char ' ')
    exactIndent :: OrgParser ()
    exactIndent = do
      _ <-
        MP.label "indentation for paragraph continuation"
          $ MP.count expectedIndentLen (char ' ')
      MP.notFollowedBy (char ' ')
      MP.notFollowedBy (char '\n')
      pure ()

cellBoundary :: OrgParser Text
cellBoundary = string "\n\n"

blockMetaEntryParser :: OrgParser (Text, Text)
blockMetaEntryParser = do
  _ <- string "#+header: :"
  key <- MP.takeWhile1P (Just "(block meta) keyword letter") isKeywordLetter
  _ <- char ' '
  o <- MP.getOffset
  value <- MP.takeWhile1P (Just "value letter") (\c -> c /= '\n')
  _ <- char '\n'
  assertUniqueTanglePath key value o
  pure (key, value)
  where
    isKeywordLetter c = or $ map ($ c) [isAsciiLower, (== '-')]

metaNameParser :: OrgParser (Text, Text)
metaNameParser = do
  _ <- string "#+name: "
  name <- T.pack <$> MP.someTill (MP.label "name of block" MP.anySingle) (char '\n')
  assertUniqueCellName name
  pure ("name", name)

assertUniqueTanglePath :: Text -> Text -> Int -> OrgParser ()
assertUniqueTanglePath "tangle" path offset = do
  orgParserState <- get
  when (Set.member path orgParserState.tanglePaths)
    $ do
      MP.setOffset $ offset
      fail $ "tangle path " <> show path <> " is not unique"
  put
    orgParserState
      { tanglePaths = Set.insert path orgParserState.tanglePaths
      }
assertUniqueTanglePath _ _ _ = pure ()

blockParser :: OrgParser Cell
blockParser = do
  orgParserState <- get
  (blockName, metaKVs, blockType, srcLang) <- blockStartParser
  let nowebOff =
        (== Just "off")
          $ lookup "noweb" metaKVs
  modify (\s -> s {insideCodeBlock = True})
  bTokens <-
    MP.choice
      [ zeroBTokensParser blockType,
        someBTokensParser blockType (not nowebOff)
      ]
  let newAnonBlockNumber = case blockName of
        Just _ -> orgParserState.anonBlockNumber
        Nothing -> orgParserState.anonBlockNumber + 1
      anonBlockMeta = case blockName of
        Just _ -> Nothing
        Nothing -> Just ("__anon_block_number", T.show newAnonBlockNumber)
      block =
        Block
          { parentOid =
              firstParent orgParserState.docOid orgParserState.headingOidStack,
            meta =
              foldl'
                (\acc (k, v) -> insert k v acc)
                (insert "__block_type" blockType metaKVs)
                $ concatMap maybeToList [blockName, srcLang, anonBlockMeta],
            body =
              ignoreSpecialCommas
                . compressBTokens
                $ padConsecutiveNowebLinks bTokens
          }
  modify
    ( \s ->
        s
          { anonBlockNumber = newAnonBlockNumber,
            insideCodeBlock = False
          }
    )
  pure $ CBlock block
  where
    firstParent :: OID -> [Maybe OID] -> OID
    firstParent docOid maybeOids = case catMaybes maybeOids of
      (oid : _) -> oid
      [] -> docOid

blockStartParser ::
  OrgParser
    ( Maybe (Text, Text),
      Map Text Text,
      Text,
      Maybe (Text, Text)
    )
blockStartParser = do
  _ <- MP.try $ MP.lookAhead (metaParser >> string "#+begin_")
  blockName <- MP.optional metaNameParser
  metaKVs <- fromList <$> (many $ lexeme blockMetaEntryParser)
  _ <- string "#+begin_"
  blockType <- MP.takeWhile1P (Just "block type") isAsciiLower
  srcLang <- MP.optional $ do
    _ <- MP.skipSome $ char ' '
    src <- MP.takeWhile1P (Just "src language") isSrcLanguageChar
    pure ("__src_language", src)
  when (blockType == "src" && srcLang == Nothing)
    $ fail "block type src requires a language specifier"
  _ <- char '\n'
  pure (blockName, metaKVs, blockType, srcLang)
  where
    isSrcLanguageChar c = isAsciiLower c || c == '-'

bTokenParser :: Bool -> Bool -> OrgParser BToken
bTokenParser isSrcBlock nowebOn = MP.choice parsers
  where
    baseParsers :: [OrgParser BToken]
    baseParsers =
      [ BTokenString
          <$> (MP.takeWhile1P (Just "block token") (`notElem` ['\n', ' ', '<'])),
        BTokenString
          . T.singleton
          <$> (MP.label "left angle bracket" $ MP.satisfy (== '<')),
        BTokenString
          . T.singleton
          <$> (MP.label "space inside block" $ MP.satisfy isSpace)
      ]
    parsers =
      if isSrcBlock && nowebOn
        then ((BTokenLink <$> MP.try nowebLinkParser) : baseParsers)
        else baseParsers

zeroBTokensParser :: Text -> OrgParser [BToken]
zeroBTokensParser blockType = do
  _ <- string $ T.concat ["#+end_", blockType, "\n"]
  pure []

someBTokensParser :: Text -> Bool -> OrgParser [BToken]
someBTokensParser blockType nowebOn =
  MP.someTill
    (bTokenParser (blockType == "src") nowebOn)
    (string $ T.concat ["\n#+end_", blockType, "\n"])

nowebLinkParser :: OrgParser Link
nowebLinkParser = do
  _ <- string "<<"
  (oid, linkName) <- oidReferenceParser "<>"
  _ <- string ">>"
  pure
    $ Link
      { target =
          LinkTargetOid
            LinkOid
              { oid = oid,
                name = linkName
              },
        name =
          [ StyledText
              { body = linkName,
                style = styleMaskFrom []
              }
          ]
      }

oidLinkParser :: OrgParser Link
oidLinkParser = do
  _ <- string "[["
  avoidParsingKnownProtocols
  (oid, name) <- oidReferenceParser "[]"
  orgParserState <- get
  let mask = orgParserState.styleMask
      implicitName
        | T.null name = "id:" <> T.show oid
        | otherwise = name
      implicitNameStyledText =
        StyledText
          { body = implicitName,
            style = mask
          }

  _ <- string "]"
  displayName <-
    MP.choice
      [ MP.try $ string "]" >> pure [implicitNameStyledText],
        MP.try linkDisplayNameParser
      ]
  pure
    $ Link
      { target =
          LinkTargetOid
            LinkOid
              { oid = oid,
                name = name
              },
        name = displayName
      }
  where
    avoidParsingKnownProtocols :: OrgParser ()
    avoidParsingKnownProtocols =
      MP.notFollowedBy
        . MP.choice
        . map (MP.try . string)
        $ protocolsWeb
        <> protocolsEmacs

oidReferenceParser :: [Char] -> OrgParser (OID, Text)
oidReferenceParser disallowedChars =
  MP.choice
    [ MP.try explicitFQCN,
      MP.try explicitOID,
      MP.try implicit
    ]
  where
    explicitOID :: OrgParser (OID, Text)
    explicitOID = do
      _ <- string "id:"
      oidText <-
        MP.takeWhile1P
          (Just "oid char")
          (`notElem` (':' : disallowedChars))
      oid <- validateOidText oidText
      pure (oid, "")
    explicitFQCN = do
      (oid, _) <- explicitOID
      _ <- string "::"
      name <- targetName
      pure (oid, name)
    implicit = do
      orgParserState <- get
      name <- targetName
      pure (orgParserState.docOid, name)
    targetName :: OrgParser Text
    targetName = do
      orgParserState <- get
      tn <-
        proseParser'
          $ proseSpaceConsumer'
            orgParserState.insideCodeBlock
      pure $ compressWhitespace tn
    compressWhitespace = T.unwords . T.words
    proseSpaceConsumer' :: Bool -> OrgParser Text
    proseSpaceConsumer' insideCodeBlock
      | insideCodeBlock = spaces
      | otherwise =
          MP.choice
            [ string "\n",
              spaces
            ]
      where
        spaces :: OrgParser Text
        spaces = MP.takeWhile1P (Just "space") (\c -> c == ' ')
    proseParser' :: OrgParser Text -> OrgParser Text
    proseParser' spaceConsumer = do
      a <- MP.choice baseParsers
      b <- MP.many $ MP.choice (spaceParser : baseParsers)
      pure . T.concat $ a : b
      where
        spaceParser = do
          styledText <- whitespaceCompressor spaceConsumer
          pure styledText.body
        baseParsers =
          [ textParser'
          ]
    textParser' :: OrgParser Text
    textParser' = do
      ws <-
        MP.some
          $ MP.choice
            [ MP.label "normal characters" normalChars
            ]
      pure $ T.concat ws
      where
        normalChars :: OrgParser Text
        normalChars = do
          cs <- MP.some $ normalChar
          pure $ T.pack cs
        normalChar :: OrgParser Char
        normalChar = MP.satisfy isNormalChar
        isNormalChar c =
          notElem c disallowedChars
            && (not $ isSpace c)

ignoreSpecialCommas :: [BToken] -> [BToken]
ignoreSpecialCommas = reverse . foldl' f []
  where
    f acc (BTokenString text) = (BTokenString $ removeSpecialCommas text) : acc
    f acc BTokenPadding = BTokenPadding : acc
    f acc link = link : acc
    removeSpecialCommas text =
      T.intercalate "\n"
        . map removeSpecialComma
        $ T.split (== '\n') text
    removeSpecialComma line
      | hasSpecialComma line = dropLeadingComma line
      | otherwise = line
    hasSpecialComma line
      | (indentAndCommas, rest) <- T.breakOnEnd "," line,
        (indent, commas) <- T.breakOn "," indentAndCommas,
        T.stripStart indent == "",
        not $ T.null commas,
        T.all (== ',') commas,
        T.take 1 rest == "*" || T.take 2 rest == "#+" =
          True
      | otherwise = False
    dropLeadingComma line =
      let (indent, commas) = T.breakOn "," line
       in indent <> T.drop 1 commas

compressBTokens :: [BToken] -> [BToken]
compressBTokens = reverse . foldl' f []
  where
    f :: [BToken] -> BToken -> [BToken]
    f [] token = token : []
    f ((BTokenString text1) : rest) (BTokenString text2) =
      (BTokenString $ T.append text1 text2) : rest
    f acc token = token : acc

padConsecutiveNowebLinks :: [BToken] -> [BToken]
padConsecutiveNowebLinks blockBody =
  reverse
    . fst
    $ foldl' f ([], getLinkPaddingBools blockBody) blockBody
  where
    f (acc, bools) text@(BTokenString _) = (text : acc, bools)
    f (acc, bools) BTokenPadding = (acc, bools)
    f (acc, bools) link@(BTokenLink _) =
      case bools of
        (padLink : rest)
          | padLink -> (BTokenPadding : link : acc, rest)
          | otherwise -> (link : acc, rest)
        [] -> (link : acc, [])

getLinkPaddingBools :: [BToken] -> [Bool]
getLinkPaddingBools blockBody =
  concatMap (concatMap distributeBools)
    . map (reverse . (zip $ False : repeat True) . reverse)
    . groupBy consecutiveNowebLines
    . T.lines
    $ getBlockContents blockBody
  where
    consecutiveNowebLines :: Text -> Text -> Bool
    consecutiveNowebLines a b = (countLinks a > 0) && (countLinks b > 0)
    distributeBools (False, line) =
      replicate (countLinks line) False
    distributeBools (True, line) =
      case countLinks line of
        1 -> [True]
        n -> reverse $ True : replicate (n - 1) False
    countLinks = T.count "\NUL_link"

getBlockContents :: [BToken] -> Text
getBlockContents blockBody = T.concat $ map bTokenToText blockBody
  where
    bTokenToText :: BToken -> Text
    bTokenToText = \case
      BTokenString text -> text
      BTokenPadding -> ""
      BTokenLink (Link _name (LinkTargetOid linkOid)) ->
        "\NUL_link" <> (T.show linkOid)
      BTokenLink {} -> ""

zeroIndent :: OrgParser (Int, Int)
zeroIndent = do
  orgParserState <- get
  _ <- MP.eof <|> MP.notFollowedBy (char ' ')
  let listLevelsToClose = length orgParserState.listLevels
      continuationIndent = 0 :: Int
  pure (listLevelsToClose, continuationIndent)

someIndent :: OrgParser (Int, Int)
someIndent = do
  orgParserState <- get
  indentFound <- T.length <$> MP.takeWhile1P (Just "indentation") (== ' ')
  let levelsToDropPerIndentationIncrement :: [(Int, Int)]
      levelsToDropPerIndentationIncrement =
        zip [0 ..]
          . map getContinuationIndentForLevel
          $ orgParserState.listLevels
      matchingIndent =
        find
          (\(_, continuationIndent) -> indentFound == continuationIndent)
          levelsToDropPerIndentationIncrement
  case matchingIndent of
    Nothing -> fail "indentation does not fit into existing levels"
    Just x -> pure x

getContinuationIndentForLevel :: Level -> Int
getContinuationIndentForLevel level = level.indent + maybeNum + punctuation + space
  where
    punctuation :: Int
    punctuation = 1
    space :: Int
    space = 1
    maybeNum :: Int
    maybeNum = case level.number of
      Just n -> length $ (show n :: String)
      Nothing -> 0

listItemsEndParser :: OrgParser Cell
listItemsEndParser = do
  orgParserState <- get
  when (null orgParserState.listMarkers) $ do
    fail "no pending ListMarker objects to close off"
  when (orgParserState.continuationIndent /= 0) $ do
    fail "not at beginning of line"
  (levelsToDrop, continuationIndent) <- zeroIndent <|> someIndent
  let markers = getEndingListMarkers levelsToDrop orgParserState.listLevels
  put
    orgParserState
      { listLevels = drop levelsToDrop orgParserState.listLevels,
        listMarkers = drop (levelsToDrop * 2) orgParserState.listMarkers,
        continuationIndent = continuationIndent
      }
  pure $ CListItem markers

getEndingListMarkers :: Int -> [Level] -> [ListMarker]
getEndingListMarkers levelsToDrop = concatMap f . take levelsToDrop
  where
    f :: Level -> [ListMarker]
    f level = case level.number of
      Nothing -> [LItemEnd, LUnorderedEnd]
      Just _ -> [LItemEnd, LOrderedEnd]

listHeadIndentLevelParser :: OrgParser Level
listHeadIndentLevelParser = do
  minimalIndentation
  spaces <- MP.takeWhile1P (Just "indentation") (== ' ')
  listHead <- unorderedHeadParser <|> orderedHeadParser
  let level =
        Level
          { indent = T.length spaces,
            number = listHead
          }
  afterListHead level
  pure level
  where
    minimalIndentation :: OrgParser ()
    minimalIndentation =
      void
        . MP.try
        . MP.lookAhead
        . MP.label "minimal indentation"
        $ MP.count 2 (char ' ')
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
      modify (\s -> s {continuationIndent = getContinuationIndentForLevel level})

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

listItemStartParser :: OrgParser Cell
listItemStartParser = do
  _ <- MP.try $ MP.lookAhead listHeadIndentLevelParser
  orgParserState <- get
  let levels = orgParserState.listLevels
  level <- listHeadIndentLevelParser
  listMarkers <-
    if nestsBeyond levels level
      then listItemNester level levels
      else listItemFitter level levels
  pure $ CListItem listMarkers
  where
    nestsBeyond :: [Level] -> Level -> Bool
    nestsBeyond [] _ = True
    nestsBeyond (deepestLevel : _) level =
      getContinuationIndentForLevel deepestLevel == level.indent

listItemNester :: Level -> [Level] -> OrgParser [ListMarker]
listItemNester level levels = do
  case level.number of
    Just 0 -> pure ()
    Just 1 -> pure ()
    Just n -> fail $ "starting list head number must be 0 or 1; got " <> show n
    Nothing -> pure ()
  orgParserState <- get
  let newMarker = case level.number of
        Just _ -> LOrderedStart
        Nothing -> LUnorderedStart
      opc = orgParserState.orgParserConf
  when (opc.maxListLevel > 0 && length levels == opc.maxListLevel) $ do
    fail
      $ "max nesting depth exceeded: cannot have more than "
      <> show opc.maxListLevel
      <> " levels of list items"
  put
    orgParserState
      { listLevels = level : levels,
        listMarkers = LItemStart : newMarker : orgParserState.listMarkers
      }
  pure [newMarker, LItemStart]

listItemFitter :: Level -> [Level] -> OrgParser [ListMarker]
listItemFitter level levels =
  case find (\(_, l) -> l.indent == level.indent) (zip [0 ..] levels) of
    Nothing -> fail "list item: invalid indentation"
    Just (levelsToDrop, lvl)
      | isJust level.number && isJust lvl.number ->
          let expectedNum = fmap (+ 1) lvl.number
           in if level.number == expectedNum
                then fitter levelsToDrop
                else
                  fail
                    $ "expected incremented list head "
                    <> (show . fromJust $ expectedNum)
                    <> " but got "
                    <> (show . fromJust $ level.number)
      | isNothing level.number && isNothing lvl.number -> fitter levelsToDrop
      | isJust lvl.number -> fail "expected ORDERED list item at this level"
      | otherwise -> fail "expected UNORDERED list item at this level"
  where
    fitter :: Int -> OrgParser [ListMarker]
    fitter levelsToDrop = do
      orgParserState <- get
      let markers = getEndingListMarkers levelsToDrop orgParserState.listLevels
      put
        orgParserState
          { listLevels = level : drop (levelsToDrop + 1) orgParserState.listLevels,
            listMarkers = drop (levelsToDrop * 2) orgParserState.listMarkers
          }
      pure $ markers <> [LItemEnd, LItemStart]

mathFragmentNumberedParser :: OrgParser MathFragment
mathFragmentNumberedParser = do
  orgParserState <- get
  let number = orgParserState.equationNumber + 1

  metaKVs <- metaParser
  _ <- lexeme $ string "\\begin{equation}"
  latex <-
    MP.someTill
      (MP.label "LaTeX standalone numbered math input" MP.anySingle)
      (string "\\end{equation}")

  modify (\s -> s {equationNumber = number})
  pure
    MathFragment
      { meta =
          insert "__equation_number" (T.pack $ show number) metaKVs,
        body = trimTrailingWhitespace $ T.pack latex
      }

trimTrailingWhitespace :: Text -> Text
trimTrailingWhitespace = T.dropWhileEnd (\c -> T.elem c " \n\r\t")

mathFragmentUnnumberedParser :: OrgParser MathFragment
mathFragmentUnnumberedParser = do
  orgParserState <- get
  metaKVs <- metaParser
  _ <- lexeme $ string "\\["
  latex <-
    MP.someTill
      (MP.label "LaTeX standalone unnumbered math input" MP.anySingle)
      (string "\\]")
  anonNumber <- case lookup "name" metaKVs of
    Just _ -> pure Nothing
    Nothing -> do
      let n = orgParserState.anonEquationNumber + 1
      modify (\s -> s {anonEquationNumber = n})
      pure $ Just n
  pure
    MathFragment
      { meta = case anonNumber of
          Just n ->
            insert "__equation_anon_number" (T.pack $ show n) metaKVs
          Nothing -> metaKVs,
        body = trimTrailingWhitespace $ T.pack latex
      }

mathFragmentStandaloneParser :: OrgParser Cell
mathFragmentStandaloneParser =
  CMathFragment
    <$> (MP.try mathFragmentNumberedParser <|> mathFragmentUnnumberedParser)

figureParser :: OrgParser Cell
figureParser = do
  orgParserState <- get
  when (orgParserState.listLevels /= [])
    $ fail "figures are not allowed inside list items"
  _ <- MP.try $ MP.lookAhead (metaParser >> fileLinkParser >> endOfCell)
  metaKVs <- metaParser
  when (lookup "type" metaKVs /= Just "figure")
    $ fail "type must be 'figure'"
  caption <- case lookup "caption" metaKVs of
    Nothing -> pure $ Prose []
    Just s -> case MP.runParser (runStateT captionParser orgParserState) "" s of
      Left err -> fail $ MP.errorBundlePretty err
      Right (prose, _orgParserState) -> pure prose
  link <- fileLinkParser
  let newFigureNumber = orgParserState.figureNumber + 1
      figureNumberMeta = ("__figure_number", T.show newFigureNumber)
  case link.target of
    LinkTargetOid {}
    LinkTargetURI {}
    LinkTargetFootnote {} -> fail "'file:...' required for link to figures"
    LinkTargetFilePath _ -> do
      modify (\s -> s {figureNumber = newFigureNumber})
      pure
        . CFigure
        $ Figure
          { meta =
              foldl'
                (\acc (k, v) -> insert k v acc)
                (delete "type" metaKVs)
                [figureNumberMeta],
            caption = caption,
            link = link
          }
  where
    endOfCell :: OrgParser ()
    endOfCell =
      MP.choice
        [ void . MP.try $ string "\n\n",
          char '\n' >> MP.eof
        ]

metaParser :: OrgParser (Map Text Text)
metaParser = do
  metaKVs <- many $ lexeme metaEntryParser
  let metaKVsMap = fromList metaKVs
  case lookup "name" metaKVsMap of
    Just cellName -> assertUniqueCellName cellName
    Nothing -> pure ()
  pure metaKVsMap

metaEntryParser :: OrgParser (Text, Text)
metaEntryParser = do
  mapM_
    MP.notFollowedBy
    $ map
      string
      [ "#+begin_",
        "#+print_bibliography:"
      ]
  _ <- string "#+"
  key <- MP.takeWhile1P (Just "(meta) keyword letter") isKeywordLetter
  _ <- MP.label "(meta) separator" $ string ": "
  _ <- many $ char ' '
  value <- MP.takeWhile1P (Just "(meta) value letter") (\c -> c /= '\n')
  pure (key, value)
  where
    isKeywordLetter c = or $ map ($ c) [isAsciiLower, (== '_')]

tableParser :: OrgParser Cell
tableParser = do
  orgParserState <- get
  _ <- MP.try $ MP.lookAhead (metaParser >> tableRowParser)
  metaKVs <- metaParser
  caption <- case lookup "caption" metaKVs of
    Nothing -> pure $ Prose []
    Just s -> case MP.runParser (runStateT captionParser orgParserState) "" s of
      Left err -> fail $ MP.errorBundlePretty err
      Right (prose, _orgParserState) -> pure prose
  rows <- MP.some $ tableLineParser <|> tableRowParser
  _ <- MP.optional tableFormulaParser
  let newTableNumber = orgParserState.tableNumber + 1
  modify (\s -> s {tableNumber = newTableNumber})
  pure
    . CTable
    $ Table
      { meta = insert "__table_number" (T.show newTableNumber) metaKVs,
        caption = caption,
        grid = fromList $ rowsToTuples rows
      }

tableFormulaParser :: OrgParser ()
tableFormulaParser = do
  _ <- string "#+"
  _ <- string "tblfm: " <|> string "TBLFM: "
  _ <- MP.takeWhile1P (Just "table formula") (/= '\n')
  _ <- char '\n'
  pure ()

captionParser :: OrgParser Prose
captionParser = do
  caption <- proseParser spaceConsumer
  failIfLeftoverActiveStyles
  pure caption
  where
    spaceConsumer :: OrgParser Text
    spaceConsumer = MP.takeWhile1P (Just "space") (== ' ')

tableLineParser :: OrgParser [Prose]
tableLineParser = do
  _ <- string "|-"
  _ <- MP.takeWhile1P (Just "table line characters") isTableLineChar
  _ <- char '\n'
  orgParserState <- get
  _ <-
    MP.label "indentation for next table line"
      $ MP.count orgParserState.continuationIndent (char ' ')
  pure [tableHorizontalLine]
  where
    isTableLineChar c = c == '-' || c == '+' || c == '|'

tableHorizontalLine :: Prose
tableHorizontalLine =
  Prose
    [ PTokenStyledText
        StyledText {body = "horizontal-line\NUL", style = styleMaskFrom []}
    ]

tableRowParser :: OrgParser [Prose]
tableRowParser = do
  _ <- char '|'
  _ <- spaces
  tableCells <- MP.some $ emptyTableCellParser <|> tableCellParser
  _ <- char '\n'
  orgParserState <- get
  _ <-
    MP.optional
      . MP.label "indentation for next table line"
      $ MP.count orgParserState.continuationIndent (char ' ')
  pure tableCells
  where
    emptyTableCellParser :: OrgParser Prose
    emptyTableCellParser = do
      _ <- char '|'
      _ <- MP.optional spaces
      pure $ Prose []
    spaces :: OrgParser Text
    spaces = MP.takeWhile1P (Just "table whitespace") (== ' ')

tableCellParser :: OrgParser Prose
tableCellParser = do
  prose <- onlyMarkingCharacterParser <|> proseParser spacesInsideProse
  _ <- MP.optional spaces
  _ <- char '|'
  _ <- MP.optional spaces
  pure $ compressProse prose
  where
    spaces :: OrgParser Text
    spaces = MP.takeWhile1P (Just "table whitespace") (== ' ')
    spacesInsideProse :: OrgParser Text
    spacesInsideProse = do
      _ <- MP.try $ MP.lookAhead (spaces >> tableCellContent)
      spaces
    tableCellContent :: OrgParser ()
    tableCellContent = void $ MP.noneOf (" \n|" :: String)
    onlyMarkingCharacterParser :: OrgParser Prose
    onlyMarkingCharacterParser = do
      c <- MP.satisfy (`elem` markingChars)
      _ <- MP.try . MP.lookAhead $ do
        _ <- MP.takeWhile1P (Just "table whitespace (marking char)") (== ' ')
        _ <- char '|'
        pure ()
      pure
        $ Prose
          [ PTokenStyledText
              StyledText {body = T.singleton c, style = styleMaskFrom []}
          ]

rowsToTuples :: [[Prose]] -> [((Int, Int), Prose)]
rowsToTuples rows = concatMap rowToTuples $ zip [1 ..] rows

rowToTuples :: (Int, [Prose]) -> [((Int, Int), Prose)]
rowToTuples (rowIndex, row)
  | null row = [((rowIndex, 1), Prose [])]
  | otherwise = fst $ foldl' f ([], 1) row
  where
    f :: ([((Int, Int), Prose)], Int) -> Prose -> ([((Int, Int), Prose)], Int)
    f (acc, colIndex) tableCell =
      ( ((rowIndex, colIndex), tableCell) : acc,
        colIndex + 1
      )

footnoteLinkParser :: OrgParser Link
footnoteLinkParser = do
  name <- footnoteReferenceParser
  orgParserState <- get
  displayName <- case lookup name orgParserState.footnoteNumbers of
    Nothing -> do
      let newFootnoteNumber = orgParserState.footnoteNumber + 1
          newFootnoteNumbers =
            insert name newFootnoteNumber orgParserState.footnoteNumbers
      modify
        ( \s ->
            s
              { footnoteNumber = newFootnoteNumber,
                footnoteNumbers = newFootnoteNumbers
              }
        )
      pure $ T.show newFootnoteNumber
    Just n -> do
      pure $ T.show n
  pure
    $ Link
      { target = LinkTargetFootnote name,
        name = [StyledText {body = displayName, style = styleMaskFrom []}]
      }

footnoteReferenceParser :: OrgParser Text
footnoteReferenceParser = do
  _ <- string "[fn:"
  name <- MP.takeWhile1P (Just "footnote name character") (/= ']')
  _ <- string "]"
  pure name

footnoteDefinitionParser :: OrgParser Cell
footnoteDefinitionParser = do
  name <- footnoteReferenceParser
  _ <- spaces
  (CParagraph prose) <- paragraphParser
  orgParserState <- get
  o <- MP.getOffset
  when
    ( (not orgParserState.prevCellIsFootnote)
        && (orgParserState.footnoteDefinitions /= M.empty)
    )
    $ do
      MP.setOffset o
      fail "cannot have more than one group of footnote definitions"
  modify
    ( \s ->
        s
          { footnoteDefinitions =
              insert name prose orgParserState.footnoteDefinitions
          }
    )
  pure $ CFootnote Footnote {name = name, body = prose}
  where
    spaces :: OrgParser Text
    spaces = MP.takeWhile1P (Just "space") (\c -> c == ' ')

reconcileFootnotes :: OrgParser (Map Text Int)
reconcileFootnotes = do
  orgParserState <- get
  let referenced = M.keysSet orgParserState.footnoteNumbers
      defined = M.keysSet orgParserState.footnoteDefinitions
      brokenLinks = Set.difference referenced defined
      unusedDefs = Set.difference defined referenced
      hasBrokenLinks = length brokenLinks /= 0
      hasUnusedDefs = length unusedDefs /= 0
      brokenLinksMsg = T.unpack . unwords $ toList brokenLinks
      unusedDefsMsg = T.unpack . unwords $ toList unusedDefs
  if
    | hasBrokenLinks -> fail $ "broken footnote links: " <> brokenLinksMsg
    | hasUnusedDefs -> fail $ "unused footnote definitions: " <> unusedDefsMsg
    | otherwise -> pure orgParserState.footnoteNumbers

validateOidText :: (MonadFail m) => Text -> m OID
validateOidText oidText = case fromText oidText of
  Nothing -> fail $ show oidText <> " is not a valid OID"
  Just oid -> pure oid

validateCslPath :: (MonadFail m) => Text -> m (Maybe FilePath)
validateCslPath maybeCslPath
  | T.isPrefixOf cslPrefix maybeCslPath =
      pure $ T.unpack <$> T.stripPrefix cslPrefix maybeCslPath
  | otherwise = fail $ show cslPrefix <> " prefix required for specifying CSL file"
  where
    cslPrefix :: Text
    cslPrefix = "csl "

collectValsFor :: Text -> [(Text, Text)] -> ([Text], [(Text, Text)])
collectValsFor key kvs =
  let otherKVs = filter (\(k, _) -> k /= key) kvs
      vals = reverse $ foldl' f [] kvs
   in (vals, otherKVs)
  where
    f :: [Text] -> (Text, Text) -> [Text]
    f acc (k, v)
      | k == key = v : acc
      | otherwise = acc

bibliographyLocationParser :: OrgParser Cell
bibliographyLocationParser = do
  _ <- string "#+print_bibliography:"
  orgParserState <- get
  when (orgParserState.cslPath == Nothing)
    $ fail "cannot print bibliography without '#+cite_export: csl <style>.csl' in top-level document metadata"
  when (orgParserState.bibPaths == [])
    $ fail "cannot print bibliography without '#+bibliography: <bib_entries>.json' in top-level document metadata"
  pure $ CBibliography ()

citationParser :: OrgParser Int
citationParser = do
  failIfVerbatimOrCode
  orgParserState <- get
  _ <- string "[cite"
  when (orgParserState.cslPath == Nothing)
    $ fail "cannot parse citations without '#+cite_export: csl <style>.csl' in top-level document metadata"
  when (orgParserState.bibPaths == [])
    $ fail "cannot parse citations without '#+bibliography: <bib_entries>.json' in top-level document metadata"
  modify (\s -> s {insideCitation = True})
  sv1 <- MP.optional $ char '/' >> MP.label "style variation 1" svParser
  sv2 <- MP.optional $ char '/' >> MP.label "style variation 2" svParser
  _ <- char ':'
  _ <- MP.optional intraCellWhitespaceParser
  citationPrefix <-
    MP.optional $ do
      _ <- MP.notFollowedBy pseudoCiteParser
      p <- citeAffixParser True
      _ <- MP.optional intraCellWhitespaceParser
      _ <- char ';'
      pure p
  cites <-
    MP.sepEndBy1
      citeParser
      (char ';' >> MP.optional intraCellWhitespaceParser)
  citationSuffix <-
    MP.optional $ do
      _ <- MP.notFollowedBy pseudoCiteParser
      p <- citeAffixParser True
      _ <- MP.optional intraCellWhitespaceParser
      pure p
  _ <- MP.optional intraCellWhitespaceParser
  _ <- char ']'
  let c =
        Citation
          { styleVariation = (T.pack <$> sv1, T.pack <$> sv2),
            cites = cites,
            prefix = fromMaybe (Prose []) citationPrefix,
            suffix = fromMaybe (Prose []) citationSuffix
          }
  modify
    ( \s ->
        s
          { insideCitation = False,
            citations = c : s.citations
          }
    )
  pure $ length orgParserState.citations
  where
    svParser :: OrgParser [Char]
    svParser =
      MP.some
        $ MP.satisfy (\c -> isAlphaNumChar c || c == '_' || c == '-')
    pseudoCiteParser = do
      prefix <-
        MP.optional $ do
          p <- citeAffixParser True
          _ <- intraCellWhitespaceParser
          pure p
      citeId <- citeIdParser
      pure (prefix, citeId)

collectCitations :: OrgParser [Citation]
collectCitations = do
  orgParserState <- get
  pure $ reverse orgParserState.citations

resetState :: OrgParser ()
resetState =
  modify $ \s ->
    s
      { OrgParserState.bibPaths = [],
        OrgParserState.citations = [],
        OrgParserState.cslPath = Nothing
      }

deleteRedundantKeys :: (Ord k) => Map k v -> [k] -> Map k v
deleteRedundantKeys = foldl' (flip M.delete)

failIfUnbalancedBrackets :: OrgParser ()
failIfUnbalancedBrackets = do
  orgParserState <- get
  when (orgParserState.bracketBalance /= 0) $ do
    fail "unbalanced brackets"

citeParser :: OrgParser Cite
citeParser = do
  _ <- MP.try . MP.lookAhead $ MP.optional citePrefixParser >> citeIdParser
  citePrefix <- MP.optional citePrefixParser
  citeId <- citeIdParser
  citeLocator <-
    MP.optional . MP.try $ do
      _ <- intraCellWhitespaceParser
      citeLocatorParser
  citeSuffix <-
    MP.optional $ do
      _ <- intraCellWhitespaceParser
      citeAffixParser False
  pure
    Cite
      { id = citeId,
        locator = citeLocator,
        prefix = fromMaybe (Prose []) citePrefix,
        suffix = fromMaybe (Prose []) citeSuffix
      }
  where
    citePrefixParser = do
      p <- citeAffixParser True
      _ <- intraCellWhitespaceParser
      pure p

citeIdParser :: OrgParser Text
citeIdParser = do
  _ <- char '@'
  citeId <-
    MP.takeWhile1P
      (Just "citeId")
      (\c -> isAlphaNumChar c || c `T.elem` "-.:?!`'/*@+|(){}<>&_^$#%~")
  pure citeId

citeLocatorParser :: OrgParser (Text, Text)
citeLocatorParser =
  MP.choice
    [ explicitLocator,
      implicitLocator
    ]
  where
    explicitLocator :: OrgParser (Text, Text)
    explicitLocator = do
      term <- citeLocatorTermParser
      _ <- intraCellWhitespaceParser
      coords <- citeLocatorCoordsParser
      pure (term, coords)
    implicitLocator :: OrgParser (Text, Text)
    implicitLocator = do
      coords <- citeLocatorCoordsParser
      pure ("page", coords)

citeLocatorTermParser :: OrgParser Text
citeLocatorTermParser =
  MP.choice $ map (MP.try . locatorTermParser) locatorTerms
  where
    locatorTermParser :: (Text, Text) -> OrgParser Text
    locatorTermParser (humanFriendlyTerm, term) = do
      _ <- string humanFriendlyTerm
      pure term
    locatorTerms :: [(Text, Text)]
    locatorTerms =
      [ ("books", "book"),
        ("book", "book"),
        ("bks.", "book"),
        ("bk.", "book"),
        ("chapters", "chapter"),
        ("chapter", "chapter"),
        ("chaps.", "chapter"),
        ("chap.", "chapter"),
        ("ch.", "chapter"),
        ("columns", "column"),
        ("column", "column"),
        ("cols.", "column"),
        ("col.", "column"),
        ("figures", "figure"),
        ("figure", "figure"),
        ("figs.", "figure"),
        ("fig.", "figure"),
        ("folios", "folio"),
        ("folio", "folio"),
        ("fols.", "folio"),
        ("fol.", "folio"),
        ("numbers", "number"),
        ("number", "number"),
        ("nos.", "number"),
        ("no.", "number"),
        ("lines", "line"),
        ("line", "line"),
        ("ll.", "line"),
        ("l.", "line"),
        ("notes", "note"),
        ("note", "note"),
        ("nn.", "note"),
        ("n.", "note"),
        ("opus", "opus"),
        ("opp.", "opus"),
        ("op.", "opus"),
        ("pages", "page"),
        ("page", "page"),
        ("pp.", "page"),
        ("p.", "page"),
        ("paragraphs", "paragraph"),
        ("paragraph", "paragraph"),
        ("paras.", "paragraph"),
        ("para.", "paragraph"),
        ("parts", "part"),
        ("part", "part"),
        ("pts.", "part"),
        ("pt.", "part"),
        ("sections", "section"),
        ("section", "section"),
        ("secs.", "section"),
        ("sec.", "section"),
        ("sub verbo", "sub verbo"),
        ("s.vv.", "sub verbo"),
        ("s.v.", "sub verbo"),
        ("verses", "verse"),
        ("verse", "verse"),
        ("vv.", "verse"),
        ("v.", "verse"),
        ("volumes", "volume"),
        ("volume", "volume"),
        ("vols.", "volume"),
        ("vol.", "volume")
      ]

citeLocatorCoordsParser :: OrgParser Text
citeLocatorCoordsParser = do
  coords <-
    MP.sepBy1
      ( MP.choice
          [ MP.try citeLocatorCoordRangeParser,
            citeLocatorCoordParser
          ]
      )
      (char ',' >> intraCellWhitespaceParser)
  pure $ T.intercalate ", " coords

citeLocatorCoordParser :: OrgParser Text
citeLocatorCoordParser = do
  numCoord <- MP.sepBy1 numParser $ char '.'
  pure $ T.intercalate "." numCoord
  where
    numParser :: OrgParser Text
    numParser = MP.takeWhile1P (Just "locatorCoordNum") isDigitChar

citeLocatorCoordRangeParser :: OrgParser Text
citeLocatorCoordRangeParser = do
  n1 <- citeLocatorCoordParser
  _ <- char '-'
  n2 <- citeLocatorCoordParser
  pure $ T.concat [n1, "-", n2]

citeAffixParser :: Bool -> OrgParser Prose
citeAffixParser isPrefix = do
  modify (\s -> s {insideCitePrefix = isPrefix})
  orgParserState <- get
  prose <- proseParser $ proseSpaceConsumer orgParserState.continuationIndent
  modify (\s -> s {insideCitePrefix = False})
  failIfLeftoverActiveStyles
  failIfUnbalancedBrackets
  pure $ compressProse prose

isNumericText :: Text -> Bool
isNumericText input = case MP.runParser
  (runStateT isNumericTextParser defaultOrgParserState)
  ""
  input of
  Left _ -> False
  Right (b, _) -> b

isNumericTextParser :: OrgParser Bool
isNumericTextParser = do
  _ <- MP.optional sign
  _ <- MP.optional $ MP.satisfy isCurrencySymbol
  _ <- MP.optional sign
  s <- MP.some $ MP.satisfy isDigit <|> MP.satisfy (`elem` ['.', ',', ' '])
  MP.eof
  let numberFound = (> 0) . length $ filter isDigit s
  pure numberFound
  where
    isCurrencySymbol c = generalCategory c == CurrencySymbol
    sign :: OrgParser Char
    sign = MP.satisfy (`elem` ['-', '+'])

isPunctuation :: Char -> Bool
isPunctuation = not . isAlphaNumChar

isAlphaNumChar :: Char -> Bool
isAlphaNumChar c = isAlphaChar c || isDigitChar c

isAlphaChar :: Char -> Bool
isAlphaChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

intraCellWhitespaceParser :: OrgParser [Char]
intraCellWhitespaceParser =
  MP.label "intra-cell whitespace"
    $ MP.some
    $ MP.choice
      [ MP.satisfy (\c -> c == ' '),
        do
          MP.notFollowedBy cellBoundary
          char '\n'
      ]
