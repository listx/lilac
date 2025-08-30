module Lilac.ParseSpec (spec) where

import Citeproc
  ( CiteprocOutput,
    addDisplay,
    addFontStyle,
    addFontVariant,
    addFontWeight,
    addHyperlink,
    addQuotes,
    addTextCase,
    addTextDecoration,
    addVerticalAlign,
    dropTextWhile,
    dropTextWhileEnd,
    fromText,
    inNote,
    localizeQuotes,
    mapText,
    movePunctuationInsideQuotes,
    toText,
  )
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Lilac.Parse qualified as L
import Lilac.Types
  ( BToken (BTokenLink, BTokenString),
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
    Citation (Citation, cites, noteNumber, prefix, styleVariation, suffix),
    Cite (Cite, id, locator, prefix, suffix),
    Figure (Figure, caption, link, meta),
    Footnote (Footnote, body, name),
    Heading (Heading, body, level, meta, section),
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
    OrgParserConf (maxHeadingLevel),
    OrgParserState
      ( anonBlockNumber,
        anonEquationNumber,
        cellNames,
        docOid,
        equationNumber,
        figureNumber,
        footnoteDefinitions,
        footnoteNumber,
        footnoteNumbers,
        headingLevel,
        headingOidStack,
        headingSection,
        orgParserConf,
        prevCellIsFootnote,
        tableNumber
      ),
    PToken (PTokenCitation, PTokenLink, PTokenMathFragment, PTokenStyledText),
    Prose (Prose),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (StyledText, body, style),
    Table (Table, caption, grid, meta),
    defaultOrgParserState,
    fromWords,
    nilOid,
    nilOidOrgDoc,
    styleMaskFrom,
  )
import Lilac.Util qualified as LU
import Network.URI qualified as N
import Relude
  ( Bool (True),
    FilePath,
    Identity,
    Int,
    Maybe (Just, Nothing),
    String,
    Text,
    Type,
    forM_,
    fromList,
    otherwise,
    repeat,
    runStateT,
    show,
    take,
    ($),
    (+),
    (.),
    (<>),
    (==),
  )
import Test.Hspec (Spec, before, describe, it, shouldBe)
import Test.Hspec.Megaparsec
  ( ET,
    elabel,
    err,
    errFancy,
    etoks,
    fancy,
    shouldFailOn,
    shouldFailWith,
    shouldParse,
    shouldSucceedOn,
    utoks,
  )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char)

spec :: Spec
spec = parseSpec

type Parser :: Type -> Type
type Parser = MP.ParsecT Void Text Identity

p :: FilePath -> OrgDoc -> Spec
p path expected = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path check
  where
    check input
      | expected.oid == nilOid =
          action nilOidOrgDoc input defaultOrgParserState
      | otherwise = action "" input defaultOrgParserState
    action inputPrefix input expectedState =
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        (inputPrefix <> input)
        `shouldParse` (expected, expectedState)

ps :: FilePath -> OrgDoc -> OrgParserState -> Spec
ps path expected expectedState = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path check
  where
    check input
      | expected.oid == nilOid =
          action nilOidOrgDoc input expectedState
      | otherwise = action "" input expectedState
    action inputPrefix input expectedState' =
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        (inputPrefix <> input)
        `shouldParse` (expected, expectedState')

pf :: FilePath -> String -> Int -> Spec
pf path errMsg pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        (inputPrefix <> input)
        `shouldFailWith` (errFancy newPos . fancy $ MP.ErrorFail errMsg)
  where
    inputPrefix = nilOidOrgDoc
    newPos = pos + T.length inputPrefix

pfm :: FilePath -> ET Text -> Int -> Spec
pfm path errorTrivial pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        (inputPrefix <> input)
        `shouldFailWith` (err newPos errorTrivial)
  where
    inputPrefix = nilOidOrgDoc
    newPos = pos + T.length inputPrefix

pfSimple :: FilePath -> Spec
pfSimple path = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        `shouldFailOn` (inputPrefix <> input)
  where
    inputPrefix = nilOidOrgDoc

plainParagraph :: Text -> Cell
plainParagraph body =
  CParagraph
    $ Prose
      [ PTokenStyledText
          StyledText
            { body = body,
              style = styleMaskFrom []
            }
      ]

plainHeading :: Int -> [Int] -> Cell
plainHeading level section =
  CHeading
    Heading
      { level = level,
        section = section,
        body =
          Prose
            [ PTokenStyledText
                StyledText
                  { body = T.pack $ show level,
                    style = styleMaskFrom []
                  }
            ],
        meta = fromList []
      }

nilOrgDoc :: [Cell] -> OrgDoc
nilOrgDoc cells =
  OrgDoc
    { oid = nilOid,
      cells = cells,
      footnoteNumbers = M.empty,
      cslPath = Nothing,
      bibPaths = [],
      citations = [],
      meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
    }

checkTextToProseRoundTrip :: String -> [(Text, Prose)] -> Spec
checkTextToProseRoundTrip msg testCases = do
  it msg $ do
    forM_ testCases $ \(text, prose) -> do
      fromText text `shouldBe` prose
      toText prose `shouldBe` text

parseSpec :: Spec
parseSpec = do
  it "empty input" $ do
    MP.runParser
      (runStateT L.orgDocParser defaultOrgParserState)
      "empty input"
      nilOidOrgDoc
      `shouldParse` (L.defaultOrgDoc nilOid [], defaultOrgParserState)

  before (LU.readFrom "" "base/lilac.org") $ do
    it "self-parse" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        "base/lilac.org"
        `shouldSucceedOn` input

  p
    "0001-blankline"
    (L.defaultOrgDoc nilOid [])

  p
    "0001-comments-only"
    (L.defaultOrgDoc nilOid [])

  ps
    "0001-blankline"
    (L.defaultOrgDoc nilOid [])
    defaultOrgParserState

  pfm
    "0002-doc-metadata-missing-value"
    (utoks ":\n" <> elabel "(doc meta) separator" <> elabel "(doc meta) keyword letter")
    7

  p
    "0002-doc-metadata-whitespace-before-value"
    $ L.defaultOrgDoc
      nilOid
      [ ("title", "Parse me without leading whitespace")
      ]

  p
    "0002-doc-metadata"
    $ L.defaultOrgDoc
      nilOid
      [ ("author", "A. U. Thor"),
        ("auto_tangle", "t"),
        ("property", "header-args+ :noweb no-export"),
        ("title", "An Org Document")
      ]

  p
    "0003-doc-metadata-with-comments"
    $ L.defaultOrgDoc
      nilOid
      [ ("author", "A. U. Thor"),
        ("auto_tangle", "t"),
        ("property", "header-args+ :noweb no-export"),
        ("title", "An Org Document")
      ]

  ps
    "0002-doc-metadata-properties-drawer"
    ( L.defaultOrgDoc
        (fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f)
        [ ("ID", "7af1aa3e-41ca-4810-a001-2fdf10768f2f")
        ]
    )
    defaultOrgParserState
      { docOid = fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f
      }

  ps
    "0002-doc-metadata-properties-drawer-combine"
    ( L.defaultOrgDoc
        (fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f)
        [ ("ID", "7af1aa3e-41ca-4810-a001-2fdf10768f2f"),
          ("author", "A. U. Thor"),
          ("property", "header-args+ :noweb no-export"),
          ("title", "An Org Document")
        ]
    )
    defaultOrgParserState
      { docOid = fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f
      }

  before (LU.readFrom "test/Lilac/ParseSpec/" "0002-doc-metadata-properties-drawer-clash") $ do
    it "0002-doc-metadata-properties-drawer-clash" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        "0002-doc-metadata-properties-drawer-clash"
        input
        `shouldFailWith` (errFancy 181 . fancy $ MP.ErrorFail "conflicting keys in the top-level PROPERTIES drawer and document metadata key/values: [\"foo\"]")

  pf
    "0004-headings"
    "found 7 asterisks found for this heading, but lilac_maxHeadingLevel = 6"
    46

  ps
    "0004-headings-20"
    ( nilOrgDoc
        [ plainHeading 1 [1],
          plainHeading 2 [1, 1],
          plainHeading 3 [1, 1, 1],
          plainHeading 4 [1, 1, 1, 1],
          plainHeading 5 [1, 1, 1, 1, 1],
          plainHeading 6 [1, 1, 1, 1, 1, 1],
          plainHeading 7 [1, 1, 1, 1, 1, 1, 1],
          plainHeading 8 [1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 9 [1, 1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 10 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 11 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 12 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        ]
    )
    defaultOrgParserState
      { headingOidStack = take 12 $ repeat Nothing,
        headingLevel = 12,
        headingSection = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        orgParserConf = L.defaultOrgParserConf {maxHeadingLevel = 20}
      }

  ps
    "0004-headings-unlimited"
    ( nilOrgDoc
        [ plainHeading 1 [1],
          plainHeading 2 [1, 1],
          plainHeading 3 [1, 1, 1],
          plainHeading 4 [1, 1, 1, 1],
          plainHeading 5 [1, 1, 1, 1, 1],
          plainHeading 6 [1, 1, 1, 1, 1, 1],
          plainHeading 7 [1, 1, 1, 1, 1, 1, 1],
          plainHeading 8 [1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 9 [1, 1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 10 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 11 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
          plainHeading 12 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        ]
    )
    defaultOrgParserState
      { headingOidStack = take 12 $ repeat Nothing,
        headingLevel = 12,
        headingSection = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        orgParserConf = L.defaultOrgParserConf {maxHeadingLevel = 0}
      }

  pf
    "0004-headings-limited"
    "found 4 asterisks found for this heading, but lilac_maxHeadingLevel = 3"
    46

  pf
    "0004-headings-prohibited"
    "headings prohibited because lilac_maxHeadingLevel < 0"
    29

  pf
    "0004-headings-cannot-skip-level"
    "headings may not skip levels (expected to see level 1 or 2, not 4)"
    8

  describe "genHeadingSection" $ do
    it "gets the base case" $ do
      L.genHeadingSection 2 [] `shouldBe` [1, 0]
      L.genHeadingSection 1 [] `shouldBe` [1]
      L.genHeadingSection 0 [] `shouldBe` [1]
      L.genHeadingSection (-1) [] `shouldBe` [1]
    it "bumps the last level if the nesting level matches" $ do
      L.genHeadingSection 1 [1] `shouldBe` [2]
      L.genHeadingSection 2 [1, 1] `shouldBe` [2, 1]
      L.genHeadingSection 3 [1, 1, 1] `shouldBe` [2, 1, 1]
    it "truncates, then increments the (newst) deepest level if we unnest" $ do
      L.genHeadingSection 1 [3, 5] `shouldBe` [6]
      L.genHeadingSection 3 [3, 5, 6, 7] `shouldBe` [6, 6, 7]
    it "grows the section if we nest deeper" $ do
      L.genHeadingSection 3 [2, 2] `shouldBe` [1, 2, 2]
      L.genHeadingSection 5 [2, 2] `shouldBe` [1, 0, 0, 2, 2]

  describe "genHeadingOidStack" $ do
    it "replaces the head if the nesting level matches (no heading yet)" $ do
      L.genHeadingOidStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        [Nothing]
        `shouldBe` [Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa]
    it "replaces the head if the nesting level matches (one heading exists)" $ do
      L.genHeadingOidStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        [Just $ fromWords 0x7777777777777777 0x7777777777777777]
        `shouldBe` [Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa]
    it "adds a new head if the heading nests (and this heading has an ID)" $ do
      L.genHeadingOidStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        2
        [Nothing]
        `shouldBe` [ Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa,
                     Nothing
                   ]
    it "adds a new head if the heading nests (and this heading does not have an ID)" $ do
      L.genHeadingOidStack
        Nothing
        2
        [Nothing]
        `shouldBe` [ Nothing,
                     Nothing
                   ]
    it "truncates if we unnest (difference of 1 level)" $ do
      L.genHeadingOidStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        [Just $ fromWords 0x7777777777777777 0x7777777777777777, Nothing]
        `shouldBe` [Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa]
    it "truncates if we unnest (multiple levels)" $ do
      L.genHeadingOidStack
        Nothing
        1
        [Nothing, Nothing, Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa]
        `shouldBe` [Nothing]
    it "truncates if we unnest (multiple levels, new heading has ID)" $ do
      L.genHeadingOidStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        [Nothing, Nothing, Just $ fromWords 0x7777777777777777 0x7777777777777777]
        `shouldBe` [Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa]

  ps
    "0004-properties-drawer-empty"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body =
                  Prose
                    [ PTokenStyledText
                        StyledText {body = "foo", style = styleMaskFrom []}
                    ],
                meta = fromList []
              }
        ]
    )
    defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSection = [1]
      }

  ps
    "0004-properties-drawer-not-empty"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body =
                  Prose
                    [ PTokenStyledText
                        StyledText {body = "foo", style = styleMaskFrom []}
                    ],
                meta =
                  fromList
                    [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6")
                    ]
              }
        ]
    )
    defaultOrgParserState
      { headingOidStack = [Just $ fromWords 0xb949ea2ee6d1453d 0x93525e85a82cd4e6],
        headingLevel = 1,
        headingSection = [1]
      }

  ps
    "0004-properties-drawer-not-empty-with-comments"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body =
                  Prose
                    [ PTokenStyledText
                        StyledText {body = "foo", style = styleMaskFrom []}
                    ],
                meta =
                  fromList
                    [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6")
                    ]
              }
        ]
    )
    defaultOrgParserState
      { headingOidStack = [Just $ fromWords 0xb949ea2ee6d1453d 0x93525e85a82cd4e6],
        headingLevel = 1,
        headingSection = [1]
      }

  pf
    "0005-paragraph-indentation-mismatch-initial-indent-invalid-between-indent-levels"
    "indentation does not fit into existing levels"
    36

  pf
    "0005-paragraph-indentation-mismatch-initial-indent-invalid"
    "indentation does not fit into existing levels"
    21

  pfm
    "0005-paragraph-indentation-mismatch-over-list-body"
    (utoks " ")
    14

  pfm
    "0005-paragraph-indentation-mismatch-over"
    (utoks " ")
    4

  pfm
    "0005-paragraph-indentation-mismatch-under-list-body"
    (utoks "b" <> etoks " ")
    12

  p
    "0005-prose-paragraphs"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "a b c d e ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "f g h i j", style = styleMaskFrom [Bold]}
            ],
        CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "x y z foo bar", style = styleMaskFrom []}
            ],
        CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText
                  { body = "multiple lines with underscore",
                    style = styleMaskFrom [Underline]
                  }
            ]
      ]

  it "accepts empty input for \"notFollowedBy (char ' ')\"" $ do
    MP.parse
      (MP.notFollowedBy (char ' ') :: Parser ())
      "empty input"
      `shouldSucceedOn` ""

  p
    "0005-styled-strings-bold"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText
                  { body = "foo ",
                    style = styleMaskFrom []
                  },
              PTokenStyledText
                StyledText
                  { body = "bar",
                    style = styleMaskFrom [Bold]
                  },
              PTokenStyledText
                StyledText
                  { body = " baz",
                    style = styleMaskFrom []
                  }
            ]
      ]

  p
    "0005-styled-strings-complex-punctuation"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "(foo ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "bar", style = styleMaskFrom [Verbatim]},
              PTokenStyledText
                StyledText {body = "). What's up? and ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "that!", style = styleMaskFrom [Bold]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "(italic)", style = styleMaskFrom [Italic]},
              PTokenStyledText
                StyledText {body = ". It's #1!", style = styleMaskFrom []}
            ]
      ]

  p
    "0005-styled-strings-complex"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "foo ", style = styleMaskFrom [Bold]},
              PTokenStyledText
                StyledText {body = "bar ", style = styleMaskFrom [Bold, Italic]},
              PTokenStyledText
                StyledText
                  { body = "underlined bold and italic",
                    style = styleMaskFrom [Bold, Italic, Underline]
                  },
              PTokenStyledText
                StyledText {body = " baz", style = styleMaskFrom [Bold]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "verbatim  text", style = styleMaskFrom [Verbatim]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "x  y  z", style = styleMaskFrom [Code]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText
                  { body = "bold",
                    style = styleMaskFrom [Bold]
                  }
            ]
      ]

  p
    "0005-styled-strings-compression"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "a b c d e ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "f g h i j", style = styleMaskFrom [Bold]}
            ]
      ]

  p
    "0005-styled-strings-ignore-marker-inside-word"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "foo and/or 1/2 bar \\ a\\b", style = styleMaskFrom [Italic]}
            ]
      ]

  pf
    "0005-styled-strings-leftover-active-styles"
    "leftover active styles: [Bold]"
    15

  ps
    "0005-styled-strings-messy-boundaries"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body =
                  Prose
                    [ PTokenStyledText
                        StyledText {body = "foo ", style = styleMaskFrom [Bold]},
                      PTokenStyledText
                        StyledText {body = "bar baz a", style = styleMaskFrom []},
                      PTokenStyledText
                        StyledText {body = "b", style = styleMaskFrom [Bold]},
                      PTokenStyledText
                        StyledText {body = "c (", style = styleMaskFrom []},
                      PTokenStyledText
                        StyledText {body = "ok", style = styleMaskFrom [Bold]},
                      PTokenStyledText
                        StyledText {body = ") yes", style = styleMaskFrom []}
                    ],
                meta = fromList []
              }
        ]
    )
    defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSection = [1]
      }

  p
    "0005-styled-strings-no-style"
    $ nilOrgDoc
      [ plainParagraph "foo"
      ]

  p
    "0005-styled-strings-verbatim-code-ignore-immediate-bold"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "*a", style = styleMaskFrom [Verbatim]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "b*", style = styleMaskFrom [Verbatim]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "*c", style = styleMaskFrom [Code]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "d*", style = styleMaskFrom [Code]}
            ]
      ]

  p
    "0005-styled-strings-verbatim-precedence"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "[[foo", style = styleMaskFrom [Verbatim]}
            ],
        CParagraph
          $ Prose
            [ PTokenLink
                Link
                  { target =
                      LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "hello"
                          },
                    name =
                      [ StyledText
                          { body = "hello",
                            style = styleMaskFrom []
                          }
                      ]
                  },
              PTokenStyledText
                StyledText {body = " there", style = styleMaskFrom []}
            ],
        CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "\\(1 + 1 ", style = styleMaskFrom [Verbatim]},
              PTokenStyledText
                StyledText {body = " ", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "\\(2 * 2\\)", style = styleMaskFrom [Code]}
            ],
        CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "[[bar]]", style = styleMaskFrom [Code]}
            ]
      ]

  p
    "0005-styled-strings-verbatim"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText {body = "(", style = styleMaskFrom []},
              PTokenStyledText
                StyledText {body = "import ./.", style = styleMaskFrom [Verbatim]},
              PTokenStyledText
                StyledText {body = ") hello", style = styleMaskFrom []}
            ]
      ]

  p
    "0006-links-inside-parentheses"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText
                  { body = "(",
                    style = styleMaskFrom []
                  },
              PTokenLink
                Link
                  { target = LinkTargetFilePath "foo/bar",
                    name =
                      [ StyledText
                          { body = "foo/bar",
                            style = styleMaskFrom []
                          }
                      ]
                  },
              PTokenStyledText
                StyledText
                  { body = ")",
                    style = styleMaskFrom []
                  }
            ]
      ]

  p
    "0006-links-nonlink"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText
                  { body = "[hi]",
                    style = styleMaskFrom []
                  }
            ]
      ]

  p
    "0006-links-raw"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ PTokenStyledText
                StyledText
                  { body = "See ",
                    style = styleMaskFrom []
                  },
              PTokenLink
                Link
                  { target =
                      LinkTargetURI
                        N.URI
                          { N.uriScheme = "https:",
                            N.uriAuthority =
                              Just
                                N.URIAuth
                                  { N.uriUserInfo = "",
                                    N.uriRegName = "foo.org",
                                    N.uriPort = ""
                                  },
                            N.uriPath = "",
                            N.uriQuery = "",
                            N.uriFragment = ""
                          },
                    name =
                      [ StyledText
                          { body = "https://foo.org",
                            style = styleMaskFrom []
                          }
                      ]
                  },
              PTokenStyledText
                StyledText
                  { body = ", or ",
                    style = styleMaskFrom []
                  },
              PTokenLink
                Link
                  { target =
                      LinkTargetURI
                        N.URI
                          { N.uriScheme = "https:",
                            N.uriAuthority =
                              Just
                                N.URIAuth
                                  { N.uriUserInfo = "",
                                    N.uriRegName = "bar.net",
                                    N.uriPort = ""
                                  },
                            N.uriPath = "",
                            N.uriQuery = "",
                            N.uriFragment = ""
                          },
                    name =
                      [ StyledText
                          { body = "https://bar.net",
                            style = styleMaskFrom []
                          }
                      ]
                  },
              PTokenStyledText
                StyledText
                  { body = ". Try ",
                    style = styleMaskFrom []
                  },
              PTokenLink
                Link
                  { target =
                      LinkTargetURI
                        N.URI
                          { N.uriScheme = "http:",
                            N.uriAuthority =
                              Just
                                N.URIAuth
                                  { N.uriUserInfo = "",
                                    N.uriRegName = "localhost",
                                    N.uriPort = ":3000"
                                  },
                            N.uriPath = "",
                            N.uriQuery = "",
                            N.uriFragment = ""
                          },
                    name =
                      [ StyledText
                          { body = "http://localhost:3000",
                            style = styleMaskFrom []
                          }
                      ]
                  },
              PTokenStyledText
                StyledText
                  { body = "? Go to ",
                    style = styleMaskFrom []
                  },
              PTokenLink
                Link
                  { target =
                      LinkTargetURI
                        N.URI
                          { N.uriScheme = "ftp:",
                            N.uriAuthority =
                              Just
                                N.URIAuth
                                  { N.uriUserInfo = "",
                                    N.uriRegName = "10.0.0.1",
                                    N.uriPort = ""
                                  },
                            N.uriPath = "",
                            N.uriQuery = "",
                            N.uriFragment = ""
                          },
                    name =
                      [ StyledText
                          { body = "ftp://10.0.0.1",
                            style = styleMaskFrom []
                          }
                      ]
                  },
              PTokenStyledText
                StyledText
                  { body = "!",
                    style = styleMaskFrom []
                  }
            ]
      ]

  ps
    "0006-links"
    ( nilOrgDoc
        [ CParagraph
            $ Prose
              [ PTokenStyledText
                  StyledText
                    { body = "foo ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "link-target-only"
                            },
                      name =
                        [ StyledText
                            { body = "link-target-only",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              ],
          CHeading
            Heading
              { level = 1,
                section = [1],
                body =
                  Prose
                    [ PTokenLink
                        Link
                          { target =
                              LinkTargetOid
                                LinkOid
                                  { oid = nilOid,
                                    name = "link-target"
                                  },
                            name =
                              [ StyledText
                                  { body = "link-name",
                                    style = styleMaskFrom []
                                  }
                              ]
                          }
                    ],
                meta = fromList []
              },
          CParagraph
            $ Prose
              [ PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "a"
                            },
                      name =
                        [ StyledText
                            { body = "a",
                              style = styleMaskFrom []
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " b",
                      style = styleMaskFrom []
                    }
              ]
        ]
    )
    defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSection = [1]
      }

  p
    "0006-links-web-protocols"
    ( nilOrgDoc
        [ CParagraph
            $ Prose
              [ PTokenLink
                  Link
                    { target =
                        LinkTargetURI
                          N.URI
                            { N.uriScheme = "http:",
                              N.uriAuthority =
                                Just
                                  N.URIAuth
                                    { N.uriUserInfo = "",
                                      N.uriRegName = "foo.org",
                                      N.uriPort = ""
                                    },
                              N.uriPath = "",
                              N.uriQuery = "",
                              N.uriFragment = ""
                            },
                      name =
                        [ StyledText
                            { body = "http://foo.org",
                              style = styleMaskFrom []
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetURI
                          N.URI
                            { N.uriScheme = "https:",
                              N.uriAuthority =
                                Just
                                  N.URIAuth
                                    { N.uriUserInfo = "",
                                      N.uriRegName = "foo.org",
                                      N.uriPort = ""
                                    },
                              N.uriPath = "",
                              N.uriQuery = "",
                              N.uriFragment = ""
                            },
                      name =
                        [ StyledText
                            { body = "https://foo.org",
                              style = styleMaskFrom []
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetURI
                          N.URI
                            { N.uriScheme = "https:",
                              N.uriAuthority =
                                Just
                                  N.URIAuth
                                    { N.uriUserInfo = "",
                                      N.uriRegName = "foo.org",
                                      N.uriPort = ""
                                    },
                              N.uriPath = "",
                              N.uriQuery = "",
                              N.uriFragment = ""
                            },
                      name =
                        [ StyledText
                            { body = "hello",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              ]
        ]
    )

  p
    "0006-links-oids"
    ( nilOrgDoc
        [ CParagraph
            $ Prose
              [ PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "id:442c807f-c9d9-45a7-88bf-ffffffffffff",
                              style = styleMaskFrom []
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "bar",
                              style = styleMaskFrom []
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "bar",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = "[]",
                              style = styleMaskFrom [Verbatim]
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "bar",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = "ok",
                              style = styleMaskFrom []
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = "foo"
                            },
                      name =
                        [ StyledText
                            { body = "abc",
                              style = styleMaskFrom []
                            }
                        ]
                    },
                PTokenStyledText
                  StyledText
                    { body = " ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "foo"
                            },
                      name =
                        [ StyledText
                            { body = "abc",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              ],
          CListItem
            [ LUnorderedStart,
              LItemStart
            ],
          CParagraph
            $ Prose
              [ PTokenStyledText
                  StyledText
                    { body = "link name across list indentation ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { name =
                        [ StyledText
                            { body = "italic",
                              style = styleMaskFrom [Italic]
                            },
                          StyledText
                            { body = " ",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = "def",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = "ghi]",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = styleMaskFrom []
                            },
                          StyledText
                            { body = "!",
                              style = styleMaskFrom []
                            }
                        ],
                      target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "foo"
                            }
                    }
              ],
          CListItem
            [ LItemEnd,
              LItemStart
            ],
          CParagraph
            $ Prose
              [ PTokenStyledText
                  StyledText
                    { body = "link target with spaces ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { name =
                        [ StyledText
                            { body = "foo bar baz",
                              style = styleMaskFrom []
                            }
                        ],
                      target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "foo bar baz"
                            }
                    }
              ],
          CListItem
            [ LItemEnd,
              LItemStart
            ],
          CParagraph
            $ Prose
              [ PTokenStyledText
                  StyledText
                    { body = "oid with spaces ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { name =
                        [ StyledText
                            { body = "a b c",
                              style = styleMaskFrom []
                            }
                        ],
                      target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = "a b c"
                            }
                    }
              ],
          CListItem
            [ LItemEnd,
              LUnorderedEnd
            ]
        ]
    )

  p
    "0006-links-oids-lenient-whitespace"
    ( nilOrgDoc
        [ CParagraph
            $ Prose
              [ PTokenStyledText
                  StyledText
                    { body = "Hello ",
                      style = styleMaskFrom []
                    },
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = "target name with newline and space is still treated as space"
                            },
                      name =
                        [ StyledText
                            { body = "foo",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              ]
        ]
    )

  pf
    "0007-block-src-without-lang"
    "block type src requires a language specifier"
    11

  ps
    "0007-blocks-empty"
    ( nilOrgDoc
        [ CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__anon_block_number", "1"),
                      ("__block_type", "src"),
                      ("__src_language", "haskell")
                    ],
                body = []
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "example"),
                      ("name", "foo")
                    ],
                body = []
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__anon_block_number", "2"),
                      ("__block_type", "quote")
                    ],
                body = []
              }
        ]
    )
    defaultOrgParserState
      { cellNames = Set.fromList ["foo"],
        anonBlockNumber = 2
      }

  ps
    "0007-blocks-ending"
    ( nilOrgDoc
        [ CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "sh"),
                      ("name", "empty")
                    ],
                body = []
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "sh"),
                      ("name", "end not in beginning of line")
                    ],
                body = [BTokenString " #+end_src"]
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "sh"),
                      ("name", "end has no preceding blank line")
                    ],
                body = [BTokenString "foo"]
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "sh"),
                      ("name", "end has preceding blank line")
                    ],
                body = [BTokenString "foo\n"]
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "sh"),
                      ("name", "end has preceding newline (multiple)")
                    ],
                body = [BTokenString "foo\n\n"]
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "sh"),
                      ("name", "ignore #+end_example because this is a src block")
                    ],
                body = [BTokenString "#+end_example"]
              }
        ]
    )
    defaultOrgParserState
      { cellNames =
          Set.fromList
            [ "empty",
              "end has no preceding blank line",
              "end has preceding blank line",
              "end has preceding newline (multiple)",
              "end not in beginning of line",
              "ignore #+end_example because this is a src block"
            ]
      }

  ps
    "0007-blocks-noweb-off"
    ( nilOrgDoc
        [ CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__anon_block_number", "1"),
                      ("__block_type", "src"),
                      ("__src_language", "sh"),
                      ("noweb", "off")
                    ],
                body =
                  [ BTokenString "<<foo>>\n\n<<quux\n>>\necho <<bar>> x y z"
                  ]
              }
        ]
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  ps
    "0007-blocks-noweb"
    ( nilOrgDoc
        [ CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__anon_block_number", "1"),
                      ("__block_type", "src"),
                      ("__src_language", "sh")
                    ],
                body =
                  [ BTokenLink
                      Link
                        { target =
                            LinkTargetOid
                              LinkOid
                                { oid = nilOid,
                                  name = "foo"
                                },
                          name =
                            [ StyledText
                                { body = "foo",
                                  style = styleMaskFrom []
                                }
                            ]
                        },
                    BTokenString "\n\n<<quux\n>>\necho ",
                    BTokenLink
                      Link
                        { target =
                            LinkTargetOid
                              LinkOid
                                { oid = nilOid,
                                  name = "bar"
                                },
                          name =
                            [ StyledText
                                { body = "bar",
                                  style = styleMaskFrom []
                                }
                            ]
                        },
                    BTokenString " x y z"
                  ]
              }
        ]
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  ps
    "0007-blocks-noweb-intraline"
    ( nilOrgDoc
        [ CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__anon_block_number", "1"),
                      ("__block_type", "src"),
                      ("__src_language", "sh")
                    ],
                body =
                  [ BTokenString "(",
                    BTokenLink
                      Link
                        { target =
                            LinkTargetOid
                              LinkOid
                                { oid = nilOid,
                                  name = "foo"
                                },
                          name =
                            [ StyledText
                                { body = "foo",
                                  style = styleMaskFrom []
                                }
                            ]
                        },
                    BTokenString ") and \"",
                    BTokenLink
                      Link
                        { target =
                            LinkTargetOid
                              LinkOid
                                { oid = nilOid,
                                  name = "bar"
                                },
                          name =
                            [ StyledText
                                { body = "bar",
                                  style = styleMaskFrom []
                                }
                            ]
                        },
                    BTokenString "\" and <",
                    BTokenLink
                      Link
                        { target =
                            LinkTargetOid
                              LinkOid
                                { oid = nilOid,
                                  name = "ok"
                                },
                          name =
                            [ StyledText
                                { body = "ok",
                                  style = styleMaskFrom []
                                }
                            ]
                        },
                    BTokenString ">"
                  ]
              }
        ]
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  pf
    "0007-blocks-duplicate-names"
    "cell name \"foo\" is not unique"
    52

  pf
    "0007-blocks-duplicate-tangle-paths"
    "tangle path \"some/path.sh\" is not unique"
    101

  ps
    "0007-blocks-special-commas"
    ( nilOrgDoc
        [ CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__anon_block_number", "1"),
                      ("__block_type", "src"),
                      ("__src_language", "sh")
                    ],
                body =
                  [ BTokenString
                      . T.stripEnd
                      $ T.unlines
                        [ "Special commas get removed:",
                          "*",
                          "#+",
                          ",*",
                          ",#+",
                          "Special commas may have leading whitespace:",
                          " *",
                          "Non-special commas stay put:",
                          ",",
                          ",.",
                          ", x"
                        ]
                  ]
              }
        ]
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  pf
    "0008-indentation-comment"
    "invalid indentation"
    1

  pf
    "0008-indentation-headings"
    "invalid indentation"
    1

  pf
    "0008-indentation-paragraph"
    "invalid indentation"
    1

  p
    "0009-list-content-simple"
    $ nilOrgDoc
      [ CListItem
          [ LOrderedStart,
            LItemStart
          ],
        plainParagraph "First paragraph.",
        plainParagraph "Second paragraph.",
        CListItem
          [ LItemEnd,
            LItemStart
          ],
        plainParagraph "Another paragraph.",
        CListItem
          [ LItemEnd,
            LOrderedEnd
          ]
      ]

  p
    "0009-list-indentation-nested-with-text"
    $ nilOrgDoc
      [ plainParagraph "Foo.",
        CListItem
          [ LOrderedStart, ---- Start outer ordered list.
            LItemStart -------- Start outer "1)".
          ],
        plainParagraph "one",
        CListItem
          [ LUnorderedStart, -- Start inner bullets under "1)".
            LItemStart -------- Inner bullet start.
          ],
        plainParagraph "a",
        CListItem
          [ LItemEnd, --------- Inner bullet end.
            LItemStart -------- Inner bullet start.
          ],
        plainParagraph "b",
        CListItem
          [ LItemEnd, --------- Inner bullet end.
            LUnorderedEnd, ---- End of inner bullets under "1)".
            LItemEnd, --------- End of outer "1)".
            LItemStart -------- Start of outer "2)".
          ],
        plainParagraph "two",
        CListItem
          [ LOrderedStart, ---- Start of nested ordered list.
            LItemStart -------- Start of nested "1)".
          ],
        plainParagraph "two-point-one",
        plainParagraph "hello",
        CListItem
          [ LUnorderedStart, -- Start deeply nested bullets.
            LItemStart -------- Deeply nested bullet start.
          ],
        plainParagraph "X",
        CListItem
          [ LItemEnd, --------- Deeply nested bullet end.
            LUnorderedEnd, ---- End of deeply nested bullets.
            LItemEnd, --------- End of nested "1)".
            LOrderedEnd, ------ End of nested ordered list.
            LItemEnd, --------- End of outer "2)".
            LItemStart -------- Start of outer "3)".
          ],
        plainParagraph "three",
        CListItem
          [ LItemEnd, --------- End of outer "3)".
            LOrderedEnd ------- End outer ordered list.
          ],
        plainParagraph "Bar."
      ]

  p
    "0009-list-indentation-nested"
    $ nilOrgDoc
      [ CListItem
          [ LOrderedStart, ---- Start outer ordered list.
            LItemStart -------- Start outer "1)".
          ],
        CListItem
          [ LUnorderedStart, -- Start inner bullets under "1)".
            LItemStart -------- Inner bullet start.
          ],
        CListItem
          [ LItemEnd, --------- Inner bullet end.
            LItemStart -------- Inner bullet start.
          ],
        CListItem
          [ LItemEnd, --------- Inner bullet end.
            LUnorderedEnd, ---- End of inner bullets under "1)".
            LItemEnd, --------- End of outer "1)".
            LItemStart -------- Start of outer "2)".
          ],
        CListItem
          [ LOrderedStart, ---- Start of nested ordered list.
            LItemStart -------- Start of nested "1)".
          ],
        CListItem
          [ LUnorderedStart, -- Start deeply nested bullets.
            LItemStart -------- Deeply nested bullet start.
          ],
        CListItem
          [ LItemEnd, --------- Deeply nested bullet end.
            LUnorderedEnd, ---- End of deeply nested bullets.
            LItemEnd, --------- End of nested "1)".
            LOrderedEnd, ------ End of nested ordered list.
            LItemEnd, --------- End of outer "2)".
            LItemStart -------- Start of outer "3)".
          ],
        CListItem
          [ LItemEnd, --------- End of outer "3)".
            LOrderedEnd ------- End outer ordered list.
          ]
      ]

  pf
    "0009-list-nesting-max"
    "max nesting depth exceeded: cannot have more than 6 levels of list items"
    88

  pf
    "0009-list-nesting-max-8"
    "max nesting depth exceeded: cannot have more than 8 levels of list items"
    156

  p
    "0009-list-indentation-no-indent"
    $ nilOrgDoc
      [ plainParagraph "1)",
        plainParagraph "2)"
      ]

  p
    "0009-list-indentation-proper-indent"
    $ nilOrgDoc
      [ CListItem
          [ LOrderedStart,
            LItemStart
          ],
        CListItem
          [ LItemEnd,
            LItemStart
          ],
        CListItem
          [ LItemEnd,
            LOrderedEnd
          ]
      ]

  p
    "0009-list-items-need-blank-line-separator"
    $ nilOrgDoc
      [ CListItem
          [ LOrderedStart,
            LItemStart
          ],
        plainParagraph "a 1) b",
        CListItem
          [ LItemEnd,
            LOrderedEnd
          ]
      ]

  pf
    "0009-lists-cannot-be-mixed-on-their-own-ordered"
    "expected ORDERED list item at this level"
    23

  pf
    "0009-lists-cannot-be-mixed-on-their-own"
    "expected UNORDERED list item at this level"
    21

  pf
    "0009-lists-require-precise-indentation"
    "list item: invalid indentation"
    15

  pf
    "0009-ordered-lists-require-increment"
    "expected incremented list head 2 but got 1"
    13

  pf
    "0009-ordered-lists-start-from-0-or-1"
    "starting list head number must be 0 or 1; got 20"
    25

  pfm
    "0009-ordered-list-head-needs-parenthesis"
    (utoks "." <> etoks ")" <> elabel "ordered list number")
    3

  pf
    "0009-ordered-list-head-cannot-use-letters"
    "invalid indentation"
    2

  p
    "0009-paragraph-can-close-deeper-levels"
    $ nilOrgDoc
      [ CListItem
          [ LOrderedStart,
            LItemStart
          ],
        plainParagraph "One",
        plainParagraph "Two x",
        CListItem
          [ LUnorderedStart,
            LItemStart
          ],
        plainParagraph "Bar",
        CListItem
          [ LUnorderedStart,
            LItemStart
          ],
        plainParagraph "Baz",
        CListItem
          [ LItemEnd,
            LUnorderedEnd,
            LItemEnd,
            LUnorderedEnd
          ],
        plainParagraph "Three a b c",
        CListItem
          [ LItemEnd,
            LOrderedEnd
          ],
        plainParagraph "Four"
      ]

  p
    "0009-paragraph-can-separate-mixed-lists"
    $ nilOrgDoc
      [ CListItem
          [ LOrderedStart,
            LItemStart
          ],
        plainParagraph "One",
        CListItem
          [ LUnorderedStart,
            LItemStart
          ],
        plainParagraph "Bar",
        CListItem
          [ LUnorderedStart,
            LItemStart
          ],
        plainParagraph "Baz",
        CListItem
          [ LItemEnd,
            LUnorderedEnd,
            LItemEnd,
            LUnorderedEnd
          ],
        plainParagraph "Two",
        CListItem
          [ LOrderedStart,
            LItemStart
          ],
        plainParagraph "a",
        CListItem
          [ LItemEnd,
            LItemStart
          ],
        plainParagraph "b",
        CListItem
          [ LItemEnd,
            LOrderedEnd
          ],
        plainParagraph "Three",
        CListItem
          [ LItemEnd,
            LOrderedEnd
          ],
        plainParagraph "Four"
      ]

  ps
    "0010-standalone-math-fragments"
    ( nilOrgDoc
        [ CMathFragment
            MathFragment
              { meta =
                  fromList
                    [ ("__equation_number", "1")
                    ],
                body = "1 + 1 = 2"
              },
          CMathFragment
            MathFragment
              { meta =
                  fromList
                    [ ("__equation_number", "2")
                    ],
                body = "1 + 1 = 2"
              },
          CMathFragment
            MathFragment
              { meta =
                  fromList
                    [ ("__equation_anon_number", "1")
                    ],
                body = "2 + 2 = 4"
              },
          CMathFragment
            MathFragment
              { meta =
                  fromList
                    [ ("name", "eq:x")
                    ],
                body = "2 + 2 = 4"
              },
          CMathFragment
            MathFragment
              { meta =
                  fromList
                    [ ("__equation_number", "3"),
                      ("name", "eq:y")
                    ],
                body = "1 + 1 = 2"
              }
        ]
    )
    defaultOrgParserState
      { equationNumber = 3,
        anonEquationNumber = 1,
        cellNames =
          Set.fromList
            [ "eq:x",
              "eq:y"
            ]
      }

  ps
    "0010-standalone-math-inside-lists"
    ( nilOrgDoc
        [ CListItem
            [ LOrderedStart,
              LItemStart
            ],
          CParagraph
            $ Prose
              [ PTokenStyledText
                  StyledText
                    { body = "foo",
                      style = styleMaskFrom []
                    }
              ],
          CMathFragment
            MathFragment
              { meta =
                  fromList
                    [ ("name", "eq:x")
                    ],
                body = "2 + 2 = 4"
              },
          CMathFragment
            MathFragment
              { meta =
                  fromList
                    [ ("__equation_number", "1"),
                      ("name", "eq:y")
                    ],
                body = "1 + 1 = 2"
              },
          CListItem
            [ LItemEnd,
              LOrderedEnd
            ]
        ]
    )
    defaultOrgParserState
      { equationNumber = 1,
        cellNames =
          Set.fromList
            [ "eq:x",
              "eq:y"
            ]
      }

  ps
    "0011-figures-and-blocks"
    ( nilOrgDoc
        [ CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "haskell"),
                      ("name", "f:f"),
                      ("xxx", "bar")
                    ],
                body = [BTokenString "f = 1 + 1"]
              },
          CFigure
            Figure
              { meta =
                  fromList
                    [ ("__figure_number", "1"),
                      ("width", "100%"),
                      ("name", "fig:hello"),
                      ("caption", "=hello=")
                    ],
                caption =
                  Prose
                    [ PTokenStyledText
                        StyledText {body = "hello", style = styleMaskFrom [Verbatim]}
                    ],
                link =
                  Link
                    { target = LinkTargetFilePath "image/hello.png",
                      name =
                        [ StyledText
                            { body = "image/hello.png",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__block_type", "src"),
                      ("__src_language", "haskell"),
                      ("name", "f:g"),
                      ("yyy", "bar")
                    ],
                body = [BTokenString "g x = x + 1"]
              },
          CFigure
            Figure
              { meta =
                  fromList
                    [ ("__figure_number", "2")
                    ],
                caption = Prose [],
                link =
                  Link
                    { target = LinkTargetFilePath "image/world.png",
                      name =
                        [ StyledText
                            { body = "image/world.png",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              },
          CListItem
            [ LUnorderedStart,
              LItemStart
            ],
          CParagraph
            $ Prose
              [ PTokenLink
                  Link
                    { target = LinkTargetFilePath "not-figure",
                      name =
                        [ StyledText
                            { body = "not-figure",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              ],
          CListItem
            [ LItemEnd,
              LUnorderedEnd
            ],
          CParagraph
            $ Prose
              [ PTokenLink
                  Link
                    { target = LinkTargetFilePath "not-a-figure-2",
                      name =
                        [ StyledText
                            { body = "not-a-figure-2",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              ],
          CParagraph
            $ Prose
              [ PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "not a figure"
                            },
                      name =
                        [ StyledText
                            { body = "not a figure",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              ],
          CFigure
            Figure
              { meta =
                  fromList
                    [ ("__figure_number", "3")
                    ],
                caption = Prose [],
                link =
                  Link
                    { target = LinkTargetFilePath "foo.png",
                      name =
                        [ StyledText
                            { body = "foo.png",
                              style = styleMaskFrom []
                            }
                        ]
                    }
              }
        ]
    )
    defaultOrgParserState
      { cellNames =
          Set.fromList
            [ "f:f",
              "f:g",
              "fig:hello"
            ],
        figureNumber = 3
      }

  ps
    "0012-tables-and-prose"
    ( nilOrgDoc
        [ CTable
            Table
              { meta =
                  fromList
                    [ ("name", "tbl:famous-equations"),
                      ("caption", "*Bold*. [[tbl:famous-equations][A self-link]]. \\(E = mc^2\\)"),
                      ("__table_number", "1")
                    ],
                caption =
                  Prose
                    [ PTokenStyledText
                        StyledText {body = "Bold", style = styleMaskFrom [Bold]},
                      PTokenStyledText
                        StyledText {body = ".", style = styleMaskFrom []},
                      PTokenStyledText
                        StyledText {body = " ", style = styleMaskFrom []},
                      PTokenLink
                        Link
                          { name =
                              [ StyledText {body = "A", style = styleMaskFrom []},
                                StyledText {body = " ", style = styleMaskFrom []},
                                StyledText {body = "self-link", style = styleMaskFrom []}
                              ],
                            target =
                              LinkTargetOid
                                LinkOid {oid = nilOid, name = "tbl:famous-equations"}
                          },
                      PTokenStyledText
                        StyledText {body = ".", style = styleMaskFrom []},
                      PTokenStyledText
                        StyledText {body = " ", style = styleMaskFrom []},
                      PTokenMathFragment
                        MathFragment {meta = fromList [], body = "E = mc^2"}
                    ],
                grid =
                  fromList
                    [ ( (1, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Name",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (1, 2),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Equation",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (1, 3),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Link",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (2, 1),
                        L.tableHorizontalLine
                      ),
                      ( (3, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Pythagoras | Foo",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (3, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { meta = fromList [],
                                  body = "a^2 + b^2 = c^2"
                                }
                          ]
                      ),
                      ( (3, 3),
                        Prose
                          [ PTokenLink
                              Link
                                { target =
                                    LinkTargetURI
                                      N.URI
                                        { N.uriScheme = "https:",
                                          N.uriAuthority =
                                            Just
                                              N.URIAuth
                                                { N.uriUserInfo = "",
                                                  N.uriRegName = "wiki",
                                                  N.uriPort = ""
                                                },
                                          N.uriPath = "/x",
                                          N.uriQuery = "",
                                          N.uriFragment = ""
                                        },
                                  name =
                                    [ StyledText
                                        { body = "Wiki",
                                          style = styleMaskFrom []
                                        }
                                    ]
                                }
                          ]
                      ),
                      ( (4, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Albert ",
                                  style = styleMaskFrom []
                                },
                            PTokenStyledText
                              StyledText
                                { body = "Einstein",
                                  style = styleMaskFrom [Bold]
                                }
                          ]
                      ),
                      ( (4, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { meta = fromList [],
                                  body = "E = mc^2"
                                }
                          ]
                      ),
                      ( (4, 3),
                        Prose
                          [ PTokenLink
                              Link
                                { target =
                                    LinkTargetURI
                                      N.URI
                                        { N.uriScheme = "https:",
                                          N.uriAuthority =
                                            Just
                                              N.URIAuth
                                                { N.uriUserInfo = "",
                                                  N.uriRegName = "wiki",
                                                  N.uriPort = ""
                                                },
                                          N.uriPath = "/y",
                                          N.uriQuery = "",
                                          N.uriFragment = ""
                                        },
                                  name =
                                    [ StyledText
                                        { body = "Wiki",
                                          style = styleMaskFrom []
                                        }
                                    ]
                                }
                          ]
                      ),
                      ( (5, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Unknown",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ((5, 2), Prose []),
                      ((5, 3), Prose []),
                      ( (6, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "_",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (6, 2),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "*",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (6, 3),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "/",
                                  style = styleMaskFrom []
                                }
                          ]
                      )
                    ]
              }
        ]
    )
    defaultOrgParserState
      { cellNames =
          Set.fromList
            [ "tbl:famous-equations"
            ],
        tableNumber = 1
      }

  ps
    "0012-tables-inside-lists"
    ( nilOrgDoc
        [ CListItem
            [ LOrderedStart,
              LItemStart
            ],
          CParagraph
            $ Prose
              [ PTokenStyledText
                  StyledText
                    { body = "foo",
                      style = styleMaskFrom []
                    }
              ],
          CTable
            Table
              { meta =
                  fromList
                    [ ("name", "tbl:famous-equations"),
                      ("bar", "baz"),
                      ("__table_number", "1")
                    ],
                caption = Prose [],
                grid =
                  fromList
                    [ ( (1, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Name",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (1, 2),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Equation",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (1, 3),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Link",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (2, 1),
                        L.tableHorizontalLine
                      ),
                      ( (3, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Pythagoras | Foo",
                                  style = styleMaskFrom []
                                }
                          ]
                      ),
                      ( (3, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { meta = fromList [],
                                  body = "a^2 + b^2 = c^2"
                                }
                          ]
                      ),
                      ( (3, 3),
                        Prose
                          [ PTokenLink
                              Link
                                { target =
                                    LinkTargetURI
                                      N.URI
                                        { N.uriScheme = "https:",
                                          N.uriAuthority =
                                            Just
                                              N.URIAuth
                                                { N.uriUserInfo = "",
                                                  N.uriRegName = "wiki",
                                                  N.uriPort = ""
                                                },
                                          N.uriPath = "/x",
                                          N.uriQuery = "",
                                          N.uriFragment = ""
                                        },
                                  name =
                                    [ StyledText
                                        { body = "Wiki",
                                          style = styleMaskFrom []
                                        }
                                    ]
                                }
                          ]
                      ),
                      ( (4, 1),
                        Prose
                          [ PTokenStyledText
                              StyledText
                                { body = "Albert ",
                                  style = styleMaskFrom []
                                },
                            PTokenStyledText
                              StyledText
                                { body = "Einstein",
                                  style = styleMaskFrom [Bold]
                                }
                          ]
                      ),
                      ( (4, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { meta = fromList [],
                                  body = "E = mc^2"
                                }
                          ]
                      ),
                      ( (4, 3),
                        Prose
                          [ PTokenLink
                              Link
                                { target =
                                    LinkTargetURI
                                      N.URI
                                        { N.uriScheme = "https:",
                                          N.uriAuthority =
                                            Just
                                              N.URIAuth
                                                { N.uriUserInfo = "",
                                                  N.uriRegName = "wiki",
                                                  N.uriPort = ""
                                                },
                                          N.uriPath = "/y",
                                          N.uriQuery = "",
                                          N.uriFragment = ""
                                        },
                                  name =
                                    [ StyledText
                                        { body = "Wiki",
                                          style = styleMaskFrom []
                                        }
                                    ]
                                }
                          ]
                      )
                    ]
              },
          CListItem
            [ LItemEnd,
              LOrderedEnd
            ]
        ]
    )
    defaultOrgParserState
      { cellNames =
          Set.fromList
            [ "tbl:famous-equations"
            ],
        tableNumber = 1
      }

  pf
    "0013-footnotes-broken-link"
    "broken footnote links: hello"
    24

  pf
    "0013-footnotes-unused-def"
    "unused footnote definitions: hello"
    30

  ps
    "0013-footnotes-simple"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenStyledText
                    StyledText
                      { body = "Hello",
                        style = styleMaskFrom []
                      },
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "hello",
                        name =
                          [ StyledText
                              { body = "1",
                                style = styleMaskFrom []
                              }
                          ]
                      },
                  PTokenStyledText
                    StyledText
                      { body = " world.",
                        style = styleMaskFrom []
                      }
                ],
            CFootnote
              Footnote
                { name = "hello",
                  body =
                    Prose
                      [ PTokenStyledText
                          StyledText {body = "Foo.", style = styleMaskFrom []}
                      ]
                }
          ],
        footnoteNumbers =
          fromList
            [ ("hello", 1)
            ],
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
      }
    defaultOrgParserState
      { footnoteNumber = 1,
        footnoteNumbers =
          fromList
            [ ("hello", 1)
            ],
        footnoteDefinitions =
          fromList
            [ ( "hello",
                Prose
                  [ PTokenStyledText
                      StyledText
                        { body = "Foo.",
                          style = styleMaskFrom []
                        }
                  ]
              )
            ],
        prevCellIsFootnote = True
      }

  ps
    "0013-footnotes-complex"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenStyledText
                    StyledText
                      { body = "Hello",
                        style = styleMaskFrom []
                      },
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "a",
                        name =
                          [ StyledText
                              { body = "1",
                                style = styleMaskFrom []
                              }
                          ]
                      },
                  PTokenStyledText
                    StyledText
                      { body = " world.",
                        style = styleMaskFrom []
                      },
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "b",
                        name =
                          [ StyledText
                              { body = "2",
                                style = styleMaskFrom []
                              }
                          ]
                      }
                ],
            CFootnote
              Footnote
                { name = "b",
                  body =
                    Prose
                      [ PTokenStyledText
                          StyledText {body = "Definition of b.", style = styleMaskFrom []}
                      ]
                },
            CFootnote
              Footnote
                { name = "a",
                  body =
                    Prose
                      [ PTokenStyledText
                          StyledText {body = "Definition of a.", style = styleMaskFrom []}
                      ]
                }
          ],
        footnoteNumbers =
          fromList
            [ ("a", 1),
              ("b", 2)
            ],
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
      }
    defaultOrgParserState
      { footnoteNumber = 2,
        footnoteNumbers =
          fromList
            [ ("a", 1),
              ("b", 2)
            ],
        footnoteDefinitions =
          fromList
            [ ( "a",
                Prose
                  [ PTokenStyledText
                      StyledText
                        { body = "Definition of a.",
                          style = styleMaskFrom []
                        }
                  ]
              ),
              ( "b",
                Prose
                  [ PTokenStyledText
                      StyledText
                        { body = "Definition of b.",
                          style = styleMaskFrom []
                        }
                  ]
              )
            ],
        prevCellIsFootnote = True
      }

  pf
    "0013-footnotes-multiple-groups"
    "cannot have more than one group of footnote definitions"
    95

  ps
    "0014-citations-csl-path"
    OrgDoc
      { oid = nilOid,
        cells = [],
        footnoteNumbers = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = [],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  ps
    "0014-citations-csl-json-paths"
    OrgDoc
      { oid = nilOid,
        cells = [],
        footnoteNumbers = M.empty,
        cslPath = Nothing,
        bibPaths =
          [ "a.json",
            "b.json"
          ],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  ps
    "0014-citations-bibliography-location"
    OrgDoc
      { oid = nilOid,
        cells = [CBibliography ()],
        footnoteNumbers = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = ["a.json"],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  pf
    "0014-citations-print-biblio-missing-csl-style"
    "cannot print bibliography without '#+cite_export: csl <style>.csl' in top-level document metadata"
    44

  pf
    "0014-citations-print-biblio-missing-bib-entries"
    "cannot print bibliography without '#+bibliography: <bib_entries>.json' in top-level document metadata"
    48

  ps
    "0014-citations-single-cite"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0
                ]
          ],
        footnoteNumbers = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = ["a.json"],
        citations =
          [ Citation
              { styleVariation = (Just "v1", Just "v2"),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "foo123-.:?!`'/*@+|(){}<>&_^$#%~",
                        locator = Nothing,
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              }
          ],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  ps
    "0014-citations-locators"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 1,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 2,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 3,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 4,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 5
                ]
          ],
        footnoteNumbers = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = ["a.json"],
        citations =
          [ Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "a",
                        locator = Just ("page", "15"),
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "b",
                        locator = Just ("page", "15-30"),
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "c",
                        locator = Just ("chapter", "15-30, 35"),
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "d",
                        locator = Just ("section", "1.5-10"),
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "e",
                        locator = Just ("section", "1.22.3"),
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "f",
                        locator = Nothing,
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              }
          ],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  ps
    "0014-citations-locators-affixes"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 1,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 2,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 3,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 4,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 5
                ]
          ],
        footnoteNumbers = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = ["a.json"],
        citations =
          [ Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "a1",
                        locator = Just ("page", "15"),
                        prefix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "prefix",
                                    style = styleMaskFrom []
                                  }
                            ],
                        suffix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "suffix",
                                    style = styleMaskFrom []
                                  }
                            ]
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "b1",
                        locator = Just ("page", "15-30"),
                        prefix = Prose [],
                        suffix = Prose []
                      },
                    Cite
                      { id = "b2",
                        locator = Just ("chapter", "4"),
                        prefix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "a really long prefix, maybe?",
                                    style = styleMaskFrom []
                                  }
                            ],
                        suffix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "more suffix",
                                    style = styleMaskFrom []
                                  }
                            ]
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "c1",
                        locator = Just ("chapter", "15-30, 35"),
                        prefix = Prose [],
                        suffix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "suffix with balanced [[brackets] and @sign]",
                                    style = styleMaskFrom []
                                  }
                            ]
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "d1",
                        locator = Just ("section", "1.5-10"),
                        prefix = Prose [],
                        suffix = Prose []
                      },
                    Cite
                      { id = "d2",
                        locator = Nothing,
                        prefix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "prefix",
                                    style = styleMaskFrom []
                                  }
                            ],
                        suffix = Prose []
                      },
                    Cite
                      { id = "d3",
                        locator = Nothing,
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "e1",
                        locator = Just ("section", "1.22.3"),
                        prefix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "@ ",
                                    style = styleMaskFrom []
                                  },
                              PTokenStyledText
                                StyledText
                                  { body = "see also",
                                    style = styleMaskFrom [Italic]
                                  }
                            ],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              },
            Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "f1",
                        locator = Nothing,
                        prefix = Prose [],
                        suffix = Prose []
                      },
                    Cite
                      { id = "f2",
                        locator = Just ("volume", "3-4"),
                        prefix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "foo",
                                    style = styleMaskFrom [Bold]
                                  },
                              PTokenStyledText
                                StyledText
                                  { body = " ",
                                    style = styleMaskFrom []
                                  },
                              PTokenStyledText
                                StyledText
                                  { body = "@fake1",
                                    style = styleMaskFrom [Verbatim]
                                  },
                              PTokenStyledText
                                StyledText
                                  { body = " ",
                                    style = styleMaskFrom []
                                  },
                              PTokenStyledText
                                StyledText
                                  { body = "@fake2",
                                    style = styleMaskFrom [Code]
                                  }
                            ],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [],
                suffix = Prose []
              }
          ],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  ps
    "0014-citations-worg"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 1,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 2,
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenCitation 3
                ]
          ],
        footnoteNumbers = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = ["a.json"],
        citations =
          [ Citation
              { styleVariation = (Nothing, Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "a1",
                        locator = Nothing,
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix =
                  Prose
                    [ PTokenStyledText
                        StyledText
                          { body = "global",
                            style = styleMaskFrom [Bold]
                          },
                      PTokenStyledText
                        StyledText
                          { body = " prefix",
                            style = styleMaskFrom []
                          }
                    ],
                suffix =
                  Prose
                    [ PTokenStyledText
                        StyledText
                          { body = "global ",
                            style = styleMaskFrom []
                          },
                      PTokenStyledText
                        StyledText
                          { body = "suffix",
                            style = styleMaskFrom [Verbatim]
                          }
                    ]
              },
            Citation
              { styleVariation = (Just "t", Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "source1",
                        locator = Nothing,
                        prefix = Prose [],
                        suffix = Prose []
                      },
                    Cite
                      { id = "source2",
                        locator = Nothing,
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix =
                  Prose
                    [ PTokenStyledText
                        StyledText
                          { body = "see",
                            style = styleMaskFrom []
                          }
                    ],
                suffix =
                  Prose
                    [ PTokenStyledText
                        StyledText
                          { body = "by Smith ",
                            style = styleMaskFrom []
                          },
                      PTokenStyledText
                        StyledText
                          { body = "et al.",
                            style = styleMaskFrom [Italic]
                          }
                    ]
              },
            Citation
              { styleVariation = (Just "t", Nothing),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "foo",
                        locator = Just ("page", "7"),
                        prefix = Prose [],
                        suffix = Prose []
                      },
                    Cite
                      { id = "bar",
                        locator = Just ("page", "4"),
                        prefix = Prose [],
                        suffix = Prose []
                      }
                  ],
                prefix =
                  Prose
                    [ PTokenStyledText
                        StyledText
                          { body = "see",
                            style = styleMaskFrom []
                          }
                    ],
                suffix =
                  Prose
                    [ PTokenStyledText
                        StyledText
                          { body = "by foo",
                            style = styleMaskFrom []
                          }
                    ]
              },
            Citation
              { styleVariation = (Just "a", Just "f"),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "@atkey",
                        locator = Nothing,
                        prefix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "the very important",
                                    style = styleMaskFrom []
                                  }
                            ],
                        suffix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "@ once",
                                    style = styleMaskFrom []
                                  }
                            ]
                      },
                    Cite
                      { id = "baz",
                        locator = Just ("volume", "3"),
                        prefix =
                          Prose
                            [ PTokenStyledText
                                StyledText
                                  { body = "the crucial",
                                    style = styleMaskFrom []
                                  }
                            ],
                        suffix = Prose []
                      }
                  ],
                prefix =
                  Prose
                    [ PTokenStyledText
                        StyledText
                          { body = "c.f.",
                            style = styleMaskFrom []
                          }
                    ],
                suffix = Prose []
              }
          ],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  pf
    "0014-citations-unbalanced-brackets-prefix-opening"
    "unbalanced brackets"
    58

  pfm
    "0014-citations-unbalanced-brackets-prefix-closing"
    ( utoks "]"
        <> etoks "[["
        <> etoks "[fn:"
        <> etoks "\\("
        <> etoks "@"
        <> etoks "\\"
        <> elabel "(raw link) protocol"
        <> etoks "\n"
        <> elabel "normal characters"
    )
    57

  pfSimple "0014-citations-unbalanced-brackets-suffix-opening"

  pfm
    "0014-citations-unbalanced-brackets-suffix-closing"
    ( utoks "]]\n"
        <> etoks "[["
        <> etoks "[fn:"
        <> etoks "\\("
        <> etoks "\\"
        <> elabel "(raw link) protocol"
        <> etoks "\n"
        <> elabel "normal characters"
    )
    62

  pfm
    "0014-citations-extra-semicolons-basic"
    ( utoks ";"
        <> etoks "[["
        <> etoks "[fn:"
        <> etoks "\\("
        <> etoks "@"
        <> etoks "\\"
        <> elabel "(raw link) protocol"
        <> elabel "intra-cell whitespace"
        <> elabel "normal characters"
    )
    56

  pfm
    "0014-citations-extra-semicolons-consecutive"
    ( utoks ";"
        <> etoks "[["
        <> etoks "[fn:"
        <> etoks "\\("
        <> etoks "@"
        <> etoks "\\"
        <> etoks "]"
        <> elabel "(raw link) protocol"
        <> elabel "intra-cell whitespace"
        <> elabel "normal characters"
    )
    61

  pf
    "0014-citations-nested"
    "unbalanced brackets"
    67

  ps
    "0014-citations-forbidden-in-verbatim-code"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenStyledText
                    StyledText
                      { body = "[cite:@foo]",
                        style = styleMaskFrom [Code]
                      },
                  PTokenStyledText StyledText {body = " ", style = styleMaskFrom []},
                  PTokenStyledText
                    StyledText
                      { body = "[cite:@bar]",
                        style = styleMaskFrom [Verbatim]
                      }
                ]
          ],
        footnoteNumbers = M.empty,
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  ps
    "0014-citations-forbidden-in-headings"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CHeading
              Heading
                { level = 1,
                  section = [1],
                  body =
                    Prose
                      [ PTokenStyledText
                          StyledText
                            { body = "heading [cite:@foo]",
                              style = styleMaskFrom []
                            }
                      ],
                  meta = fromList []
                }
          ],
        footnoteNumbers = M.empty,
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSection = [1]
      }

  pf
    "0014-citations-missing-csl-style"
    "cannot parse citations without '#+cite_export: csl <style>.csl' in top-level document metadata"
    28

  pf
    "0014-citations-missing-bib-entries"
    "cannot parse citations without '#+bibliography: <bib_entries>.json' in top-level document metadata"
    32

  describe "Text to Prose round trip" $ do
    checkTextToProseRoundTrip
      "can handle StyledText"
      [ ( "no style",
          Prose
            [ PTokenStyledText
                StyledText
                  { body = "no",
                    style = styleMaskFrom []
                  },
              PTokenStyledText
                StyledText
                  { body = " ",
                    style = styleMaskFrom []
                  },
              PTokenStyledText
                StyledText
                  { body = "style",
                    style = styleMaskFrom []
                  }
            ]
        ),
        ( "*bold*",
          Prose
            [ PTokenStyledText
                StyledText
                  { body = "bold",
                    style = styleMaskFrom [Bold]
                  }
            ]
        ),
        ( "/italic/",
          Prose
            [ PTokenStyledText
                StyledText
                  { body = "italic",
                    style = styleMaskFrom [Italic]
                  }
            ]
        ),
        ( "_underline_",
          Prose
            [ PTokenStyledText
                StyledText
                  { body = "underline",
                    style = styleMaskFrom [Underline]
                  }
            ]
        ),
        ( "=*/_~verbatim=",
          Prose
            [ PTokenStyledText
                StyledText
                  { body = "*/_~verbatim",
                    style = styleMaskFrom [Verbatim]
                  }
            ]
        ),
        ( "~*/_=code~",
          Prose
            [ PTokenStyledText
                StyledText
                  { body = "*/_=code",
                    style = styleMaskFrom [Code]
                  }
            ]
        )
      ]
    checkTextToProseRoundTrip
      "can handle OID links"
      [ ( "[[id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaa1]]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaa1",
                            style = styleMaskFrom []
                          }
                      ],
                    target =
                      LinkTargetOid
                        LinkOid
                          { oid = fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaa1,
                            name = ""
                          }
                  }
            ]
        ),
        ( "[[id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaa2][*foo*]]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "foo",
                            style = styleMaskFrom [Bold]
                          }
                      ],
                    target =
                      LinkTargetOid
                        LinkOid
                          { oid = fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaa2,
                            name = ""
                          }
                  }
            ]
        ),
        ( "[[id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaa3::abc][/bar/]]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "bar",
                            style = styleMaskFrom [Italic]
                          }
                      ],
                    target =
                      LinkTargetOid
                        LinkOid
                          { oid = fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaa3,
                            name = "abc"
                          }
                  }
            ]
        )
      ]
    checkTextToProseRoundTrip
      "can handle footnote links"
      [ ( "[fn:abc]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "1",
                            style = styleMaskFrom []
                          }
                      ],
                    target = LinkTargetFootnote "abc"
                  }
            ]
        ),
        ( "[fn:abc][fn:xyz][fn:abc]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "1",
                            style = styleMaskFrom []
                          }
                      ],
                    target = LinkTargetFootnote "abc"
                  },
              PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "2",
                            style = styleMaskFrom []
                          }
                      ],
                    target = LinkTargetFootnote "xyz"
                  },
              PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "1",
                            style = styleMaskFrom []
                          }
                      ],
                    target = LinkTargetFootnote "abc"
                  }
            ]
        )
      ]
    checkTextToProseRoundTrip
      "can handle web links"
      [ ( "[[https://foo.org][bar]]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "bar",
                            style = styleMaskFrom []
                          }
                      ],
                    target =
                      LinkTargetURI
                        N.URI
                          { N.uriScheme = "https:",
                            N.uriAuthority =
                              Just
                                N.URIAuth
                                  { N.uriUserInfo = "",
                                    N.uriRegName = "foo.org",
                                    N.uriPort = ""
                                  },
                            N.uriPath = "",
                            N.uriQuery = "",
                            N.uriFragment = ""
                          }
                  }
            ]
        ),
        ( "https://foo2.org",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "https://foo2.org",
                            style = styleMaskFrom []
                          }
                      ],
                    target =
                      LinkTargetURI
                        N.URI
                          { N.uriScheme = "https:",
                            N.uriAuthority =
                              Just
                                N.URIAuth
                                  { N.uriUserInfo = "",
                                    N.uriRegName = "foo2.org",
                                    N.uriPort = ""
                                  },
                            N.uriPath = "",
                            N.uriQuery = "",
                            N.uriFragment = ""
                          }
                  }
            ]
        )
      ]
    checkTextToProseRoundTrip
      "can handle file links"
      [ ( "[[file:foo/bar]]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "foo/bar",
                            style = styleMaskFrom []
                          }
                      ],
                    target = LinkTargetFilePath "foo/bar"
                  }
            ]
        ),
        ( "[[file:foo/bar][baz]]",
          Prose
            [ PTokenLink
                Link
                  { name =
                      [ StyledText
                          { body = "baz",
                            style = styleMaskFrom []
                          }
                      ],
                    target = LinkTargetFilePath "foo/bar"
                  }
            ]
        )
      ]
    checkTextToProseRoundTrip
      "can handle inline math fragments"
      [ ( "\\(4 = 2 * 2\\)",
          Prose
            [ PTokenMathFragment
                MathFragment
                  { body = "4 = 2 * 2",
                    meta = fromList []
                  }
            ]
        )
      ]

  describe "Drop text chars from Prose" $ do
    it "can handle plain text from the front" $ do
      dropTextWhile (== 'a') (fromText "aa b *cc*" :: Prose)
        `shouldBe` (fromText " b *cc*")
    it "can handle plain text from the back" $ do
      dropTextWhileEnd (== 'c') (fromText "aa b *cc*" :: Prose)
        `shouldBe` (fromText "aa b ")
    it "can handle styled (italic) text from the front" $ do
      dropTextWhile (== 'a') (fromText "/aa/ b *cc*" :: Prose)
        `shouldBe` (fromText " b *cc*")
    it "can handle styled (italic) text from the back" $ do
      dropTextWhileEnd (== 'c') (fromText "/aa/ b *cc*" :: Prose)
        `shouldBe` (fromText "/aa/ b ")
    it "skips over non-text elements from the front" $ do
      dropTextWhile (== 'a') (fromText "[[foo]]/aa/ b *cc*" :: Prose)
        `shouldBe` (fromText "[[foo]] b *cc*")
    it "skips over non-text elements from the back" $ do
      dropTextWhileEnd (== 'c') (fromText "/aa/ b *cc*[[foo]]" :: Prose)
        `shouldBe` (fromText "/aa/ b [[foo]]")
