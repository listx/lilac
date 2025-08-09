module Lilac.ParseSpec (spec) where

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
    Citation (Citation, cites, prefix, styleVariation, suffix),
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
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (StyledText, body, style),
    Table (Table, caption, grid, meta),
    fromWords,
    nilOid,
    nilOidOrgDoc,
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
          action nilOidOrgDoc input L.defaultOrgParserState
      | otherwise = action "" input L.defaultOrgParserState
    action inputPrefix input expectedState =
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
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
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (inputPrefix <> input)
        `shouldParse` (expected, expectedState')

pf :: FilePath -> String -> Int -> Spec
pf path errMsg pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
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
        (runStateT L.orgDocParser L.defaultOrgParserState)
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
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        `shouldFailOn` (inputPrefix <> input)
  where
    inputPrefix = nilOidOrgDoc

plainParagraph :: Text -> Cell
plainParagraph body =
  CParagraph
    [ PTokenStyledText
        ( StyledText
            { body = body,
              style = L.styleMaskFrom []
            }
        )
    ]

plainHeading :: Int -> [Int] -> Cell
plainHeading level section =
  CHeading
    ( Heading
        { level = level,
          section = section,
          body =
            [ PTokenStyledText
                ( StyledText
                    { body = T.pack $ show level,
                      style = L.styleMaskFrom []
                    }
                )
            ],
          meta = fromList []
        }
    )

nilOrgDoc :: [Cell] -> OrgDoc
nilOrgDoc cells =
  OrgDoc
    { oid = nilOid,
      cells = cells,
      footnoteNumbers = M.empty,
      cslPath = Nothing,
      bibPaths = [],
      meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
    }

parseSpec :: Spec
parseSpec = do
  it "empty input" $ do
    MP.runParser
      (runStateT L.orgDocParser L.defaultOrgParserState)
      "empty input"
      nilOidOrgDoc
      `shouldParse` (L.defaultOrgDoc nilOid [], L.defaultOrgParserState)

  before (LU.readFrom "" "base/lilac.org") $ do
    it "self-parse" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
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
    L.defaultOrgParserState

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
    L.defaultOrgParserState
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
    L.defaultOrgParserState
      { docOid = fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f
      }

  before (LU.readFrom "test/Lilac/ParseSpec/" "0002-doc-metadata-properties-drawer-clash") $ do
    it "0002-doc-metadata-properties-drawer-clash" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
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
    L.defaultOrgParserState
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
    L.defaultOrgParserState
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
            ( Heading
                { level = 1,
                  section = [1],
                  body =
                    [ PTokenStyledText
                        (StyledText {body = "foo", style = L.styleMaskFrom []})
                    ],
                  meta = fromList []
                }
            )
        ]
    )
    L.defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSection = [1]
      }

  ps
    "0004-properties-drawer-not-empty"
    ( nilOrgDoc
        [ CHeading
            ( Heading
                { level = 1,
                  section = [1],
                  body =
                    [ PTokenStyledText
                        (StyledText {body = "foo", style = L.styleMaskFrom []})
                    ],
                  meta =
                    fromList
                      [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6")
                      ]
                }
            )
        ]
    )
    L.defaultOrgParserState
      { headingOidStack = [Just $ fromWords 0xb949ea2ee6d1453d 0x93525e85a82cd4e6],
        headingLevel = 1,
        headingSection = [1]
      }

  ps
    "0004-properties-drawer-not-empty-with-comments"
    ( nilOrgDoc
        [ CHeading
            ( Heading
                { level = 1,
                  section = [1],
                  body =
                    [ PTokenStyledText
                        (StyledText {body = "foo", style = L.styleMaskFrom []})
                    ],
                  meta =
                    fromList
                      [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6")
                      ]
                }
            )
        ]
    )
    L.defaultOrgParserState
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
          [ PTokenStyledText
              (StyledText {body = "a b c d e ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "f g h i j", style = L.styleMaskFrom [Bold]})
          ],
        CParagraph
          [ PTokenStyledText
              (StyledText {body = "x y z foo bar", style = L.styleMaskFrom []})
          ],
        CParagraph
          [ PTokenStyledText
              ( StyledText
                  { body = "multiple lines with underscore",
                    style = L.styleMaskFrom [Underline]
                  }
              )
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
          [ PTokenStyledText
              ( StyledText
                  { body = "foo ",
                    style = L.styleMaskFrom []
                  }
              ),
            PTokenStyledText
              ( StyledText
                  { body = "bar",
                    style = L.styleMaskFrom [Bold]
                  }
              ),
            PTokenStyledText
              ( StyledText
                  { body = " baz",
                    style = L.styleMaskFrom []
                  }
              )
          ]
      ]

  p
    "0005-styled-strings-complex-punctuation"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              (StyledText {body = "(foo ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "bar", style = L.styleMaskFrom [Verbatim]}),
            PTokenStyledText
              (StyledText {body = "). What's up? and ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "that!", style = L.styleMaskFrom [Bold]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "(italic)", style = L.styleMaskFrom [Italic]}),
            PTokenStyledText
              (StyledText {body = ". It's #1!", style = L.styleMaskFrom []})
          ]
      ]

  p
    "0005-styled-strings-complex"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              (StyledText {body = "foo ", style = L.styleMaskFrom [Bold]}),
            PTokenStyledText
              (StyledText {body = "bar ", style = L.styleMaskFrom [Bold, Italic]}),
            PTokenStyledText
              ( StyledText
                  { body = "underlined bold and italic",
                    style = L.styleMaskFrom [Bold, Italic, Underline]
                  }
              ),
            PTokenStyledText
              (StyledText {body = " baz", style = L.styleMaskFrom [Bold]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "verbatim  text", style = L.styleMaskFrom [Verbatim]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "x  y  z", style = L.styleMaskFrom [Code]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              ( StyledText
                  { body = "bold",
                    style = L.styleMaskFrom [Bold]
                  }
              )
          ]
      ]

  p
    "0005-styled-strings-compression"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              (StyledText {body = "a b c d e ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "f g h i j", style = L.styleMaskFrom [Bold]})
          ]
      ]

  p
    "0005-styled-strings-ignore-marker-inside-word"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              (StyledText {body = "foo and/or 1/2 bar \\ a\\b", style = L.styleMaskFrom [Italic]})
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
            ( Heading
                { level = 1,
                  section = [1],
                  body =
                    [ PTokenStyledText
                        (StyledText {body = "foo ", style = L.styleMaskFrom [Bold]}),
                      PTokenStyledText
                        (StyledText {body = "bar baz a", style = L.styleMaskFrom []}),
                      PTokenStyledText
                        (StyledText {body = "b", style = L.styleMaskFrom [Bold]}),
                      PTokenStyledText
                        (StyledText {body = "c (", style = L.styleMaskFrom []}),
                      PTokenStyledText
                        (StyledText {body = "ok", style = L.styleMaskFrom [Bold]}),
                      PTokenStyledText
                        (StyledText {body = ") yes", style = L.styleMaskFrom []})
                    ],
                  meta = fromList []
                }
            )
        ]
    )
    L.defaultOrgParserState
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
          [ PTokenStyledText
              (StyledText {body = "*a", style = L.styleMaskFrom [Verbatim]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "b*", style = L.styleMaskFrom [Verbatim]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "*c", style = L.styleMaskFrom [Code]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "d*", style = L.styleMaskFrom [Code]})
          ]
      ]

  p
    "0005-styled-strings-verbatim-precedence"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              (StyledText {body = "[[foo", style = L.styleMaskFrom [Verbatim]})
          ],
        CParagraph
          [ PTokenLink
              ( Link
                  { target =
                      LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "hello"
                          },
                    name =
                      [ StyledText
                          { body = "hello",
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
              ),
            PTokenStyledText
              (StyledText {body = " there", style = L.styleMaskFrom []})
          ],
        CParagraph
          [ PTokenStyledText
              (StyledText {body = "\\(1 + 1 ", style = L.styleMaskFrom [Verbatim]}),
            PTokenStyledText
              (StyledText {body = " ", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "\\(2 * 2\\)", style = L.styleMaskFrom [Code]})
          ],
        CParagraph
          [ PTokenStyledText
              (StyledText {body = "[[bar]]", style = L.styleMaskFrom [Code]})
          ]
      ]

  p
    "0005-styled-strings-verbatim"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              (StyledText {body = "(", style = L.styleMaskFrom []}),
            PTokenStyledText
              (StyledText {body = "import ./.", style = L.styleMaskFrom [Verbatim]}),
            PTokenStyledText
              (StyledText {body = ") hello", style = L.styleMaskFrom []})
          ]
      ]

  p
    "0006-links-inside-parentheses"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              ( StyledText
                  { body = "(",
                    style = L.styleMaskFrom []
                  }
              ),
            PTokenLink
              ( Link
                  { target = LinkTargetFilePath "foo/bar",
                    name =
                      [ StyledText
                          { body = "foo/bar",
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
              ),
            PTokenStyledText
              ( StyledText
                  { body = ")",
                    style = L.styleMaskFrom []
                  }
              )
          ]
      ]

  p
    "0006-links-nonlink"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              ( StyledText
                  { body = "[hi]",
                    style = L.styleMaskFrom []
                  }
              )
          ]
      ]

  p
    "0006-links-raw"
    $ nilOrgDoc
      [ CParagraph
          [ PTokenStyledText
              ( StyledText
                  { body = "See ",
                    style = L.styleMaskFrom []
                  }
              ),
            PTokenLink
              ( Link
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
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
              ),
            PTokenStyledText
              ( StyledText
                  { body = ", or ",
                    style = L.styleMaskFrom []
                  }
              ),
            PTokenLink
              ( Link
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
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
              ),
            PTokenStyledText
              ( StyledText
                  { body = ". Try ",
                    style = L.styleMaskFrom []
                  }
              ),
            PTokenLink
              ( Link
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
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
              ),
            PTokenStyledText
              ( StyledText
                  { body = "? Go to ",
                    style = L.styleMaskFrom []
                  }
              ),
            PTokenLink
              ( Link
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
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
              ),
            PTokenStyledText
              ( StyledText
                  { body = "!",
                    style = L.styleMaskFrom []
                  }
              )
          ]
      ]

  ps
    "0006-links"
    ( nilOrgDoc
        [ CParagraph
            [ PTokenStyledText
                ( StyledText
                    { body = "foo ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "link-target-only"
                            },
                      name =
                        [ StyledText
                            { body = "link-target-only",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                )
            ],
          CHeading
            ( Heading
                { level = 1,
                  section = [1],
                  body =
                    [ PTokenLink
                        ( Link
                            { target =
                                LinkTargetOid
                                  LinkOid
                                    { oid = nilOid,
                                      name = "link-target"
                                    },
                              name =
                                [ StyledText
                                    { body = "link-name",
                                      style = L.styleMaskFrom []
                                    }
                                ]
                            }
                        )
                    ],
                  meta = fromList []
                }
            ),
          CParagraph
            [ PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "a"
                            },
                      name =
                        [ StyledText
                            { body = "a",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " b",
                      style = L.styleMaskFrom []
                    }
                )
            ]
        ]
    )
    L.defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSection = [1]
      }

  p
    "0006-links-web-protocols"
    ( nilOrgDoc
        [ CParagraph
            [ PTokenLink
                ( Link
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
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
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
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
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
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                )
            ]
        ]
    )

  p
    "0006-links-oids"
    ( nilOrgDoc
        [ CParagraph
            [ PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "id:442c807f-c9d9-45a7-88bf-ffffffffffff",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "bar",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "bar",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = "[]",
                              style = L.styleMaskFrom [Verbatim]
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ StyledText
                            { body = "bar",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = "ok",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = "foo"
                            },
                      name =
                        [ StyledText
                            { body = "abc",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                ),
              PTokenStyledText
                ( StyledText
                    { body = " ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "foo"
                            },
                      name =
                        [ StyledText
                            { body = "abc",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                )
            ],
          CListItem
            [ LUnorderedStart,
              LItemStart
            ],
          CParagraph
            [ PTokenStyledText
                ( StyledText
                    { body = "link name across list indentation ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { name =
                        [ StyledText
                            { body = "italic",
                              style = L.styleMaskFrom [Italic]
                            },
                          StyledText
                            { body = " ",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = "def",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = "ghi]",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = " ",
                              style = L.styleMaskFrom []
                            },
                          StyledText
                            { body = "!",
                              style = L.styleMaskFrom []
                            }
                        ],
                      target =
                        LinkTargetOid
                          ( LinkOid
                              { oid = nilOid,
                                name = "foo"
                              }
                          )
                    }
                )
            ],
          CListItem
            [ LItemEnd,
              LItemStart
            ],
          CParagraph
            [ PTokenStyledText
                ( StyledText
                    { body = "link target with spaces ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { name =
                        [ StyledText
                            { body = "foo bar baz",
                              style = L.styleMaskFrom []
                            }
                        ],
                      target =
                        LinkTargetOid
                          ( LinkOid
                              { oid = nilOid,
                                name = "foo bar baz"
                              }
                          )
                    }
                )
            ],
          CListItem
            [ LItemEnd,
              LItemStart
            ],
          CParagraph
            [ PTokenStyledText
                ( StyledText
                    { body = "oid with spaces ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { name =
                        [ StyledText
                            { body = "a b c",
                              style = L.styleMaskFrom []
                            }
                        ],
                      target =
                        LinkTargetOid
                          ( LinkOid
                              { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                                name = "a b c"
                              }
                          )
                    }
                )
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
            [ PTokenStyledText
                ( StyledText
                    { body = "Hello ",
                      style = L.styleMaskFrom []
                    }
                ),
              PTokenLink
                ( Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = "target name with newline and space is still treated as space"
                            },
                      name =
                        [ StyledText
                            { body = "foo",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
                )
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
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__anon_block_number", "1"),
                        ("__block_type", "src"),
                        ("__src_language", "haskell")
                      ],
                  body = []
                }
            ),
          CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__block_type", "example"),
                        ("name", "foo")
                      ],
                  body = []
                }
            ),
          CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__anon_block_number", "2"),
                        ("__block_type", "quote")
                      ],
                  body = []
                }
            )
        ]
    )
    L.defaultOrgParserState
      { cellNames = Set.fromList ["foo"],
        anonBlockNumber = 2
      }

  ps
    "0007-blocks-ending"
    ( nilOrgDoc
        [ CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "sh"),
                        ("name", "empty")
                      ],
                  body = []
                }
            ),
          CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "sh"),
                        ("name", "end not in beginning of line")
                      ],
                  body = [BTokenString " #+end_src"]
                }
            ),
          CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "sh"),
                        ("name", "end has no preceding blank line")
                      ],
                  body = [BTokenString "foo"]
                }
            ),
          CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "sh"),
                        ("name", "end has preceding blank line")
                      ],
                  body = [BTokenString "foo\n"]
                }
            ),
          CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "sh"),
                        ("name", "end has preceding newline (multiple)")
                      ],
                  body = [BTokenString "foo\n\n"]
                }
            ),
          CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "sh"),
                        ("name", "ignore #+end_example because this is a src block")
                      ],
                  body = [BTokenString "#+end_example"]
                }
            )
        ]
    )
    L.defaultOrgParserState
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
            ( Block
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
            )
        ]
    )
    L.defaultOrgParserState
      { anonBlockNumber = 1
      }

  ps
    "0007-blocks-noweb"
    ( nilOrgDoc
        [ CBlock
            ( Block
                { parentOid = nilOid,
                  meta =
                    fromList
                      [ ("__anon_block_number", "1"),
                        ("__block_type", "src"),
                        ("__src_language", "sh")
                      ],
                  body =
                    [ BTokenLink
                        ( Link
                            { target =
                                LinkTargetOid
                                  LinkOid
                                    { oid = nilOid,
                                      name = "foo"
                                    },
                              name =
                                [ StyledText
                                    { body = "foo",
                                      style = L.styleMaskFrom []
                                    }
                                ]
                            }
                        ),
                      BTokenString "\n\n<<quux\n>>\necho ",
                      BTokenLink
                        ( Link
                            { target =
                                LinkTargetOid
                                  LinkOid
                                    { oid = nilOid,
                                      name = "bar"
                                    },
                              name =
                                [ StyledText
                                    { body = "bar",
                                      style = L.styleMaskFrom []
                                    }
                                ]
                            }
                        ),
                      BTokenString " x y z"
                    ]
                }
            )
        ]
    )
    L.defaultOrgParserState
      { anonBlockNumber = 1
      }

  ps
    "0007-blocks-noweb-intraline"
    ( nilOrgDoc
        [ CBlock
            ( Block
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
                        ( Link
                            { target =
                                LinkTargetOid
                                  LinkOid
                                    { oid = nilOid,
                                      name = "foo"
                                    },
                              name =
                                [ StyledText
                                    { body = "foo",
                                      style = L.styleMaskFrom []
                                    }
                                ]
                            }
                        ),
                      BTokenString ") and \"",
                      BTokenLink
                        ( Link
                            { target =
                                LinkTargetOid
                                  LinkOid
                                    { oid = nilOid,
                                      name = "bar"
                                    },
                              name =
                                [ StyledText
                                    { body = "bar",
                                      style = L.styleMaskFrom []
                                    }
                                ]
                            }
                        ),
                      BTokenString "\" and <",
                      BTokenLink
                        ( Link
                            { target =
                                LinkTargetOid
                                  LinkOid
                                    { oid = nilOid,
                                      name = "ok"
                                    },
                              name =
                                [ StyledText
                                    { body = "ok",
                                      style = L.styleMaskFrom []
                                    }
                                ]
                            }
                        ),
                      BTokenString ">"
                    ]
                }
            )
        ]
    )
    L.defaultOrgParserState
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
            ( Block
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
            )
        ]
    )
    L.defaultOrgParserState
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
    L.defaultOrgParserState
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
            [ PTokenStyledText
                ( StyledText
                    { body = "foo",
                      style = L.styleMaskFrom []
                    }
                )
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
    L.defaultOrgParserState
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
                  [ PTokenStyledText
                      (StyledText {body = "hello", style = L.styleMaskFrom [Verbatim]})
                  ],
                link =
                  Link
                    { target = LinkTargetFilePath "image/hello.png",
                      name =
                        [ StyledText
                            { body = "image/hello.png",
                              style = L.styleMaskFrom []
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
                caption = [],
                link =
                  Link
                    { target = LinkTargetFilePath "image/world.png",
                      name =
                        [ StyledText
                            { body = "image/world.png",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
              },
          CListItem
            [ LUnorderedStart,
              LItemStart
            ],
          CParagraph
            [ PTokenLink
                Link
                  { target = LinkTargetFilePath "not-figure",
                    name =
                      [ StyledText
                          { body = "not-figure",
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
            ],
          CListItem
            [ LItemEnd,
              LUnorderedEnd
            ],
          CParagraph
            [ PTokenLink
                Link
                  { target = LinkTargetFilePath "not-a-figure-2",
                    name =
                      [ StyledText
                          { body = "not-a-figure-2",
                            style = L.styleMaskFrom []
                          }
                      ]
                  }
            ],
          CParagraph
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
                            style = L.styleMaskFrom []
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
                caption = [],
                link =
                  Link
                    { target = LinkTargetFilePath "foo.png",
                      name =
                        [ StyledText
                            { body = "foo.png",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
              }
        ]
    )
    L.defaultOrgParserState
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
                  [ PTokenStyledText
                      (StyledText {body = "Bold", style = L.styleMaskFrom [Bold]}),
                    PTokenStyledText
                      (StyledText {body = ".", style = L.styleMaskFrom []}),
                    PTokenStyledText
                      (StyledText {body = " ", style = L.styleMaskFrom []}),
                    PTokenLink
                      ( Link
                          { name =
                              [ StyledText {body = "A", style = L.styleMaskFrom []},
                                StyledText {body = " ", style = L.styleMaskFrom []},
                                StyledText {body = "self-link", style = L.styleMaskFrom []}
                              ],
                            target =
                              LinkTargetOid
                                LinkOid {oid = nilOid, name = "tbl:famous-equations"}
                          }
                      ),
                    PTokenStyledText
                      (StyledText {body = ".", style = L.styleMaskFrom []}),
                    PTokenStyledText
                      (StyledText {body = " ", style = L.styleMaskFrom []}),
                    PTokenMathFragment
                      (MathFragment {meta = fromList [], body = "E = mc^2"})
                  ],
                grid =
                  fromList
                    [ ( (1, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Name",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (1, 2),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Equation",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (1, 3),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Link",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (2, 1),
                        L.tableHorizontalLine
                      ),
                      ( (3, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Pythagoras | Foo",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (3, 2),
                        [ PTokenMathFragment
                            ( MathFragment
                                { meta = fromList [],
                                  body = "a^2 + b^2 = c^2"
                                }
                            )
                        ]
                      ),
                      ( (3, 3),
                        [ PTokenLink
                            ( Link
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
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            )
                        ]
                      ),
                      ( (4, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Albert ",
                                  style = L.styleMaskFrom []
                                }
                            ),
                          PTokenStyledText
                            ( StyledText
                                { body = "Einstein",
                                  style = L.styleMaskFrom [Bold]
                                }
                            )
                        ]
                      ),
                      ( (4, 2),
                        [ PTokenMathFragment
                            ( MathFragment
                                { meta = fromList [],
                                  body = "E = mc^2"
                                }
                            )
                        ]
                      ),
                      ( (4, 3),
                        [ PTokenLink
                            ( Link
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
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            )
                        ]
                      ),
                      ( (5, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Unknown",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ((5, 2), []),
                      ((5, 3), []),
                      ( (6, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "_",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (6, 2),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "*",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (6, 3),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "/",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      )
                    ]
              }
        ]
    )
    L.defaultOrgParserState
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
            [ PTokenStyledText
                ( StyledText
                    { body = "foo",
                      style = L.styleMaskFrom []
                    }
                )
            ],
          CTable
            Table
              { meta =
                  fromList
                    [ ("name", "tbl:famous-equations"),
                      ("bar", "baz"),
                      ("__table_number", "1")
                    ],
                caption = [],
                grid =
                  fromList
                    [ ( (1, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Name",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (1, 2),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Equation",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (1, 3),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Link",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (2, 1),
                        L.tableHorizontalLine
                      ),
                      ( (3, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Pythagoras | Foo",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ]
                      ),
                      ( (3, 2),
                        [ PTokenMathFragment
                            ( MathFragment
                                { meta = fromList [],
                                  body = "a^2 + b^2 = c^2"
                                }
                            )
                        ]
                      ),
                      ( (3, 3),
                        [ PTokenLink
                            ( Link
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
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            )
                        ]
                      ),
                      ( (4, 1),
                        [ PTokenStyledText
                            ( StyledText
                                { body = "Albert ",
                                  style = L.styleMaskFrom []
                                }
                            ),
                          PTokenStyledText
                            ( StyledText
                                { body = "Einstein",
                                  style = L.styleMaskFrom [Bold]
                                }
                            )
                        ]
                      ),
                      ( (4, 2),
                        [ PTokenMathFragment
                            ( MathFragment
                                { meta = fromList [],
                                  body = "E = mc^2"
                                }
                            )
                        ]
                      ),
                      ( (4, 3),
                        [ PTokenLink
                            ( Link
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
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            )
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
    L.defaultOrgParserState
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
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CParagraph
                [ PTokenStyledText
                    ( StyledText
                        { body = "Hello",
                          style = L.styleMaskFrom []
                        }
                    ),
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "hello",
                        name =
                          [ StyledText
                              { body = "1",
                                style = L.styleMaskFrom []
                              }
                          ]
                      },
                  PTokenStyledText
                    ( StyledText
                        { body = " world.",
                          style = L.styleMaskFrom []
                        }
                    )
                ],
              CFootnote
                Footnote
                  { name = "hello",
                    body =
                      [ PTokenStyledText
                          (StyledText {body = "Foo.", style = L.styleMaskFrom []})
                      ]
                  }
            ],
          footnoteNumbers =
            fromList
              [ ("hello", 1)
              ],
          cslPath = Nothing,
          bibPaths = [],
          meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
        }
    )
    L.defaultOrgParserState
      { footnoteNumber = 1,
        footnoteNumbers =
          fromList
            [ ("hello", 1)
            ],
        footnoteDefinitions =
          fromList
            [ ( "hello",
                [ PTokenStyledText
                    ( StyledText
                        { body = "Foo.",
                          style = L.styleMaskFrom []
                        }
                    )
                ]
              )
            ],
        prevCellIsFootnote = True
      }

  ps
    "0013-footnotes-complex"
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CParagraph
                [ PTokenStyledText
                    ( StyledText
                        { body = "Hello",
                          style = L.styleMaskFrom []
                        }
                    ),
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "a",
                        name =
                          [ StyledText
                              { body = "1",
                                style = L.styleMaskFrom []
                              }
                          ]
                      },
                  PTokenStyledText
                    ( StyledText
                        { body = " world.",
                          style = L.styleMaskFrom []
                        }
                    ),
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "b",
                        name =
                          [ StyledText
                              { body = "2",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                ],
              CFootnote
                Footnote
                  { name = "b",
                    body =
                      [ PTokenStyledText
                          (StyledText {body = "Definition of b.", style = L.styleMaskFrom []})
                      ]
                  },
              CFootnote
                Footnote
                  { name = "a",
                    body =
                      [ PTokenStyledText
                          (StyledText {body = "Definition of a.", style = L.styleMaskFrom []})
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
          meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
        }
    )
    L.defaultOrgParserState
      { footnoteNumber = 2,
        footnoteNumbers =
          fromList
            [ ("a", 1),
              ("b", 2)
            ],
        footnoteDefinitions =
          fromList
            [ ( "a",
                [ PTokenStyledText
                    ( StyledText
                        { body = "Definition of a.",
                          style = L.styleMaskFrom []
                        }
                    )
                ]
              ),
              ( "b",
                [ PTokenStyledText
                    ( StyledText
                        { body = "Definition of b.",
                          style = L.styleMaskFrom []
                        }
                    )
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
    ( OrgDoc
        { oid = nilOid,
          cells = [],
          footnoteNumbers = M.empty,
          cslPath = Just "foo.csl",
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000"),
                ("cite_export", "csl foo.csl")
              ]
        }
    )
    L.defaultOrgParserState

  ps
    "0014-citations-csl-json-paths"
    ( OrgDoc
        { oid = nilOid,
          cells = [],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths =
            [ "a.json",
              "b.json"
            ],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState

  ps
    "0014-citations-bibliography-location"
    ( OrgDoc
        { oid = nilOid,
          cells = [CBibliography ()],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState

  ps
    "0014-citations-single-cite"
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CParagraph
                [ PTokenCitation
                    ( Citation
                        { styleVariation = (Just "v1", Just "v2"),
                          cites =
                            [ Cite
                                { id = "foo123-.:?!`'/*@+|(){}<>&_^$#%~",
                                  locator = Nothing,
                                  prefix = [],
                                  suffix = []
                                }
                            ],
                          prefix = [],
                          suffix = []
                        }
                    )
                ]
            ],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState

  ps
    "0014-citations-locators"
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CParagraph
                [ PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "a",
                                locator = Just ("page", "15"),
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "b",
                                locator = Just ("page", "15-30"),
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "c",
                                locator = Just ("chapter", "15-30, 35"),
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "d",
                                locator = Just ("section", "1.5-10"),
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "e",
                                locator = Just ("section", "1.22.3"),
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "f",
                                locator = Nothing,
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      }
                ]
            ],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState

  ps
    "0014-citations-locators-affixes"
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CParagraph
                [ PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "a1",
                                locator = Just ("page", "15"),
                                prefix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "prefix",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ],
                                suffix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "suffix",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ]
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "b1",
                                locator = Just ("page", "15-30"),
                                prefix = [],
                                suffix = []
                              },
                            Cite
                              { id = "b2",
                                locator = Just ("chapter", "4"),
                                prefix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "a really long prefix, maybe?",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ],
                                suffix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "more suffix",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ]
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "c1",
                                locator = Just ("chapter", "15-30, 35"),
                                prefix = [],
                                suffix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "suffix with balanced [[brackets] and @sign]",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ]
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "d1",
                                locator = Just ("section", "1.5-10"),
                                prefix = [],
                                suffix = []
                              },
                            Cite
                              { id = "d2",
                                locator = Nothing,
                                prefix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "prefix",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ],
                                suffix = []
                              },
                            Cite
                              { id = "d3",
                                locator = Nothing,
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "e1",
                                locator = Just ("section", "1.22.3"),
                                prefix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "@ ",
                                            style = L.styleMaskFrom []
                                          }
                                      ),
                                    PTokenStyledText
                                      ( StyledText
                                          { body = "see also",
                                            style = L.styleMaskFrom [Italic]
                                          }
                                      )
                                  ],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "f1",
                                locator = Nothing,
                                prefix = [],
                                suffix = []
                              },
                            Cite
                              { id = "f2",
                                locator = Just ("volume", "3-4"),
                                prefix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "foo",
                                            style = L.styleMaskFrom [Bold]
                                          }
                                      ),
                                    PTokenStyledText
                                      ( StyledText
                                          { body = " ",
                                            style = L.styleMaskFrom []
                                          }
                                      ),
                                    PTokenStyledText
                                      ( StyledText
                                          { body = "@fake1",
                                            style = L.styleMaskFrom [Verbatim]
                                          }
                                      ),
                                    PTokenStyledText
                                      ( StyledText
                                          { body = " ",
                                            style = L.styleMaskFrom []
                                          }
                                      ),
                                    PTokenStyledText
                                      ( StyledText
                                          { body = "@fake2",
                                            style = L.styleMaskFrom [Code]
                                          }
                                      )
                                  ],
                                suffix = []
                              }
                          ],
                        prefix = [],
                        suffix = []
                      }
                ]
            ],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState

  ps
    "0014-citations-worg"
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CParagraph
                [ PTokenCitation
                    Citation
                      { styleVariation = (Nothing, Nothing),
                        cites =
                          [ Cite
                              { id = "a1",
                                locator = Nothing,
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix =
                          [ PTokenStyledText
                              ( StyledText
                                  { body = "global",
                                    style = L.styleMaskFrom [Bold]
                                  }
                              ),
                            PTokenStyledText
                              ( StyledText
                                  { body = " prefix",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ],
                        suffix =
                          [ PTokenStyledText
                              ( StyledText
                                  { body = "global ",
                                    style = L.styleMaskFrom []
                                  }
                              ),
                            PTokenStyledText
                              ( StyledText
                                  { body = "suffix",
                                    style = L.styleMaskFrom [Verbatim]
                                  }
                              )
                          ]
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Just "t", Nothing),
                        cites =
                          [ Cite
                              { id = "source1",
                                locator = Nothing,
                                prefix = [],
                                suffix = []
                              },
                            Cite
                              { id = "source2",
                                locator = Nothing,
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix =
                          [ PTokenStyledText
                              ( StyledText
                                  { body = "see",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ],
                        suffix =
                          [ PTokenStyledText
                              ( StyledText
                                  { body = "by Smith ",
                                    style = L.styleMaskFrom []
                                  }
                              ),
                            PTokenStyledText
                              ( StyledText
                                  { body = "et al.",
                                    style = L.styleMaskFrom [Italic]
                                  }
                              )
                          ]
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Just "t", Nothing),
                        cites =
                          [ Cite
                              { id = "foo",
                                locator = Just ("page", "7"),
                                prefix = [],
                                suffix = []
                              },
                            Cite
                              { id = "bar",
                                locator = Just ("page", "4"),
                                prefix = [],
                                suffix = []
                              }
                          ],
                        prefix =
                          [ PTokenStyledText
                              ( StyledText
                                  { body = "see",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ],
                        suffix =
                          [ PTokenStyledText
                              ( StyledText
                                  { body = "by foo",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenCitation
                    Citation
                      { styleVariation = (Just "a", Just "f"),
                        cites =
                          [ Cite
                              { id = "@atkey",
                                locator = Nothing,
                                prefix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "the very important",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ],
                                suffix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "@ once",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ]
                              },
                            Cite
                              { id = "baz",
                                locator = Just ("volume", "3"),
                                prefix =
                                  [ PTokenStyledText
                                      ( StyledText
                                          { body = "the crucial",
                                            style = L.styleMaskFrom []
                                          }
                                      )
                                  ],
                                suffix = []
                              }
                          ],
                        prefix =
                          [ PTokenStyledText
                              ( StyledText
                                  { body = "c.f.",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ],
                        suffix = []
                      }
                ]
            ],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState

  pf
    "0014-citations-unbalanced-brackets-prefix-opening"
    "unbalanced brackets"
    8

  pfm
    "0014-citations-unbalanced-brackets-prefix-closing"
    (utoks "]" <> etoks "@" <> etoks "\n")
    7

  pfSimple "0014-citations-unbalanced-brackets-suffix-opening"

  pfSimple "0014-citations-unbalanced-brackets-suffix-closing"

  pfSimple "0014-citations-extra-semicolons-basic"

  pfSimple "0014-citations-extra-semicolons-consecutive"

  pf
    "0014-citations-nested"
    "unbalanced brackets"
    17

  ps
    "0014-citations-forbidden-in-verbatim-code"
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CParagraph
                [ PTokenStyledText
                    StyledText
                      { body = "[cite:@foo]",
                        style = L.styleMaskFrom [Code]
                      },
                  PTokenStyledText StyledText {body = " ", style = L.styleMaskFrom []},
                  PTokenStyledText
                    StyledText
                      { body = "[cite:@bar]",
                        style = L.styleMaskFrom [Verbatim]
                      }
                ]
            ],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState

  ps
    "0014-citations-forbidden-in-headings"
    ( OrgDoc
        { oid = nilOid,
          cells =
            [ CHeading
                Heading
                  { level = 1,
                    section = [1],
                    body =
                      [ PTokenStyledText
                          StyledText
                            { body = "heading [cite:@foo]",
                              style = L.styleMaskFrom []
                            }
                      ],
                    meta = fromList []
                  }
            ],
          footnoteNumbers = M.empty,
          cslPath = Nothing,
          bibPaths = [],
          meta =
            fromList
              [ ("ID", "00000000-0000-0000-0000-000000000000")
              ]
        }
    )
    L.defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSection = [1]
      }
