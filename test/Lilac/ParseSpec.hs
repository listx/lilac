module Lilac.ParseSpec (spec) where

import Citeproc qualified as CP
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Lilac.Parse qualified as L
import Lilac.Types
  ( BToken (BTokenLinkTarget, BTokenString),
    Block (Block, body, meta, parentOid),
    Cell
      ( CBlock,
        CCommand,
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
    Command (PrintBibliography, PrintGlossary),
    Figure (Figure, caption, link, meta, parentOid),
    Footnote (Footnote, body, name),
    Heading (Heading, body, level, meta, section),
    LCI (LCI),
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
    MathFragment (MathFragment, body, meta, parentOid),
    OrgDoc
      ( OrgDoc,
        bibPaths,
        cells,
        citations,
        cslPath,
        footnoteRefs,
        glossary,
        lineLabels,
        meta,
        oid,
        pseudoEdges
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
        footnoteRefs,
        glossary,
        headingLevel,
        headingOidStack,
        headingSectionNumber,
        lineLabels,
        orgParserConf,
        prevCellIsFootnote,
        tableNumber,
        tanglePaths
      ),
    PToken (PTokenCitation, PTokenLink, PTokenMathFragment, PTokenStyledText),
    Prose (Prose),
    PseudoEdge (PseudoEdge),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (StyledText, body, style),
    Table (Table, caption, grid, meta, parentOid),
    defaultOrgParserState,
    fromWords,
    nilOid,
    nilOidOrgDocPrefix,
    ptext,
    stext,
    styleMaskFrom,
  )
import Lilac.Util qualified as LU
import Network.URI qualified as N
import Relude
  ( Bool (False, True),
    FilePath,
    IO,
    Identity,
    Int,
    Maybe (Just, Nothing),
    String,
    Text,
    Type,
    forM_,
    fromList,
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

spec :: Spec --
spec = do
  it "empty input" $ do
    MP.runParser
      (runStateT L.orgDocParser defaultOrgParserState)
      "empty input"
      nilOidOrgDocPrefix
      `shouldParse` (L.defaultOrgDoc nilOid [], defaultOrgParserState)

  before (LU.readFrom "" "base/lilac.org") $ do
    it "self-parse" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        "base/lilac.org"
        `shouldSucceedOn` input

  shouldParseWith
    "0001-blankline"
    (L.defaultOrgDoc nilOid [])

  shouldParseWith
    "0001-comments-only"
    (L.defaultOrgDoc nilOid [])

  shouldParseWithState
    "0001-blankline"
    (L.defaultOrgDoc nilOid [])
    defaultOrgParserState

  shouldFailWithError
    "0002-doc-metadata-missing-value"
    ( utoks ":\n"
        <> elabel "(doc meta) separator"
        <> elabel "(doc meta) keyword letter"
    )
    7

  shouldParseWith
    "0002-doc-metadata-whitespace-before-value"
    $ L.defaultOrgDoc
      nilOid
      [ ("title", "Parse me without leading whitespace")
      ]

  shouldParseWith
    "0002-doc-metadata"
    $ L.defaultOrgDoc
      nilOid
      [ ("author", "A. U. Thor"),
        ("auto_tangle", "t"),
        ("property", "header-args+ :noweb no-export"),
        ("title", "An Org Document")
      ]

  shouldParseWith
    "0003-doc-metadata-with-comments"
    $ L.defaultOrgDoc
      nilOid
      [ ("author", "A. U. Thor"),
        ("auto_tangle", "t"),
        ("property", "header-args+ :noweb no-export"),
        ("title", "An Org Document")
      ]

  shouldParseWithState
    "0002-doc-metadata-properties-drawer"
    ( L.defaultOrgDoc
        (fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f)
        [ ("ID", "7af1aa3e-41ca-4810-a001-2fdf10768f2f")
        ]
    )
    defaultOrgParserState
      { docOid = fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f
      }

  shouldParseWithState
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

  before
    ( LU.readFrom
        "test/Lilac/ParseSpec/"
        "0002-doc-metadata-properties-drawer-clash"
    )
    $ do
      it "0002-doc-metadata-properties-drawer-clash" $ \input -> do
        MP.runParser
          (runStateT L.orgDocParser defaultOrgParserState)
          "0002-doc-metadata-properties-drawer-clash"
          input
          `shouldFailWith` ( errFancy 181
                               . fancy
                               $ MP.ErrorFail
                                 ( "conflicting keys in the top-level PROPERTIES"
                                     <> " drawer and document metadata"
                                     <> " key/values: [\"foo\"]"
                                 )
                           )

  shouldFailWithMessage
    "0004-headings"
    "found 4 asterisks found for this heading, but lilac_maxHeadingLevel = 3"
    19

  shouldParseWithState
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
        []
    )
    defaultOrgParserState
      { headingOidStack = take 12 $ repeat Nothing,
        headingLevel = 12,
        headingSectionNumber = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        orgParserConf = L.defaultOrgParserConf {maxHeadingLevel = 20}
      }

  shouldParseWithState
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
        []
    )
    defaultOrgParserState
      { headingOidStack = take 12 $ repeat Nothing,
        headingLevel = 12,
        headingSectionNumber = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        orgParserConf = L.defaultOrgParserConf {maxHeadingLevel = 0}
      }

  shouldFailWithMessage
    "0004-headings-limited"
    "found 3 asterisks found for this heading, but lilac_maxHeadingLevel = 2"
    39

  shouldFailWithMessage
    "0004-headings-prohibited"
    "headings prohibited because lilac_maxHeadingLevel < 0"
    29

  shouldFailWithMessage
    "0004-headings-cannot-skip-level"
    "headings may not skip levels (expected to see level 1 or 2, not 4)"
    35

  describe "genSectionNumber" $ do
    it "gets the base case" $ do
      L.genSectionNumber 2 [] `shouldBe` [1, 0]
      L.genSectionNumber 1 [] `shouldBe` [1]
      L.genSectionNumber 0 [] `shouldBe` [1]
      L.genSectionNumber (-1) [] `shouldBe` [1]
    it "bumps the last level if the nesting level matches" $ do
      L.genSectionNumber 1 [1] `shouldBe` [2]
      L.genSectionNumber 2 [1, 1] `shouldBe` [2, 1]
      L.genSectionNumber 3 [1, 1, 1] `shouldBe` [2, 1, 1]
    it "truncates, then increments the (newst) deepest level if we unnest" $ do
      L.genSectionNumber 1 [3, 5] `shouldBe` [6]
      L.genSectionNumber 3 [3, 5, 6, 7] `shouldBe` [6, 6, 7]
    it "grows the section if we nest deeper" $ do
      L.genSectionNumber 3 [2, 2] `shouldBe` [1, 2, 2]
      L.genSectionNumber 5 [2, 2] `shouldBe` [1, 0, 0, 2, 2]

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
    it "adds a new head if heading nests (new heading has an ID)" $ do
      L.genHeadingOidStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        2
        [Nothing]
        `shouldBe` [ Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa,
                     Nothing
                   ]
    it "adds a new head if heading nests (new heading does not have an ID)" $ do
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

  shouldParseWithState
    "0004-properties-drawer-empty"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body = Prose [ptext "foo" []],
                meta = fromList []
              }
        ]
        []
    )
    defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSectionNumber = [1]
      }

  shouldParseWithState
    "0004-properties-drawer-not-empty"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body = Prose [ptext "foo" []],
                meta =
                  fromList
                    [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6"),
                      ("FOO", "BAR")
                    ]
              }
        ]
        []
    )
    defaultOrgParserState
      { headingOidStack =
          [Just $ fromWords 0xb949ea2ee6d1453d 0x93525e85a82cd4e6],
        headingLevel = 1,
        headingSectionNumber = [1]
      }

  shouldParseWithState
    "0004-properties-drawer-not-empty-with-comments"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body = Prose [ptext "foo" []],
                meta =
                  fromList
                    [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6")
                    ]
              }
        ]
        []
    )
    defaultOrgParserState
      { headingOidStack =
          [Just $ fromWords 0xb949ea2ee6d1453d 0x93525e85a82cd4e6],
        headingLevel = 1,
        headingSectionNumber = [1]
      }

  shouldFailWithMessage
    "0005-paragraph-indentation-mismatch-initial-indent-invalid-between-levels"
    "indentation does not fit into existing levels"
    38

  shouldFailWithMessage
    "0005-paragraph-indentation-mismatch-initial-indent-invalid"
    "indentation does not fit into existing levels"
    21

  shouldFailWithError
    "0005-paragraph-indentation-mismatch-over-list-body"
    (utoks " ")
    14

  shouldFailWithError
    "0005-paragraph-indentation-mismatch-over"
    (utoks " ")
    4

  shouldFailWithError
    "0005-paragraph-indentation-mismatch-under-list-body"
    (utoks "b" <> etoks " ")
    12

  shouldParseWith
    "0005-prose-paragraphs"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "a b c d e " [],
              ptext "f g h i j" [Bold]
            ],
        CParagraph $ Prose [ptext "x y z foo bar" []],
        CParagraph $ Prose [ptext "multiple lines with underscore" [Underline]]
      ]
      []

  it "accepts empty input for \"notFollowedBy (char ' ')\"" $ do
    MP.parse
      (MP.notFollowedBy (char ' ') :: Parser ())
      "empty input"
      `shouldSucceedOn` ""

  shouldParseWith
    "0005-styled-strings-bold"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "foo " [],
              ptext "bar" [Bold],
              ptext " baz" []
            ]
      ]
      []

  shouldParseWith
    "0005-styled-strings-complex-punctuation"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "(foo " [],
              ptext "bar" [Verbatim],
              ptext "). What's up? and " [],
              ptext "that!" [Bold],
              ptext " " [],
              ptext "(italic)" [Italic],
              ptext ". It's #1!" []
            ]
      ]
      []

  shouldParseWith
    "0005-styled-strings-slash-separated-words"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "italic fraction 1/2" [Italic],
              ptext " foo and/or bar " [],
              ptext "A+/B-" [Italic],
              ptext " +1/+2. key/value" []
            ]
      ]
      []

  shouldParseWith
    "0005-styled-strings-complex"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "foo " [Bold],
              ptext "bar " [Bold, Italic],
              ptext "underlined bold italic" [Bold, Italic, Underline],
              ptext " baz" [Bold],
              ptext " " [],
              ptext "verbatim  text" [Verbatim],
              ptext " " [],
              ptext "x  y  z" [Code],
              ptext " " [],
              ptext "bold" [Bold]
            ]
      ]
      []

  shouldParseWith
    "0005-styled-strings-compression"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "a b c d e " [],
              ptext "f g h i j" [Bold]
            ]
      ]
      []

  shouldParseWith
    "0005-styled-strings-escape-marker"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "foo and/or 1/2 bar \\ a\\b" [Italic]
            ]
      ]
      []

  shouldFailWithMessage
    "0005-styled-strings-leftover-active-styles"
    "leftover active styles: [Bold]"
    15

  shouldParseWithState
    "0005-styled-strings-messy-boundaries"
    ( nilOrgDoc
        [ CHeading
            Heading
              { level = 1,
                section = [1],
                body =
                  Prose
                    [ ptext "foo " [Bold],
                      ptext "bar baz a" [],
                      ptext "b" [Bold],
                      ptext "c (" [],
                      ptext "ok" [Bold],
                      ptext ") yes" []
                    ],
                meta = fromList []
              }
        ]
        []
    )
    defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSectionNumber = [1]
      }

  shouldParseWith
    "0005-styled-strings-no-style"
    $ nilOrgDoc
      [ plainParagraph "foo"
      ]
      []

  shouldParseWith
    "0005-styled-strings-verbatim-code-ignore-immediate-bold"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "*a" [Verbatim],
              ptext " " [],
              ptext "b*" [Verbatim],
              ptext " " [],
              ptext "*c" [Code],
              ptext " " [],
              ptext "d*" [Code]
            ]
      ]
      []

  shouldParseWith
    "0005-styled-strings-verbatim-precedence"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "[[foo" [Verbatim]
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
                      [ stext "hello" []
                      ]
                  },
              ptext " there" []
            ],
        CParagraph
          $ Prose
            [ ptext "\\(1 + 1 " [Verbatim],
              ptext " " [],
              ptext "\\(2 * 2\\)" [Code]
            ],
        CParagraph
          $ Prose
            [ ptext "[[bar]]" [Code]
            ]
      ]
      [PseudoEdge (LCI 1, LinkOid {oid = fromWords 0 0, name = "hello"})]

  shouldParseWith
    "0005-styled-strings-verbatim"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "(" [],
              ptext "import ./." [Verbatim],
              ptext ") hello" []
            ]
      ]
      []

  shouldParseWith
    "0005-styled-strings-escaping"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "\\" [Verbatim],
              ptext " " [],
              ptext "=" [Verbatim],
              ptext " " [],
              ptext "\\(" [Verbatim],
              ptext " " [],
              ptext "\\" [Code],
              ptext " " [],
              ptext "~" [Code],
              ptext " " [],
              ptext "\\(" [Code]
            ]
      ]
      []

  shouldParseWith
    "0006-links-inside-parentheses"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "(" [],
              PTokenLink
                Link
                  { target = LinkTargetFilePath "foo/bar",
                    name = [stext "foo/bar" []]
                  },
              ptext ")" []
            ]
      ]
      []

  shouldParseWith
    "0006-links-nonlink"
    $ nilOrgDoc
      [ CParagraph $ Prose [ptext "[hi]" []]
      ]
      []

  shouldParseWith
    "0006-links-raw"
    $ nilOrgDoc
      [ CParagraph
          $ Prose
            [ ptext "See " [],
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
                    name = [stext "https://foo.org" []]
                  },
              ptext ", or " [],
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
                    name = [stext "https://bar.net" []]
                  },
              ptext ". Try " [],
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
                    name = [stext "http://localhost:3000" []]
                  },
              ptext "? Go to " [],
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
                    name = [stext "ftp://10.0.0.1" []]
                  },
              ptext "! " [],
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
                                    N.uriRegName = "a.com",
                                    N.uriPort = ""
                                  },
                            N.uriPath = "/path/to/something/",
                            N.uriQuery = "",
                            N.uriFragment = ""
                          },
                    name = [stext "https://a.com/path/to/something/" []]
                  }
            ]
      ]
      []

  shouldParseWithState
    "0006-links"
    ( nilOrgDoc
        [ CParagraph
            $ Prose
              [ ptext "foo " [],
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "link-target-only"
                            },
                      name = [stext "link-target-only" []]
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
                            name = [stext "link-name" []]
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
                      name = [stext "a" []]
                    },
                ptext " b" []
              ]
        ]
        [ PseudoEdge (LCI 2, LinkOid {oid = fromWords 0x0000000000000000 0x0000000000000000, name = "a"}),
          PseudoEdge (LCI 0, LinkOid {oid = fromWords 0 0, name = "link-target-only"})
        ]
    )
    defaultOrgParserState
      { headingOidStack = [Nothing],
        headingLevel = 1,
        headingSectionNumber = [1]
      }

  shouldParseWith
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
                      name = [stext "http://foo.org" []]
                    },
                ptext " " [],
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
                      name = [stext "https://foo.org" []]
                    },
                ptext " " [],
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
                      name = [stext "hello" []]
                    }
              ]
        ]
        []
    )

  shouldParseWith
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
                        [ stext "id:442c807f-c9d9-45a7-88bf-ffffffffffff" []
                        ]
                    },
                ptext " " [],
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ stext "bar" []
                        ]
                    },
                ptext " " [],
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ stext "bar" [],
                          stext " " [],
                          stext "[]" [Verbatim]
                        ]
                    },
                ptext " " [],
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = ""
                            },
                      name =
                        [ stext "bar" [],
                          stext " " [],
                          stext "ok" []
                        ]
                    },
                ptext " " [],
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = "foo"
                            },
                      name = [stext "abc" []]
                    },
                ptext " " [],
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = nilOid,
                              name = "foo"
                            },
                      name = [stext "abc" []]
                    }
              ],
          CListItem
            [ LUnorderedStart,
              LItemStart
            ],
          CParagraph
            $ Prose
              [ ptext "link name across list indentation " [],
                PTokenLink
                  Link
                    { name =
                        [ stext "italic" [Italic],
                          stext " " [],
                          stext "def" [],
                          stext " " [],
                          stext "ghi]" [],
                          stext " " [],
                          stext "!" []
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
              [ ptext "link target with spaces " [],
                PTokenLink
                  Link
                    { name = [stext "foo bar baz" []],
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
              [ ptext "oid with spaces " [],
                PTokenLink
                  Link
                    { name = [stext "a b c" []],
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
        [ PseudoEdge (LCI 6, LinkOid {oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff, name = "a b c"}),
          PseudoEdge (LCI 4, LinkOid {oid = fromWords 0x0000000000000000 0x0000000000000000, name = "foo bar baz"}),
          PseudoEdge (LCI 2, LinkOid {oid = fromWords 0x0000000000000000 0x0000000000000000, name = "foo"}),
          PseudoEdge (LCI 0, LinkOid {oid = fromWords 0x0000000000000000 0x0000000000000000, name = "foo"}),
          PseudoEdge (LCI 0, LinkOid {oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff, name = "foo"}),
          PseudoEdge (LCI 0, LinkOid {oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff, name = ""}),
          PseudoEdge (LCI 0, LinkOid {oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff, name = ""}),
          PseudoEdge (LCI 0, LinkOid {oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff, name = ""}),
          PseudoEdge (LCI 0, LinkOid {oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff, name = ""})
        ]
    )

  shouldParseWith
    "0006-links-oids-lenient-whitespace"
    ( nilOrgDoc
        [ CParagraph
            $ Prose
              [ ptext "Hello " [],
                PTokenLink
                  Link
                    { target =
                        LinkTargetOid
                          LinkOid
                            { oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff,
                              name = "target name with newline and space is still treated as space"
                            },
                      name = [stext "foo" []]
                    }
              ]
        ]
        [PseudoEdge (LCI 0, LinkOid {oid = fromWords 0x442c807fc9d945a7 0x88bfffffffffffff, name = "target name with newline and space is still treated as space"})]
    )

  shouldFailWithMessage
    "0006-unique-line-labels"
    "line label \"x\" is not unique"
    58

  shouldFailWithMessage
    "0007-block-src-without-lang"
    "block type src requires a language specifier"
    11

  shouldParseWithState
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
              },
          CBlock
            Block
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__anon_block_number", "3"),
                      ("__block_type", "src"),
                      ("__src_language", "haskell"),
                      ("tangle", "foo.hs")
                    ],
                body = []
              }
        ]
        []
    )
    defaultOrgParserState
      { cellNames = Set.fromList ["foo"],
        anonBlockNumber = 3,
        tanglePaths = fromList ["foo.hs"]
      }

  shouldParseWithState
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
        []
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

  shouldParseWithState
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
        []
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  shouldParseWithState
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
                  [ BTokenLinkTarget
                      $ LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "foo"
                          },
                    BTokenString "\n\n<<quux\n>>\necho ", --
                    BTokenLinkTarget
                      $ LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "bar"
                          },
                    BTokenString " x y z"
                  ]
              }
        ]
        []
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  shouldParseWithState
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
                    BTokenLinkTarget
                      $ LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "foo"
                          },
                    BTokenString ") and \"",
                    BTokenLinkTarget
                      $ LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "bar"
                          },
                    BTokenString "\" and <",
                    BTokenLinkTarget
                      $ LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "ok"
                          },
                    BTokenString "> ",
                    BTokenLinkTarget
                      $ LinkTargetOid
                        LinkOid
                          { oid = nilOid,
                            name = "a->b"
                          }
                  ]
              }
        ]
        []
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  shouldFailWithMessage
    "0007-blocks-duplicate-names"
    "cell name \"foo\" is not unique"
    52

  shouldFailWithMessage
    "0007-blocks-duplicate-tangle-paths"
    "tangle path \"some/path.sh\" is not unique"
    101

  shouldParseWithState
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
        []
    )
    defaultOrgParserState
      { anonBlockNumber = 1
      }

  shouldFailWithError
    "0007-blocks-no-name-with-opening-parenthesis"
    (utoks "(" <> etoks " ")
    8

  shouldFailWithMessage
    "0008-indentation-comment"
    "invalid indentation"
    1

  shouldFailWithMessage
    "0008-indentation-headings"
    "invalid indentation"
    1

  shouldFailWithMessage
    "0008-indentation-paragraph"
    "invalid indentation"
    1

  shouldParseWith
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
      []

  shouldParseWith
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
      []

  shouldParseWith
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
      []

  shouldFailWithMessage
    "0009-list-nesting-max"
    "max nesting depth exceeded: cannot have more than 3 levels of list items"
    49

  shouldFailWithMessage
    "0009-list-nesting-max-8"
    "max nesting depth exceeded: cannot have more than 8 levels of list items"
    228

  shouldParseWith
    "0009-list-indentation-no-indent"
    $ nilOrgDoc
      [ plainParagraph "1)",
        plainParagraph "2)"
      ]
      []

  shouldParseWith
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
      []

  shouldParseWith
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
      []

  shouldFailWithMessage
    "0009-lists-cannot-be-mixed-on-their-own-ordered"
    "expected ORDERED list item at this level"
    25

  shouldFailWithMessage
    "0009-lists-cannot-be-mixed-on-their-own"
    "expected UNORDERED list item at this level"
    23

  shouldFailWithMessage
    "0009-lists-require-precise-indentation"
    "list item: invalid indentation"
    15

  shouldFailWithMessage
    "0009-ordered-lists-require-increment"
    "expected incremented list head 2 but got 1"
    13

  shouldFailWithMessage
    "0009-ordered-lists-start-from-1"
    "starting list head number must be 1; got 20"
    19

  shouldFailWithError
    "0009-ordered-list-head-needs-parenthesis"
    (utoks "." <> etoks ")" <> elabel "ordered list number")
    3

  shouldFailWithMessage
    "0009-ordered-list-head-cannot-use-letters"
    "invalid indentation"
    2

  shouldParseWith
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
      []

  shouldParseWith
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
      []

  shouldParseWithState
    "0010-standalone-math-fragments"
    ( nilOrgDoc
        [ CMathFragment
            MathFragment
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__equation_number", "1")
                    ],
                body = "1 + 1 = 2"
              },
          CMathFragment
            MathFragment
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__equation_number", "2")
                    ],
                body = "1 + 1 = 2"
              },
          CMathFragment
            MathFragment
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__equation_anon_number", "1")
                    ],
                body = "2 + 2 = 4"
              },
          CMathFragment
            MathFragment
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("name", "eq:x")
                    ],
                body = "2 + 2 = 4"
              },
          CMathFragment
            MathFragment
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__equation_number", "3"),
                      ("name", "eq:y")
                    ],
                body = "1 + 1 = 2"
              }
        ]
        []
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

  shouldParseWithState
    "0010-standalone-math-inside-lists"
    ( nilOrgDoc
        [ CListItem
            [ LOrderedStart,
              LItemStart
            ],
          CParagraph $ Prose [ptext "foo" []],
          CMathFragment
            MathFragment
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("name", "eq:x")
                    ],
                body = "2 + 2 = 4"
              },
          CMathFragment
            MathFragment
              { parentOid = nilOid,
                meta =
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
        []
    )
    defaultOrgParserState
      { equationNumber = 1,
        cellNames =
          Set.fromList
            [ "eq:x",
              "eq:y"
            ]
      }

  shouldParseWithState
    "0010-inline-math"
    ( nilOrgDoc
        [ CParagraph
            $ Prose
              [ PTokenMathFragment
                  MathFragment
                    { parentOid = nilOid,
                      meta = fromList [],
                      body = "1 + 1"
                    },
                ptext " is the same as " [],
                PTokenMathFragment
                  MathFragment
                    { parentOid = nilOid,
                      meta = fromList [],
                      body = "2"
                    },
                ptext ", better known as " [],
                PTokenMathFragment
                  MathFragment
                    { parentOid = nilOid,
                      meta = fromList [],
                      body = "1 + 1 = 2"
                    },
                ptext "." []
              ]
        ]
        []
    )
    defaultOrgParserState

  shouldParseWithState
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
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__figure_number", "1"),
                      ("width", "100%"),
                      ("name", "fig:hello"),
                      ("caption", "=hello=")
                    ],
                caption =
                  Prose
                    [ ptext "hello" [Verbatim]
                    ],
                link =
                  Link
                    { target = LinkTargetFilePath "image/hello.png",
                      name = [stext "image/hello.png" []]
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
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__figure_number", "2")
                    ],
                caption = Prose [],
                link =
                  Link
                    { target = LinkTargetFilePath "image/world.png",
                      name = [stext "image/world.png" []]
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
                      name = [stext "not-figure" []]
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
                      name = [stext "not-a-figure-2" []]
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
                      name = [stext "not a figure" []]
                    }
              ],
          CFigure
            Figure
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("__figure_number", "3")
                    ],
                caption = Prose [],
                link =
                  Link
                    { target = LinkTargetFilePath "foo.png",
                      name = [stext "foo.png" []]
                    }
              }
        ]
        [PseudoEdge (LCI 8, LinkOid {oid = fromWords 0 0, name = "not a figure"})]
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

  shouldParseWithState
    "0012-tables-and-prose"
    ( nilOrgDoc
        [ CTable
            Table
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("name", "tbl:famous-eqs"),
                      ( "caption",
                        "*Bold*. [[tbl:famous-eqs][A self-link]]. \\(E = mc^2\\)"
                      ),
                      ("__table_number", "1")
                    ],
                caption =
                  Prose
                    [ ptext "Bold" [Bold],
                      ptext "." [],
                      ptext " " [],
                      PTokenLink
                        Link
                          { name =
                              [ stext "A" [],
                                stext " " [],
                                stext "self-link" []
                              ],
                            target =
                              LinkTargetOid
                                LinkOid {oid = nilOid, name = "tbl:famous-eqs"}
                          },
                      ptext "." [],
                      ptext " " [],
                      PTokenMathFragment
                        MathFragment
                          { parentOid = nilOid,
                            meta = fromList [],
                            body = "E = mc^2"
                          }
                    ],
                grid =
                  fromList
                    [ ( (1, 1),
                        Prose [ptext "Name" []]
                      ),
                      ( (1, 2),
                        Prose [ptext "Equation" []]
                      ),
                      ( (1, 3),
                        Prose [ptext "Link" []]
                      ),
                      ( (2, 1),
                        L.tableHorizontalLine
                      ),
                      ( (3, 1),
                        Prose [ptext "Pythagoras | Foo" []]
                      ),
                      ( (3, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { parentOid = nilOid,
                                  meta = fromList [],
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
                                  name = [stext "Wiki" []]
                                }
                          ]
                      ),
                      ( (4, 1),
                        Prose
                          [ ptext "Albert " [],
                            ptext "Einstein" [Bold]
                          ]
                      ),
                      ( (4, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { parentOid = nilOid,
                                  meta = fromList [],
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
                                  name = [stext "Wiki" []]
                                }
                          ]
                      ),
                      ((5, 1), Prose [ptext "Unknown" []]),
                      ((5, 2), Prose []),
                      ((5, 3), Prose []),
                      ((6, 1), Prose [ptext "_" []]),
                      ((6, 2), Prose [ptext "*" []]),
                      ((6, 3), Prose [ptext "/" []])
                    ]
              }
        ]
        []
    )
    defaultOrgParserState
      { cellNames =
          Set.fromList
            [ "tbl:famous-eqs"
            ],
        tableNumber = 1
      }

  shouldParseWithState
    "0012-tables-inside-lists"
    ( nilOrgDoc
        [ CListItem
            [ LOrderedStart,
              LItemStart
            ],
          CParagraph $ Prose [ptext "foo" []],
          CTable
            Table
              { parentOid = nilOid,
                meta =
                  fromList
                    [ ("name", "tbl:famous-equations"),
                      ("bar", "baz"),
                      ("__table_number", "1")
                    ],
                caption = Prose [],
                grid =
                  fromList
                    [ ( (1, 1),
                        Prose [ptext "Name" []]
                      ),
                      ( (1, 2),
                        Prose [ptext "Equation" []]
                      ),
                      ( (1, 3),
                        Prose [ptext "Link" []]
                      ),
                      ( (2, 1),
                        L.tableHorizontalLine
                      ),
                      ( (3, 1),
                        Prose [ptext "Pythagoras | Foo" []]
                      ),
                      ( (3, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { parentOid = nilOid,
                                  meta = fromList [],
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
                                  name = [stext "Wiki" []]
                                }
                          ]
                      ),
                      ( (4, 1),
                        Prose
                          [ ptext "Albert " [],
                            ptext "Einstein" [Bold]
                          ]
                      ),
                      ( (4, 2),
                        Prose
                          [ PTokenMathFragment
                              MathFragment
                                { parentOid = nilOid,
                                  meta = fromList [],
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
                                  name = [stext "Wiki" []]
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
        []
    )
    defaultOrgParserState
      { cellNames =
          Set.fromList
            [ "tbl:famous-equations"
            ],
        tableNumber = 1
      }

  shouldFailWithMessage
    "0013-footnotes-broken-link"
    "broken footnote links: hello"
    24

  shouldFailWithMessage
    "0013-footnotes-unused-def"
    "unused footnote definitions: hello"
    30

  shouldParseWithState
    "0013-footnotes-simple"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ ptext "Hello" [],
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "hello",
                        name = [stext "1" []]
                      },
                  ptext " world." []
                ],
            CFootnote
              Footnote
                { name = "hello",
                  body = Prose [ptext "Foo." []]
                }
          ],
        pseudoEdges = [],
        footnoteRefs =
          fromList
            [ ("hello", (1, LCI 0))
            ],
        glossary = M.empty,
        lineLabels = M.empty,
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
      }
    defaultOrgParserState
      { footnoteNumber = 1,
        footnoteRefs =
          fromList
            [ ("hello", (1, LCI 0))
            ],
        footnoteDefinitions =
          fromList
            [ ( "hello",
                Prose [ptext "Foo." []]
              )
            ],
        prevCellIsFootnote = True,
        glossary = M.empty,
        lineLabels = M.empty
      }

  shouldParseWithState
    "0013-footnotes-complex"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ ptext "Hello" [],
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "a",
                        name = [stext "1" []]
                      },
                  ptext " world." [],
                  PTokenLink
                    Link
                      { target = LinkTargetFootnote "b",
                        name = [stext "2" []]
                      }
                ],
            CFootnote
              Footnote
                { name = "b",
                  body = Prose [ptext "Definition of b." []]
                },
            CFootnote
              Footnote
                { name = "a",
                  body = Prose [ptext "Definition of a." []]
                }
          ],
        pseudoEdges = [],
        footnoteRefs =
          fromList
            [ ("a", (1, LCI 0)),
              ("b", (2, LCI 0))
            ],
        glossary = M.empty,
        lineLabels = M.empty,
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
      }
    defaultOrgParserState
      { footnoteNumber = 2,
        footnoteRefs =
          fromList
            [ ("a", (1, LCI 0)),
              ("b", (2, LCI 0))
            ],
        footnoteDefinitions =
          fromList
            [ ("a", Prose [ptext "Definition of a." []]),
              ("b", Prose [ptext "Definition of b." []])
            ],
        prevCellIsFootnote = True,
        glossary = M.empty,
        lineLabels = M.empty
      }

  shouldFailWithMessage
    "0013-footnotes-multiple-groups"
    "cannot have more than one group of footnote definitions"
    95

  shouldParseWithState
    "0014-citations-csl-path"
    OrgDoc
      { oid = nilOid,
        cells = [],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = [],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  shouldParseWithState
    "0014-citations-csl-json-paths"
    OrgDoc
      { oid = nilOid,
        cells = [],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
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

  shouldParseWithState
    "0014-citations-bibliography-location"
    OrgDoc
      { oid = nilOid,
        cells = [CCommand PrintBibliography],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
        cslPath = Just "foo.csl",
        bibPaths = ["a.json"],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  shouldFailWithMessage
    "0014-citations-print-biblio-missing-csl-style"
    "cannot print bibliography without '#+cite_export: csl <style>.csl' in top-level document metadata"
    44

  shouldFailWithMessage
    "0014-citations-print-biblio-missing-bib-entries"
    "cannot print bibliography without '#+bibliography: <bib_entries>.json' in top-level document metadata"
    48

  shouldParseWithState
    "0014-citations-single-cite"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0
                ]
          ],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
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

  shouldParseWithState
    "0014-citations-locators"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0,
                  ptext " " [],
                  PTokenCitation 1,
                  ptext " " [],
                  PTokenCitation 2,
                  ptext " " [],
                  PTokenCitation 3,
                  ptext " " [],
                  PTokenCitation 4,
                  ptext " " [],
                  PTokenCitation 5
                ]
          ],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
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

  shouldParseWithState
    "0014-citations-locators-affixes"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0,
                  ptext " " [],
                  PTokenCitation 1,
                  ptext " " [],
                  PTokenCitation 2,
                  ptext " " [],
                  PTokenCitation 3,
                  ptext " " [],
                  PTokenCitation 4,
                  ptext " " [],
                  PTokenCitation 5
                ]
          ],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
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
                        prefix = Prose [ptext "prefix" []],
                        suffix = Prose [ptext "suffix" []]
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
                        prefix = Prose [ptext "a really long prefix, maybe?" []],
                        suffix = Prose [ptext "more suffix" []]
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
                            [ ptext "suffix with balanced [[brackets] and @sign]" []
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
                        prefix = Prose [ptext "prefix" []],
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
                            [ ptext "@ " [],
                              ptext "see also" [Italic]
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
                            [ ptext "foo" [Bold],
                              ptext " " [],
                              ptext "@fake1" [Verbatim],
                              ptext " " [],
                              ptext "@fake2" [Code]
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

  shouldParseWithState
    "0014-citations-worg"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ PTokenCitation 0,
                  ptext " " [],
                  PTokenCitation 1,
                  ptext " " [],
                  PTokenCitation 2,
                  ptext " " [],
                  PTokenCitation 3
                ]
          ],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
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
                    [ ptext "global" [Bold],
                      ptext " prefix" []
                    ],
                suffix =
                  Prose
                    [ ptext "global " [],
                      ptext "suffix" [Verbatim]
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
                prefix = Prose [ptext "see" []],
                suffix =
                  Prose
                    [ ptext "by Smith " [],
                      ptext "et al." [Italic]
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
                prefix = Prose [ptext "see" []],
                suffix = Prose [ptext "by foo" []]
              },
            Citation
              { styleVariation = (Just "a", Just "f"),
                noteNumber = Nothing,
                cites =
                  [ Cite
                      { id = "@atkey",
                        locator = Nothing,
                        prefix = Prose [ptext "the very important" []],
                        suffix = Prose [ptext "@ once" []]
                      },
                    Cite
                      { id = "baz",
                        locator = Just ("volume", "3"),
                        prefix = Prose [ptext "the crucial" []],
                        suffix = Prose []
                      }
                  ],
                prefix = Prose [ptext "c.f." []],
                suffix = Prose []
              }
          ],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  shouldFailWithMessage
    "0014-citations-unbalanced-brackets-prefix-opening"
    "unbalanced brackets"
    58

  shouldFailWithError
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

  shouldFail "0014-citations-unbalanced-brackets-suffix-opening"

  shouldFailWithError
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

  shouldFailWithError
    "0014-citations-extra-semicolons-basic"
    ( utoks ";"
        <> etoks "@"
        <> elabel "intra-cell whitespace"
    )
    56

  shouldFailWithError
    "0014-citations-extra-semicolons-consecutive"
    ( utoks ";"
        <> etoks "@"
        <> etoks "]"
        <> elabel "intra-cell whitespace"
    )
    61

  shouldFailWithMessage
    "0014-citations-nested"
    "unbalanced brackets"
    67

  shouldParseWithState
    "0014-citations-forbidden-in-verbatim-code"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CParagraph
              $ Prose
                [ ptext "[cite:@foo]" [Code],
                  ptext " " [],
                  ptext "[cite:@bar]" [Verbatim]
                ]
          ],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  shouldParseWithState
    "0014-citations-forbidden-in-headings"
    OrgDoc
      { oid = nilOid,
        cells =
          [ CHeading
              Heading
                { level = 1,
                  section = [1],
                  body = Prose [ptext "heading [cite:@foo]" []],
                  meta = fromList []
                }
          ],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
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
        headingSectionNumber = [1]
      }

  shouldFailWithMessage
    "0014-citations-missing-csl-style"
    "cannot parse citations without '#+cite_export: csl <style>.csl' in top-level document metadata"
    28

  shouldFailWithMessage
    "0014-citations-missing-bib-entries"
    "cannot parse citations without '#+bibliography: <bib_entries>.json' in top-level document metadata"
    32

  describe "Text to Prose round trip" $ do
    checkTextToProseRoundTrip
      "can handle StyledText"
      [ ( "no style",
          Prose [ptext "no style" []]
        ),
        ( "*bold*",
          Prose [ptext "bold" [Bold]]
        ),
        ( "*bo\\*ld (escaped)*",
          Prose [ptext "bo*ld (escaped)" [Bold]]
        ),
        ( "/italic/",
          Prose [ptext "italic" [Italic]]
        ),
        ( "/it\\/alic (escaped)/",
          Prose [ptext "it/alic (escaped)" [Italic]]
        ),
        ( "_underline_",
          Prose [ptext "underline" [Underline]]
        ),
        ( "_under\\_line (escaped)_",
          Prose [ptext "under_line (escaped)" [Underline]]
        ),
        ( "=*/_~verbatim=",
          Prose [ptext "*/_~verbatim" [Verbatim]]
        ),
        ( "~*/_=code~",
          Prose [ptext "*/_=code" [Code]]
        )
      ]
    checkTextToProseRoundTrip
      "can handle OID links"
      [ ( "[[id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaa1]]",
          Prose
            [ PTokenLink
                Link
                  { name = [stext "id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaa1" []],
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
                  { name = [stext "foo" [Bold]],
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
                  { name = [stext "bar" [Italic]],
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
                  { name = [stext "1" []],
                    target = LinkTargetFootnote "abc"
                  }
            ]
        ),
        ( "[fn:abc][fn:xyz][fn:abc]",
          Prose
            [ PTokenLink
                Link
                  { name = [stext "1" []],
                    target = LinkTargetFootnote "abc"
                  },
              PTokenLink
                Link
                  { name = [stext "2" []],
                    target = LinkTargetFootnote "xyz"
                  },
              PTokenLink
                Link
                  { name = [stext "1" []],
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
                  { name = [stext "bar" []],
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
                  { name = [stext "https://foo2.org" []],
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
                  { name = [stext "foo/bar" []],
                    target = LinkTargetFilePath "foo/bar"
                  }
            ]
        ),
        ( "[[file:foo/bar][baz]]",
          Prose
            [ PTokenLink
                Link
                  { name = [stext "baz" []],
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
                  { parentOid = nilOid,
                    body = "4 = 2 * 2",
                    meta = fromList []
                  }
            ]
        )
      ]

  describe "Drop text chars from Prose" $ do
    it "can handle plain text from the front" $ do
      CP.dropTextWhile (== 'a') (CP.fromText "aa b *cc*" :: Prose)
        `shouldBe` (CP.fromText " b *cc*")
    it "can handle plain text from the back" $ do
      CP.dropTextWhileEnd (== 'c') (CP.fromText "aa b *cc*" :: Prose)
        `shouldBe` (CP.fromText "aa b ")
    it "can handle styled (italic) text from the front" $ do
      CP.dropTextWhile (== 'a') (CP.fromText "/aa/ b *cc*" :: Prose)
        `shouldBe` (CP.fromText " b *cc*")
    it "can handle styled (italic) text from the back" $ do
      CP.dropTextWhileEnd (== 'c') (CP.fromText "/aa/ b *cc*" :: Prose)
        `shouldBe` (CP.fromText "/aa/ b ")
    it "skips over non-text elements from the front" $ do
      CP.dropTextWhile (== 'a') (CP.fromText "[[foo]]/aa/ b *cc*" :: Prose)
        `shouldBe` (CP.fromText "[[foo]] b *cc*")
    it "skips over non-text elements from the back" $ do
      CP.dropTextWhileEnd (== 'c') (CP.fromText "/aa/ b *cc*[[foo]]" :: Prose)
        `shouldBe` (CP.fromText "/aa/ b [[foo]]")

  shouldFailWithMessage
    "0015-glossary-duplicate-term"
    "glossary term \"foo\" is not unique"
    14

  shouldParseWithState
    "0015-glossary-location"
    OrgDoc
      { oid = nilOid,
        cells = [CCommand $ PrintGlossary False],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  shouldParseWithState
    "0015-glossary-location-global"
    OrgDoc
      { oid = nilOid,
        cells = [CCommand $ PrintGlossary True],
        pseudoEdges = [],
        footnoteRefs = M.empty,
        glossary = M.empty,
        lineLabels = M.empty,
        cslPath = Nothing,
        bibPaths = [],
        citations = [],
        meta =
          fromList
            [ ("ID", "00000000-0000-0000-0000-000000000000")
            ]
      }
    defaultOrgParserState

  shouldFailWithMessage
    "0016-metadata-reserved-underscores"
    "cannot use leading double underscores for metadata key: \"__block_type\""
    2

type Parser :: Type -> Type
type Parser = MP.ParsecT Void Text Identity --

shouldParseWith :: FilePath -> OrgDoc -> Spec
shouldParseWith path expectedOrgDoc = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> parseAs input path expected --
  where
    expected = (expectedOrgDoc, defaultOrgParserState)

shouldParseWithState :: FilePath -> OrgDoc -> OrgParserState -> Spec
shouldParseWithState path expectedOrgDoc expectedState = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> parseAs input path expected
  where
    expected = (expectedOrgDoc, expectedState)

parseAs ::
  Text ->
  FilePath ->
  (OrgDoc, OrgParserState) ->
  IO ()
parseAs input path expected@(doc, _) =
  MP.runParser
    (runStateT L.orgDocParser defaultOrgParserState)
    path
    input'
    `shouldParse` expected
  where
    input' =
      if doc.oid == nilOid
        then nilOidOrgDocPrefix <> input
        else input

shouldFailWithMessage :: FilePath -> String -> Int -> Spec
shouldFailWithMessage path errMsg pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        (inputPrefix <> input)
        `shouldFailWith` (errFancy newPos . fancy $ MP.ErrorFail errMsg) --
  where
    inputPrefix = nilOidOrgDocPrefix
    newPos = pos + T.length inputPrefix

shouldFailWithError :: FilePath -> ET Text -> Int -> Spec
shouldFailWithError path errorTrivial pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        (inputPrefix <> input)
        `shouldFailWith` (err newPos errorTrivial)
  where
    inputPrefix = nilOidOrgDocPrefix
    newPos = pos + T.length inputPrefix

shouldFail :: FilePath -> Spec
shouldFail path = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser defaultOrgParserState)
        path
        `shouldFailOn` (inputPrefix <> input)
  where
    inputPrefix = nilOidOrgDocPrefix

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

nilOrgDoc :: [Cell] -> [PseudoEdge] -> OrgDoc
nilOrgDoc cells pseudoEdges =
  OrgDoc
    { oid = nilOid,
      cells = cells,
      pseudoEdges = pseudoEdges,
      footnoteRefs = M.empty,
      glossary = M.empty,
      lineLabels = M.empty,
      cslPath = Nothing,
      bibPaths = [],
      citations = [],
      meta = fromList [("ID", "00000000-0000-0000-0000-000000000000")]
    }

checkTextToProseRoundTrip :: String -> [(Text, Prose)] -> Spec
checkTextToProseRoundTrip msg testCases = do
  it msg $ do
    forM_ testCases $ \(text, prose) -> do
      CP.fromText text `shouldBe` prose
      CP.toText prose `shouldBe` text
