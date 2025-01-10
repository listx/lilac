module Lilac.ParseSpec (spec) where

import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Lilac.Parse qualified as L
import Relude
  ( FilePath,
    Int,
    String,
    decodeUtf8,
    fromList,
    readFileBS,
    runStateT,
    ($),
    (.),
    (<>),
  )
import Test.Hspec (Spec, before, describe, it)
import Test.Hspec.Megaparsec
  ( ET,
    elabel,
    err,
    errFancy,
    etoks,
    fancy,
    shouldFailWith,
    shouldParse,
    utoks,
  )
import Text.Megaparsec qualified as MP

p :: String -> FilePath -> L.OrgDoc -> Spec
p behavior path expected = do
  before (readFileBS path) $ do
    it behavior $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldParse` (expected, L.defaultOrgParserState)

ps :: String -> FilePath -> L.OrgDoc -> L.OrgParserState -> Spec
ps behavior path expected state = do
  before (readFileBS path) $ do
    it behavior $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldParse` (expected, state)

pf :: String -> FilePath -> String -> Int -> Spec
pf behavior path errMsg pos = do
  before (readFileBS path) $ do
    it behavior $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldFailWith` (errFancy pos . fancy $ MP.ErrorFail errMsg)

pfm :: String -> FilePath -> ET T.Text -> Int -> Spec
pfm behavior path errorTrivial pos = do
  before (readFileBS path) $ do
    it behavior $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldFailWith` (err pos errorTrivial)

spec :: Spec
spec =
  describe "parse" $ do
    it "parses empty input as defaultOrgDoc" $ do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        "empty input"
        ""
        `shouldParse` (L.defaultOrgDoc, L.defaultOrgParserState)
    p
      "parses blank line as defaultOrgDoc"
      "test/Lilac/ParseSpec/0001-blankline"
      L.defaultOrgDoc
    p "parses document metadata" "test/Lilac/ParseSpec/0002-doc-metadata"
      $ L.defaultOrgDocWithMeta
        [ ("author", "A. U. Thor"),
          ("auto_tangle", "t"),
          ("property", "header-args+ :noweb no-export"),
          ("title", "An Org Document")
        ]

    p
      "parses document metadata (skips comments)"
      "test/Lilac/ParseSpec/0003-doc-metadata-with-comments"
      $ L.defaultOrgDocWithMeta
        [ ("author", "A. U. Thor"),
          ("auto_tangle", "t"),
          ("property", "header-args+ :noweb no-export"),
          ("title", "An Org Document")
        ]

    p "parses document metadata (title without leading whitespace)" "test/Lilac/ParseSpec/0002-doc-metadata-whitespace-before-value"
      $ L.defaultOrgDocWithMeta
        [ ("title", "Parse me without leading whitespace")
        ]

    pfm
      "detects missing document metadata value"
      "test/Lilac/ParseSpec/0002-doc-metadata-missing-value"
      (utoks ":\n" <> elabel "(doc meta) separator" <> elabel "(doc meta) keyword letter")
      7
    p
      "parses styled strings (slash inside word)"
      "test/Lilac/ParseSpec/0005-styled-strings-ignore-marker-inside-word"
      $ L.OrgDoc
        { cells =
            [ L.CParagraph
                [ L.PTokenStyledText
                    (L.StyledText {body = "foo and/or 1/2 bar \\ a\\b", style = L.styleMaskFrom [L.Italic]})
                ]
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    pf
      "detects unbalanced style markers (redundant activations)"
      "test/Lilac/ParseSpec/0005-styled-strings-redundant-activations"
      "redundant style activations: [Bold]"
      8

    pf
      "detects unbalanced style markers (overzealous deactivations)"
      "test/Lilac/ParseSpec/0005-styled-strings-overzealous-deactivations"
      "overzealous style deactivations: [Bold]"
      10

    pf
      "detects unbalanced style markers (leftover active styles)"
      "test/Lilac/ParseSpec/0005-styled-strings-leftover-active-styles"
      "leftover active styles: [Bold]"
      15

    pf
      "detects unbalanced style markers (redundant style marker)"
      "test/Lilac/ParseSpec/0005-styled-strings-redundant-style-marker"
      "stylesToStyleMask: redundant style marker: Bold"
      8

    p
      "parses styled strings (compression)"
      "test/Lilac/ParseSpec/0005-styled-strings-compression"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledText
                            (L.StyledText {body = "a b c d e ", style = L.styleMaskFrom []}),
                          L.PTokenStyledText
                            (L.StyledText {body = "f g h i j", style = L.styleMaskFrom [L.Bold]})
                        ],
                      meta = fromList []
                    }
                )
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses styled strings (no style)"
      "test/Lilac/ParseSpec/0005-styled-strings-no-style"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledText
                            ( L.StyledText
                                { body = "foo",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ],
                      meta = fromList []
                    }
                )
            ],
          code = [],
          codeBlocks = IM.fromList [],
          meta = M.fromList []
        }

    p
      "parses styled strings (bold style)"
      "test/Lilac/ParseSpec/0005-styled-strings-bold"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledText
                            ( L.StyledText
                                { body = "foo ",
                                  style = L.styleMaskFrom []
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "bar",
                                  style = L.styleMaskFrom [L.Bold]
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = " baz",
                                  style = L.styleMaskFrom []
                                }
                            )
                        ],
                      meta = fromList []
                    }
                )
            ],
          code = [],
          codeBlocks = IM.fromList [],
          meta = M.fromList []
        }

    p
      "parses styled strings (complex)"
      "test/Lilac/ParseSpec/0005-styled-strings-complex"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledText
                            (L.StyledText {body = "foo ", style = L.styleMaskFrom [L.Bold]}),
                          L.PTokenStyledText
                            (L.StyledText {body = "bar ", style = L.styleMaskFrom [L.Bold, L.Italic]}),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "underlined bold and italic",
                                  style = L.styleMaskFrom [L.Bold, L.Italic, L.Underline]
                                }
                            ),
                          L.PTokenStyledText
                            (L.StyledText {body = " baz", style = L.styleMaskFrom [L.Bold]}),
                          L.PTokenStyledText
                            (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                          L.PTokenStyledText
                            (L.StyledText {body = "verbatim  text", style = L.styleMaskFrom [L.Verbatim]}),
                          L.PTokenStyledText
                            (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                          L.PTokenStyledText
                            (L.StyledText {body = "x  y  z", style = L.styleMaskFrom [L.Code]}),
                          L.PTokenStyledText
                            (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "strikethrough ",
                                  style = L.styleMaskFrom [L.Strikethrough]
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "bold",
                                  style = L.styleMaskFrom [L.Bold, L.Strikethrough]
                                }
                            )
                        ],
                      meta = fromList []
                    }
                )
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses styled strings (complex punctuation)"
      "test/Lilac/ParseSpec/0005-styled-strings-complex-punctuation"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledText
                            (L.StyledText {body = "(foo ", style = L.styleMaskFrom []}),
                          L.PTokenStyledText
                            (L.StyledText {body = "bar", style = L.styleMaskFrom [L.Verbatim]}),
                          L.PTokenStyledText
                            (L.StyledText {body = "). What's up? and ", style = L.styleMaskFrom []}),
                          L.PTokenStyledText
                            (L.StyledText {body = "that!", style = L.styleMaskFrom [L.Bold]}),
                          L.PTokenStyledText
                            (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                          L.PTokenStyledText
                            (L.StyledText {body = "(italic)", style = L.styleMaskFrom [L.Italic]}),
                          L.PTokenStyledText
                            (L.StyledText {body = ". It's #1!", style = L.styleMaskFrom []})
                        ],
                      meta = fromList []
                    }
                )
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses paragraphs"
      "test/Lilac/ParseSpec/0005-prose-paragraphs"
      $ L.OrgDoc
        { cells =
            [ L.CParagraph
                [ L.PTokenStyledText
                    (L.StyledText {body = "a b c d e ", style = L.styleMaskFrom []}),
                  L.PTokenStyledText
                    (L.StyledText {body = "f g h i j", style = L.styleMaskFrom [L.Bold]})
                ],
              L.CParagraph
                [ L.PTokenStyledText
                    (L.StyledText {body = "x y z foo bar", style = L.styleMaskFrom []})
                ],
              L.CParagraph
                [ L.PTokenStyledText
                    (L.StyledText {body = "multiple lines with underscore", style = L.styleMaskFrom [L.Underline]})
                ]
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    ps
      "parses standalone math fragments"
      "test/Lilac/ParseSpec/0010-standalone-math-fragments"
      L.OrgDoc
        { cells =
            [ L.CMathFragment
                L.MathFragment
                  { meta =
                      fromList
                        [ ("__equation_number", "1")
                        ],
                    body = "1 + 1 = 2\n"
                  },
              L.CMathFragment
                L.MathFragment
                  { meta =
                      fromList
                        [ ("__equation_number", "2")
                        ],
                    body = "1 + 1 = 2\n"
                  },
              L.CMathFragment
                L.MathFragment
                  { meta = fromList [],
                    body = "2 + 2 = 4\n"
                  },
              L.CMathFragment
                L.MathFragment
                  { meta = fromList [],
                    body = "2 + 2 = 4\n"
                  }
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }
      L.OrgParserState
        { styleMask = L.styleMaskFrom [],
          listLevels = [],
          listMarkers = [],
          continuationIndent = 0,
          equationNumber = 2
        }
    p
      "parses links (target only)"
      "test/Lilac/ParseSpec/0006-links"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenMathFragment
                            ( L.MathFragment
                                { meta = fromList [],
                                  body = "foo"
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = " ",
                                  style = L.styleMaskFrom []
                                }
                            ),
                          L.PTokenLink
                            ( L.Link
                                { typ = L.LinkExternal,
                                  target = "link-target-only",
                                  name =
                                    [ L.StyledText
                                        { body = "link-target-only",
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            )
                        ],
                      meta = fromList []
                    }
                ),
              L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenMathFragment
                            ( L.MathFragment
                                { meta = fromList [],
                                  body = "bar"
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = " ",
                                  style = L.styleMaskFrom []
                                }
                            ),
                          L.PTokenLink
                            ( L.Link
                                { typ = L.LinkExternal,
                                  target = "link-target",
                                  name =
                                    [ L.StyledText
                                        { body = "link-name",
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            )
                        ],
                      meta = fromList []
                    }
                )
            ],
          code = [],
          codeBlocks = IM.fromList [],
          meta = M.fromList []
        }
    p
      "parses drawers (empty)"
      "test/Lilac/ParseSpec/0004-drawers-empty"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledText
                            (L.StyledText {body = "foo", style = L.styleMaskFrom []})
                        ],
                      meta = fromList [("FOO", "")]
                    }
                )
            ],
          code = [],
          codeBlocks = IM.fromList [],
          meta = M.fromList []
        }

    p
      "parses drawers (not empty)"
      "test/Lilac/ParseSpec/0004-drawers-not-empty"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledText
                            (L.StyledText {body = "foo", style = L.styleMaskFrom []})
                        ],
                      meta = fromList [("FOO", "- a\n- b\n")]
                    }
                )
            ],
          code = [],
          codeBlocks = IM.fromList [],
          meta = M.fromList []
        }
    pf
      "detects mixed list types (unordered)"
      "test/Lilac/ParseSpec/0009-lists-cannot-be-mixed-on-their-own"
      "expected UNORDERED list item at this level"
      21

    pf
      "detects mixed list types (ordered)"
      "test/Lilac/ParseSpec/0009-lists-cannot-be-mixed-on-their-own-ordered"
      "expected ORDERED list item at this level"
      23

    pf
      "detects slightly wrong indentation"
      "test/Lilac/ParseSpec/0009-lists-require-precise-indentation"
      "list item: invalid indentation"
      15

    p
      "requires blank line separator for list items"
      "test/Lilac/ParseSpec/0009-list-items-need-blank-line-separator"
      $ L.OrgDoc
        { cells =
            [ L.CListItem
                [ L.LOrderedStart,
                  L.LItemStart
                ],
              plainParagraph "a 1) b",
              L.CListItem
                [ L.LItemEnd,
                  L.LOrderedEnd
                ]
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    pf
      "detects non-incrementing ordered list item"
      "test/Lilac/ParseSpec/0009-ordered-lists-require-increment"
      "expected incremented list head 2 but got 1"
      13

    pf
      "detects invalid initial ordered list number of 2"
      "test/Lilac/ParseSpec/0009-ordered-lists-start-from-0-or-1"
      "starting list head number must be 0 or 1; got 20"
      25

    p
      "parses blocks with noweb references"
      "test/Lilac/ParseSpec/0007-blocks-noweb"
      $ L.OrgDoc
        { cells =
            [ L.CBlock
                ( L.Block
                    { meta =
                        fromList
                          [ ("__block_type", "src"),
                            ("__src_language", "sh")
                          ],
                      body =
                        [ L.BTokenLink
                            ( L.Link
                                { typ = L.LinkNoweb,
                                  target = "foo",
                                  name =
                                    [ L.StyledText
                                        { body = "foo",
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            ),
                          L.BTokenString "\n\n<<quux\n>>\necho ",
                          L.BTokenLink
                            ( L.Link
                                { typ = L.LinkNoweb,
                                  target = "bar",
                                  name =
                                    [ L.StyledText
                                        { body = "bar",
                                          style = L.styleMaskFrom []
                                        }
                                    ]
                                }
                            ),
                          L.BTokenString " x y z"
                        ]
                    }
                )
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses blocks with noweb references, but treats them if ':noweb' is 'off'"
      "test/Lilac/ParseSpec/0007-blocks-noweb-off"
      $ L.OrgDoc
        { cells =
            [ L.CBlock
                ( L.Block
                    { meta =
                        fromList
                          [ ("__block_type", "src"),
                            ("__src_language", "sh"),
                            ("noweb", "off")
                          ],
                      body =
                        [ L.BTokenString "<<foo>>\n\n<<quux\n>>\necho <<bar>> x y z"
                        ]
                    }
                )
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses blocks"
      "test/Lilac/ParseSpec/0007-blocks-empty"
      $ L.OrgDoc
        { cells =
            [ L.CBlock
                ( L.Block
                    { meta =
                        fromList
                          [ ("__block_type", "src"),
                            ("__src_language", "haskell")
                          ],
                      body = []
                    }
                ),
              L.CBlock
                ( L.Block
                    { meta =
                        fromList
                          [ ("__block_type", "example")
                          ],
                      body = []
                    }
                ),
              L.CBlock
                ( L.Block
                    { meta =
                        fromList
                          [ ("__block_type", "quote")
                          ],
                      body = []
                    }
                )
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    pf
      "detects missing language specifier in src blocks"
      "test/Lilac/ParseSpec/0007-block-src-without-lang"
      "block type src requires a language specifier"
      11

    p
      "parses block metadata"
      "test/Lilac/ParseSpec/0007-blocks-metadata"
      $ L.OrgDoc
        { cells =
            [ L.CBlock
                ( L.Block
                    { meta =
                        fromList
                          [ ("__block_type", "src"),
                            ("__src_language", "haskell"),
                            ("name", "some-code"),
                            ("noweb-ref", "Foo bar")
                          ],
                      body = []
                    }
                ),
              L.CBlock
                ( L.Block
                    { meta =
                        fromList
                          [ ("__block_type", "example"),
                            ("name", "some-example")
                          ],
                      body = []
                    }
                )
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }
    pf
      "detects indentation (comment line)"
      "test/Lilac/ParseSpec/0008-indentation-comment"
      "invalid indentation"
      1

    pf
      "detects indentation (heading)"
      "test/Lilac/ParseSpec/0008-indentation-headings"
      "invalid indentation"
      1

    pf
      "detects indentation (paragraph)"
      "test/Lilac/ParseSpec/0008-indentation-paragraph"
      "invalid indentation"
      1
    p
      "does not recognize list without enough indentation"
      "test/Lilac/ParseSpec/0009-list-indentation-no-indent"
      $ L.OrgDoc
        { cells =
            [ plainParagraph "1)",
              plainParagraph "2)"
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses 2-space indented list"
      "test/Lilac/ParseSpec/0009-list-indentation-proper-indent"
      $ L.OrgDoc
        { cells =
            [ L.CListItem
                [ L.LOrderedStart,
                  L.LItemStart
                ],
              L.CListItem
                [ L.LItemEnd,
                  L.LItemStart
                ],
              L.CListItem
                [ L.LItemEnd,
                  L.LOrderedEnd
                ]
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses nested list"
      "test/Lilac/ParseSpec/0009-list-indentation-nested"
      $ L.OrgDoc
        { cells =
            [ L.CListItem
                [ L.LOrderedStart, ---- Start outer ordered list.
                  L.LItemStart -------- Start outer "1)".
                ],
              L.CListItem
                [ L.LUnorderedStart, -- Start inner bullets under "1)".
                  L.LItemStart -------- Inner bullet start.
                ],
              L.CListItem
                [ L.LItemEnd, --------- Inner bullet end.
                  L.LItemStart -------- Inner bullet start.
                ],
              L.CListItem
                [ L.LItemEnd, --------- Inner bullet end.
                  L.LUnorderedEnd, ---- End of inner bullets under "1)".
                  L.LItemEnd, --------- End of outer "1)".
                  L.LItemStart -------- Start of outer "2)".
                ],
              L.CListItem
                [ L.LOrderedStart, ---- Start of nested ordered list.
                  L.LItemStart -------- Start of nested "1)".
                ],
              L.CListItem
                [ L.LUnorderedStart, -- Start deeply nested bullets.
                  L.LItemStart -------- Deeply nested bullet start.
                ],
              L.CListItem
                [ L.LItemEnd, --------- Deeply nested bullet end.
                  L.LUnorderedEnd, ---- End of deeply nested bullets.
                  L.LItemEnd, --------- End of nested "1)".
                  L.LOrderedEnd, ------ End of nested ordered list.
                  L.LItemEnd, --------- End of outer "2)".
                  L.LItemStart -------- Start of outer "3)".
                ],
              L.CListItem
                [ L.LItemEnd, --------- End of outer "3)".
                  L.LOrderedEnd ------- End outer ordered list.
                ]
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses list with content (simple)"
      "test/Lilac/ParseSpec/0009-list-content-simple"
      $ L.OrgDoc
        { cells =
            [ L.CListItem
                [ L.LOrderedStart,
                  L.LItemStart
                ],
              plainParagraph "First paragraph.",
              plainParagraph "Second paragraph.",
              L.CListItem
                [ L.LItemEnd,
                  L.LItemStart
                ],
              plainParagraph "Another paragraph.",
              L.CListItem
                [ L.LItemEnd,
                  L.LOrderedEnd
                ]
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses nested list (with text)"
      "test/Lilac/ParseSpec/0009-list-indentation-nested-with-text"
      $ L.OrgDoc
        { cells =
            [ plainParagraph "Foo.",
              L.CListItem
                [ L.LOrderedStart, ---- Start outer ordered list.
                  L.LItemStart -------- Start outer "1)".
                ],
              plainParagraph "one",
              L.CListItem
                [ L.LUnorderedStart, -- Start inner bullets under "1)".
                  L.LItemStart -------- Inner bullet start.
                ],
              plainParagraph "a",
              L.CListItem
                [ L.LItemEnd, --------- Inner bullet end.
                  L.LItemStart -------- Inner bullet start.
                ],
              plainParagraph "b",
              L.CListItem
                [ L.LItemEnd, --------- Inner bullet end.
                  L.LUnorderedEnd, ---- End of inner bullets under "1)".
                  L.LItemEnd, --------- End of outer "1)".
                  L.LItemStart -------- Start of outer "2)".
                ],
              plainParagraph "two",
              L.CListItem
                [ L.LOrderedStart, ---- Start of nested ordered list.
                  L.LItemStart -------- Start of nested "1)".
                ],
              plainParagraph "two-point-one",
              plainParagraph "hello",
              L.CListItem
                [ L.LUnorderedStart, -- Start deeply nested bullets.
                  L.LItemStart -------- Deeply nested bullet start.
                ],
              plainParagraph "X",
              L.CListItem
                [ L.LItemEnd, --------- Deeply nested bullet end.
                  L.LUnorderedEnd, ---- End of deeply nested bullets.
                  L.LItemEnd, --------- End of nested "1)".
                  L.LOrderedEnd, ------ End of nested ordered list.
                  L.LItemEnd, --------- End of outer "2)".
                  L.LItemStart -------- Start of outer "3)".
                ],
              plainParagraph "three",
              L.CListItem
                [ L.LItemEnd, --------- End of outer "3)".
                  L.LOrderedEnd ------- End outer ordered list.
                ],
              plainParagraph "Bar."
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses paragraphs which can close deeper list levels"
      "test/Lilac/ParseSpec/0009-paragraph-can-close-deeper-levels"
      $ L.OrgDoc
        { cells =
            [ L.CListItem
                [ L.LOrderedStart,
                  L.LItemStart
                ],
              plainParagraph "One",
              plainParagraph "Two x",
              L.CListItem
                [ L.LUnorderedStart,
                  L.LItemStart
                ],
              plainParagraph "Bar",
              L.CListItem
                [ L.LUnorderedStart,
                  L.LItemStart
                ],
              plainParagraph "Baz",
              L.CListItem
                [ L.LItemEnd,
                  L.LUnorderedEnd,
                  L.LItemEnd,
                  L.LUnorderedEnd
                ],
              plainParagraph "Three a b c",
              L.CListItem
                [ L.LItemEnd,
                  L.LOrderedEnd
                ],
              plainParagraph "Four"
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }

    p
      "parses paragraphs which can separate mixed lists"
      "test/Lilac/ParseSpec/0009-paragraph-can-separate-mixed-lists"
      $ L.OrgDoc
        { cells =
            [ L.CListItem
                [ L.LOrderedStart,
                  L.LItemStart
                ],
              plainParagraph "One",
              L.CListItem
                [ L.LUnorderedStart,
                  L.LItemStart
                ],
              plainParagraph "Bar",
              L.CListItem
                [ L.LUnorderedStart,
                  L.LItemStart
                ],
              plainParagraph "Baz",
              L.CListItem
                [ L.LItemEnd,
                  L.LUnorderedEnd,
                  L.LItemEnd,
                  L.LUnorderedEnd
                ],
              plainParagraph "Two",
              L.CListItem
                [ L.LOrderedStart,
                  L.LItemStart
                ],
              plainParagraph "a",
              L.CListItem
                [ L.LItemEnd,
                  L.LItemStart
                ],
              plainParagraph "b",
              L.CListItem
                [ L.LItemEnd,
                  L.LOrderedEnd
                ],
              plainParagraph "Three",
              L.CListItem
                [ L.LItemEnd,
                  L.LOrderedEnd
                ],
              plainParagraph "Four"
            ],
          code = [],
          codeBlocks = fromList [],
          meta = fromList []
        }
    pfm
      "detects over-indentation for paragraph continuation line (list body)"
      "test/Lilac/ParseSpec/0005-paragraph-indentation-mismatch-over-list-body"
      (utoks " ")
      14

    pfm
      "detects over-indentation for paragraph continuation line"
      "test/Lilac/ParseSpec/0005-paragraph-indentation-mismatch-over"
      (utoks " ")
      4

    pfm
      "detects under-indentation for paragraph continuation line (list body)"
      "test/Lilac/ParseSpec/0005-paragraph-indentation-mismatch-under-list-body"
      (utoks "b" <> etoks " ")
      12

    pf
      "detects invalid initial indentation for paragraph"
      "test/Lilac/ParseSpec/0005-paragraph-indentation-mismatch-initial-indent-invalid"
      "indentation does not fit into existing levels"
      21

    pf
      "detects invalid initial indentation for paragraph (between list levels)"
      "test/Lilac/ParseSpec/0005-paragraph-indentation-mismatch-initial-indent-invalid-between-indent-levels"
      "indentation does not fit into existing levels"
      36

plainParagraph :: T.Text -> L.Cell
plainParagraph body =
  L.CParagraph
    [ L.PTokenStyledText
        ( L.StyledText
            { body = body,
              style = L.styleMaskFrom []
            }
        )
    ]
