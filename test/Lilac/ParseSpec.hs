module Lilac.ParseSpec (spec) where

import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Lilac.Parse qualified as L
import Relude
import Test.Hspec (Spec, before, describe, it)
import Test.Hspec.Megaparsec
  ( errFancy,
    fancy,
    parseSatisfies,
    shouldFailWith,
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
        `parseSatisfies` ((== expected) . fst)

pf :: String -> FilePath -> String -> Int -> Spec
pf behavior path errMsg pos = do
  before (readFileBS path) $ do
    it behavior $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldFailWith` (errFancy pos . fancy $ MP.ErrorFail errMsg)

spec :: Spec
spec =
  describe "parse" $ do
    it "parses empty input as defaultOrgDoc" $ do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        "empty input"
        ""
        `parseSatisfies` ((== L.defaultOrgDoc) . fst)
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
                                { body = "foo",
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
                                { body = "baz",
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
                            (L.StyledText {body = "foo", style = L.styleMaskFrom [L.Bold]}),
                          L.PTokenStyledText (L.StyledText {body = "bar", style = L.styleMaskFrom [L.Bold, L.Italic]}),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "underlined",
                                  style = L.styleMaskFrom [L.Bold, L.Italic, L.Underline]
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "bold",
                                  style = L.styleMaskFrom [L.Bold, L.Italic, L.Underline]
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "and",
                                  style = L.styleMaskFrom [L.Bold, L.Italic, L.Underline]
                                }
                            ),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "italic",
                                  style = L.styleMaskFrom [L.Bold, L.Italic, L.Underline]
                                }
                            ),
                          L.PTokenStyledText (L.StyledText {body = "baz", style = L.styleMaskFrom [L.Bold]}),
                          L.PTokenStyledText (L.StyledText {body = "verbatim  text", style = L.styleMaskFrom [L.Verbatim]}),
                          L.PTokenStyledText (L.StyledText {body = "x  y  z", style = L.styleMaskFrom [L.Code]}),
                          L.PTokenStyledText
                            ( L.StyledText
                                { body = "strikethrough",
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
                        [ L.PTokenMathFragment
                            ( L.MathFragment
                                { meta = fromList [],
                                  body = "foo"
                                }
                            )
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
                        [ L.PTokenMathFragment
                            ( L.MathFragment
                                { meta = fromList [],
                                  body = "foo"
                                }
                            )
                        ],
                      meta = fromList [("FOO", "- a\n- b\n")]
                    }
                )
            ],
          code = [],
          codeBlocks = IM.fromList [],
          meta = M.fromList []
        }
