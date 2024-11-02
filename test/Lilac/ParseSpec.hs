module Lilac.ParseSpec (spec) where

import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Lilac.Parse qualified as L
import Relude
import Test.Hspec (Spec, before, describe, it)
import Test.Hspec.Megaparsec (parseSatisfies, shouldFailOn)
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

pf :: String -> FilePath -> Spec
pf behavior path = do
  before (readFileBS path) $ do
    it behavior $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        `shouldFailOn` (decodeUtf8 input)

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
    p
      "parses styled strings (no style)"
      "test/Lilac/ParseSpec/0005-styled-strings-no-style"
      $ L.OrgDoc
        { cells =
            [ L.CHeading
                ( L.Heading
                    { level = 1,
                      body =
                        [ L.PTokenStyledString
                            ( L.StyledString
                                { body = "foo",
                                  style = []
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
                        [ L.PTokenStyledString
                            ( L.StyledString
                                { body = "foo",
                                  style = []
                                }
                            ),
                          L.PTokenStyledString
                            ( L.StyledString
                                { body = "bar",
                                  style = [L.Bold]
                                }
                            ),
                          L.PTokenStyledString
                            ( L.StyledString
                                { body = "baz",
                                  style = []
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
    pf
      "detect unbalanced style markers (starting marker)"
      "test/Lilac/ParseSpec/0005-styled-strings-unbalanced-start"
    pf
      "detect unbalanced style markers (ending marker)"
      "test/Lilac/ParseSpec/0005-styled-strings-unbalanced-end"
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
                                    [ L.StyledString
                                        { body = "link-target-only",
                                          style = []
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
                                    [ L.StyledString
                                        { body = "link-name",
                                          style = []
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
