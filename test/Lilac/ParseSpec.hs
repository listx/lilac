module Lilac.ParseSpec (spec) where

import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Lilac.Parse qualified as L
import Relude
  ( ByteString,
    FilePath,
    Int,
    MonadIO,
    String,
    decodeUtf8,
    fromList,
    readFileBS,
    runStateT,
    ($),
    (.),
    (<>),
  )
import Test.Hspec (Spec, before, it)
import Test.Hspec.Megaparsec
  ( ET,
    elabel,
    err,
    errFancy,
    etoks,
    fancy,
    shouldFailWith,
    shouldParse,
    shouldSucceedOn,
    utoks,
  )
import Text.Megaparsec qualified as MP

readFrom :: (MonadIO m) => FilePath -> FilePath -> m ByteString
readFrom prefix path = readFileBS $ prefix <> path

p :: FilePath -> L.OrgDoc -> Spec
p path expected = do
  before (readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldParse` (expected, L.defaultOrgParserState)

ps :: FilePath -> L.OrgDoc -> L.OrgParserState -> Spec
ps path expected state = do
  before (readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldParse` (expected, state)

pf :: FilePath -> String -> Int -> Spec
pf path errMsg pos = do
  before (readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldFailWith` (errFancy pos . fancy $ MP.ErrorFail errMsg)

pfm :: FilePath -> ET T.Text -> Int -> Spec
pfm path errorTrivial pos = do
  before (readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        path
        (decodeUtf8 input)
        `shouldFailWith` (err pos errorTrivial)

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

spec :: Spec
spec = do
  it "empty input" $ do
    MP.runParser
      (runStateT L.orgDocParser L.defaultOrgParserState)
      "empty input"
      ""
      `shouldParse` (L.defaultOrgDoc [], L.defaultOrgParserState)
  before (readFrom "" "lilac.org") $ do
    it "self-parse" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        "lilac.org"
        `shouldSucceedOn` (decodeUtf8 input)
  p
    "0001-blankline"
    (L.defaultOrgDoc [])
  pfm
    "0002-doc-metadata-missing-value"
    (utoks ":\n" <> elabel "(doc meta) separator" <> elabel "(doc meta) keyword letter")
    7
  p
    "0002-doc-metadata-whitespace-before-value"
    $ L.defaultOrgDoc
      [ ("title", "Parse me without leading whitespace")
      ]
  p
    "0002-doc-metadata"
    $ L.defaultOrgDoc
      [ ("author", "A. U. Thor"),
        ("auto_tangle", "t"),
        ("property", "header-args+ :noweb no-export"),
        ("title", "An Org Document")
      ]
  p
    "0003-doc-metadata-with-comments"
    $ L.defaultOrgDoc
      [ ("author", "A. U. Thor"),
        ("auto_tangle", "t"),
        ("property", "header-args+ :noweb no-export"),
        ("title", "An Org Document")
      ]
  p
    "0004-drawers-empty"
    L.OrgDoc
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
    "0004-drawers-not-empty"
    L.OrgDoc
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
    L.OrgDoc
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
  p
    "0005-styled-strings-bold"
    L.OrgDoc
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
    "0005-styled-strings-complex-punctuation"
    L.OrgDoc
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
    "0005-styled-strings-complex"
    L.OrgDoc
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
                              { body = "bold",
                                style = L.styleMaskFrom [L.Bold]
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
    "0005-styled-strings-compression"
    L.OrgDoc
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
    "0005-styled-strings-ignore-marker-inside-word"
    L.OrgDoc
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
    "0005-styled-strings-leftover-active-styles"
    "leftover active styles: [Bold]"
    15
  p
    "0005-styled-strings-messy-boundaries"
    L.OrgDoc
      { cells =
          [ L.CHeading
              ( L.Heading
                  { level = 1,
                    body =
                      [ L.PTokenStyledText
                          (L.StyledText {body = "foo ", style = L.styleMaskFrom [L.Bold]}),
                        L.PTokenStyledText
                          (L.StyledText {body = "bar baz a", style = L.styleMaskFrom []}),
                        L.PTokenStyledText
                          (L.StyledText {body = "b", style = L.styleMaskFrom [L.Bold]}),
                        L.PTokenStyledText
                          (L.StyledText {body = "c (", style = L.styleMaskFrom []}),
                        L.PTokenStyledText
                          (L.StyledText {body = "ok", style = L.styleMaskFrom [L.Bold]}),
                        L.PTokenStyledText
                          (L.StyledText {body = ") yes", style = L.styleMaskFrom []})
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
    "0005-styled-strings-no-style"
    L.OrgDoc
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
    "0005-styled-strings-verbatim-code-ignore-immediate-bold"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  (L.StyledText {body = "*a", style = L.styleMaskFrom [L.Verbatim]}),
                L.PTokenStyledText
                  (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                L.PTokenStyledText
                  (L.StyledText {body = "b*", style = L.styleMaskFrom [L.Verbatim]}),
                L.PTokenStyledText
                  (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                L.PTokenStyledText
                  (L.StyledText {body = "*c", style = L.styleMaskFrom [L.Code]}),
                L.PTokenStyledText
                  (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                L.PTokenStyledText
                  (L.StyledText {body = "d*", style = L.styleMaskFrom [L.Code]})
              ]
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
  p
    "0005-styled-strings-verbatim-precedence"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  (L.StyledText {body = "[[foo", style = L.styleMaskFrom [L.Verbatim]})
              ],
            L.CParagraph
              [ L.PTokenLink
                  ( L.Link
                      { typ = L.LinkExternal,
                        target = "hello",
                        name =
                          [ L.StyledText
                              { body = "hello",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  ),
                L.PTokenStyledText
                  (L.StyledText {body = " there", style = L.styleMaskFrom []})
              ],
            L.CParagraph
              [ L.PTokenStyledText
                  (L.StyledText {body = "\\(1 + 1 ", style = L.styleMaskFrom [L.Verbatim]}),
                L.PTokenStyledText
                  (L.StyledText {body = " ", style = L.styleMaskFrom []}),
                L.PTokenStyledText
                  (L.StyledText {body = "\\(2 * 2\\)", style = L.styleMaskFrom [L.Code]})
              ],
            L.CParagraph
              [ L.PTokenStyledText
                  (L.StyledText {body = "[[bar]]", style = L.styleMaskFrom [L.Code]})
              ]
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
  p
    "0005-styled-strings-verbatim"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  (L.StyledText {body = "(", style = L.styleMaskFrom []}),
                L.PTokenStyledText
                  (L.StyledText {body = "import ./.", style = L.styleMaskFrom [L.Verbatim]}),
                L.PTokenStyledText
                  (L.StyledText {body = ") hello", style = L.styleMaskFrom []})
              ]
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
  p
    "0006-links-inside-parentheses"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  ( L.StyledText
                      { body = "(",
                        style = L.styleMaskFrom []
                      }
                  ),
                L.PTokenLink
                  ( L.Link
                      { typ = L.LinkExternal,
                        target = "file:foo/bar",
                        name =
                          [ L.StyledText
                              { body = "file:foo/bar",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  ),
                L.PTokenStyledText
                  ( L.StyledText
                      { body = ")",
                        style = L.styleMaskFrom []
                      }
                  )
              ]
          ],
        code = [],
        codeBlocks = IM.fromList [],
        meta = M.fromList []
      }
  p
    "0006-links-nonlink"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  ( L.StyledText
                      { body = "[hi]",
                        style = L.styleMaskFrom []
                      }
                  )
              ]
          ],
        code = [],
        codeBlocks = IM.fromList [],
        meta = M.fromList []
      }
  p
    "0006-links-raw"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  ( L.StyledText
                      { body = "See ",
                        style = L.styleMaskFrom []
                      }
                  ),
                L.PTokenLink
                  ( L.Link
                      { typ = L.LinkExternal,
                        target = "https://foo.org",
                        name =
                          [ L.StyledText
                              { body = "https://foo.org",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  ),
                L.PTokenStyledText
                  ( L.StyledText
                      { body = ", or ",
                        style = L.styleMaskFrom []
                      }
                  ),
                L.PTokenLink
                  ( L.Link
                      { typ = L.LinkExternal,
                        target = "https://bar.net",
                        name =
                          [ L.StyledText
                              { body = "https://bar.net",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  ),
                L.PTokenStyledText
                  ( L.StyledText
                      { body = ". Try ",
                        style = L.styleMaskFrom []
                      }
                  ),
                L.PTokenLink
                  ( L.Link
                      { typ = L.LinkExternal,
                        target = "http://localhost:3000",
                        name =
                          [ L.StyledText
                              { body = "http://localhost:3000",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  ),
                L.PTokenStyledText
                  ( L.StyledText
                      { body = "? Go to ",
                        style = L.styleMaskFrom []
                      }
                  ),
                L.PTokenLink
                  ( L.Link
                      { typ = L.LinkExternal,
                        target = "ftp://10.0.0.1",
                        name =
                          [ L.StyledText
                              { body = "ftp://10.0.0.1",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  ),
                L.PTokenStyledText
                  ( L.StyledText
                      { body = "!",
                        style = L.styleMaskFrom []
                      }
                  )
              ]
          ],
        code = [],
        codeBlocks = IM.fromList [],
        meta = M.fromList []
      }
  p
    "0006-links"
    L.OrgDoc
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
              ),
            L.CParagraph
              [ L.PTokenLink
                  ( L.Link
                      { typ = L.LinkExternal,
                        target = "a",
                        name =
                          [ L.StyledText
                              { body = "a",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  ),
                L.PTokenStyledText
                  ( L.StyledText
                      { body = " b",
                        style = L.styleMaskFrom []
                      }
                  )
              ]
          ],
        code = [],
        codeBlocks = IM.fromList [],
        meta = M.fromList []
      }
  pf
    "0007-block-src-without-lang"
    "block type src requires a language specifier"
    11
  p
    "0007-blocks-empty"
    L.OrgDoc
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
  p
    "0007-blocks-ending"
    L.OrgDoc
      { cells =
          [ L.CBlock
              ( L.Block
                  { meta =
                      fromList
                        [ ("__block_type", "src"),
                          ("__src_language", "sh"),
                          ("name", "empty")
                        ],
                    body = []
                  }
              ),
            L.CBlock
              ( L.Block
                  { meta =
                      fromList
                        [ ("__block_type", "src"),
                          ("__src_language", "sh"),
                          ("name", "end not in beginning of line")
                        ],
                    body = [L.BTokenString " #+end_src"]
                  }
              ),
            L.CBlock
              ( L.Block
                  { meta =
                      fromList
                        [ ("__block_type", "src"),
                          ("__src_language", "sh"),
                          ("name", "end has no preceding newline")
                        ],
                    body = [L.BTokenString "foo"]
                  }
              ),
            L.CBlock
              ( L.Block
                  { meta =
                      fromList
                        [ ("__block_type", "src"),
                          ("__src_language", "sh"),
                          ("name", "end has preceding newline")
                        ],
                    body = [L.BTokenString "foo\n"]
                  }
              ),
            L.CBlock
              ( L.Block
                  { meta =
                      fromList
                        [ ("__block_type", "src"),
                          ("__src_language", "sh"),
                          ("name", "end has preceding newline (multiple)")
                        ],
                    body = [L.BTokenString "foo\n\n"]
                  }
              )
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
  p
    "0007-blocks-metadata"
    L.OrgDoc
      { cells =
          [ L.CBlock
              ( L.Block
                  { meta =
                      fromList
                        [ ("__block_type", "src"),
                          ("__src_language", "haskell"),
                          ("name", "some-code")
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
  p
    "0007-blocks-noweb-off"
    L.OrgDoc
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
    "0007-blocks-noweb"
    L.OrgDoc
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
    L.OrgDoc
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
    "0009-list-indentation-nested-with-text"
    L.OrgDoc
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
    "0009-list-indentation-nested"
    L.OrgDoc
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
    "0009-list-indentation-no-indent"
    L.OrgDoc
      { cells =
          [ plainParagraph "1)",
            plainParagraph "2)"
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
  p
    "0009-list-indentation-proper-indent"
    L.OrgDoc
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
    "0009-list-items-need-blank-line-separator"
    L.OrgDoc
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
  p
    "0009-paragraph-can-close-deeper-levels"
    L.OrgDoc
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
    "0009-paragraph-can-separate-mixed-lists"
    L.OrgDoc
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
  ps
    "0010-standalone-math-fragments"
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
                { meta =
                    fromList
                      [ ("name", "eq:x")
                      ],
                  body = "2 + 2 = 4\n"
                },
            L.CMathFragment
              L.MathFragment
                { meta =
                    fromList
                      [ ("__equation_number", "3"),
                        ("name", "eq:y")
                      ],
                  body = "1 + 1 = 2\n"
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
        equationNumber = 3
      }
  p
    "0011-figures-and-blocks"
    L.OrgDoc
      { cells =
          [ L.CBlock
              L.Block
                { meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "haskell"),
                        ("name", "f:f"),
                        ("xxx", "bar")
                      ],
                  body = [L.BTokenString "f = 1 + 1"]
                },
            L.CFigure
              L.Figure
                { meta =
                    fromList
                      [ ("attr_html", ":width 100%"),
                        ("name", "fig:hello")
                      ],
                  link =
                    L.Link
                      { typ = L.LinkExternal,
                        target = "./image/hello.png",
                        name =
                          [ L.StyledText
                              { body = "./image/hello.png",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                },
            L.CBlock
              L.Block
                { meta =
                    fromList
                      [ ("__block_type", "src"),
                        ("__src_language", "haskell"),
                        ("name", "f:g"),
                        ("yyy", "bar")
                      ],
                  body = [L.BTokenString "g x = x + 1"]
                },
            L.CFigure
              L.Figure
                { meta = fromList [],
                  link =
                    L.Link
                      { typ = L.LinkExternal,
                        target = "./image/world.png",
                        name =
                          [ L.StyledText
                              { body = "./image/world.png",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                }
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
  p
    "0012-tables-and-prose"
    L.OrgDoc
      { cells =
          [ L.CTable
              L.Table
                { meta =
                    fromList
                      [ ("name", "tbl:famous-equations")
                      ],
                  grid =
                    fromList
                      [ ( (1, 1),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Name",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (1, 2),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Equation",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (1, 3),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Link",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (2, 1),
                          []
                        ),
                        ( (3, 1),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Pythagoras | Foo",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (3, 2),
                          [ L.PTokenMathFragment
                              ( L.MathFragment
                                  { meta = fromList [],
                                    body = "a^2 + b^2 = c^2"
                                  }
                              )
                          ]
                        ),
                        ( (3, 3),
                          [ L.PTokenLink
                              ( L.Link
                                  { typ = L.LinkExternal,
                                    target = "https://en.wikipedia.org/wiki/Pythagorean_theorem",
                                    name =
                                      [ L.StyledText
                                          { body = "Wiki",
                                            style = L.styleMaskFrom []
                                          }
                                      ]
                                  }
                              )
                          ]
                        ),
                        ( (4, 1),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Albert ",
                                    style = L.styleMaskFrom []
                                  }
                              ),
                            L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Einstein",
                                    style = L.styleMaskFrom [L.Bold]
                                  }
                              )
                          ]
                        ),
                        ( (4, 2),
                          [ L.PTokenMathFragment
                              ( L.MathFragment
                                  { meta = fromList [],
                                    body = "E = mc^2"
                                  }
                              )
                          ]
                        ),
                        ( (4, 3),
                          [ L.PTokenLink
                              ( L.Link
                                  { typ = L.LinkExternal,
                                    target = "https://en.wikipedia.org/wiki/Mass%E2%80%93energy_equivalence",
                                    name =
                                      [ L.StyledText
                                          { body = "Wiki",
                                            style = L.styleMaskFrom []
                                          }
                                      ]
                                  }
                              )
                          ]
                        )
                      ]
                }
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
  p
    "0012-tables-inside-lists"
    L.OrgDoc
      { cells =
          [ L.CListItem
              [ L.LOrderedStart,
                L.LItemStart
              ],
            L.CParagraph
              [ L.PTokenStyledText
                  ( L.StyledText
                      { body = "foo",
                        style = L.styleMaskFrom []
                      }
                  )
              ],
            L.CTable
              L.Table
                { meta =
                    fromList
                      [ ("name", "tbl:famous-equations"),
                        ("bar", "baz")
                      ],
                  grid =
                    fromList
                      [ ( (1, 1),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Name",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (1, 2),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Equation",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (1, 3),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Link",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (2, 1),
                          []
                        ),
                        ( (3, 1),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Pythagoras | Foo",
                                    style = L.styleMaskFrom []
                                  }
                              )
                          ]
                        ),
                        ( (3, 2),
                          [ L.PTokenMathFragment
                              ( L.MathFragment
                                  { meta = fromList [],
                                    body = "a^2 + b^2 = c^2"
                                  }
                              )
                          ]
                        ),
                        ( (3, 3),
                          [ L.PTokenLink
                              ( L.Link
                                  { typ = L.LinkExternal,
                                    target = "https://en.wikipedia.org/wiki/Pythagorean_theorem",
                                    name =
                                      [ L.StyledText
                                          { body = "Wiki",
                                            style = L.styleMaskFrom []
                                          }
                                      ]
                                  }
                              )
                          ]
                        ),
                        ( (4, 1),
                          [ L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Albert ",
                                    style = L.styleMaskFrom []
                                  }
                              ),
                            L.PTokenStyledText
                              ( L.StyledText
                                  { body = "Einstein",
                                    style = L.styleMaskFrom [L.Bold]
                                  }
                              )
                          ]
                        ),
                        ( (4, 2),
                          [ L.PTokenMathFragment
                              ( L.MathFragment
                                  { meta = fromList [],
                                    body = "E = mc^2"
                                  }
                              )
                          ]
                        ),
                        ( (4, 3),
                          [ L.PTokenLink
                              ( L.Link
                                  { typ = L.LinkExternal,
                                    target = "https://en.wikipedia.org/wiki/Mass%E2%80%93energy_equivalence",
                                    name =
                                      [ L.StyledText
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
            L.CListItem
              [ L.LItemEnd,
                L.LOrderedEnd
              ]
          ],
        code = [],
        codeBlocks = fromList [],
        meta = fromList []
      }
