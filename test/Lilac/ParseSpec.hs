module Lilac.ParseSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID.Types (UUID, fromText, nil)
import Data.Void (Void)
import Lilac.Parse qualified as L
import Lilac.Util qualified as LU
import Relude
  ( Bool (True),
    FilePath,
    Identity,
    Int,
    Maybe (Just),
    String,
    Type,
    fromList,
    map,
    otherwise,
    runStateT,
    show,
    ($),
    (.),
    (<>),
  )
import System.OsPath qualified as P
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
import Text.Megaparsec.Char (char)

spec :: Spec
spec = parseSpec

type Parser :: Type -> Type
type Parser = MP.ParsecT Void T.Text Identity

p :: FilePath -> L.OrgDoc -> Spec
p path expected = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser st)
        path
        input
        `shouldParse` ( expected,
                        L.defaultOrgParserState
                          { L.sourcePath = relPath
                          }
                      )
  where
    st = L.defaultOrgParserState {L.sourcePath = relPath}
    relPath = "test/Lilac/ParseSpec/" <> path

ps :: FilePath -> L.OrgDoc -> L.OrgParserState -> Spec
ps path expected state = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser st)
        path
        input
        `shouldParse` (expected, state)
  where
    st = L.defaultOrgParserState {L.sourcePath = relPath}
    relPath = "test/Lilac/ParseSpec/" <> path

pf :: FilePath -> String -> Int -> Spec
pf path errMsg pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser st)
        path
        input
        `shouldFailWith` (errFancy pos . fancy $ MP.ErrorFail errMsg)
  where
    st = L.defaultOrgParserState {L.sourcePath = relPath}
    relPath = "test/Lilac/ParseSpec/" <> path

pfm :: FilePath -> ET T.Text -> Int -> Spec
pfm path errorTrivial pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser st)
        path
        input
        `shouldFailWith` (err pos errorTrivial)
  where
    st = L.defaultOrgParserState {L.sourcePath = relPath}
    relPath = "test/Lilac/ParseSpec/" <> path

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

plainHeading :: Int -> L.Cell
plainHeading level =
  L.CHeading
    ( L.Heading
        { level = level,
          body =
            [ L.PTokenStyledText
                ( L.StyledText
                    { body = T.pack $ show level,
                      style = L.styleMaskFrom []
                    }
                )
            ],
          meta = fromList []
        }
    )

pOrgIdsElisp :: FilePath -> [(T.Text, FilePath)] -> Spec
pOrgIdsElisp path expected = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgIdsElispParser st)
        path
        input
        `shouldParse` ( M.fromList $ map f expected,
                        L.defaultOrgIdsElispParserState
                      )
  where
    st = L.defaultOrgIdsElispParserState
    f :: (T.Text, FilePath) -> (UUID, P.OsPath)
    f (uuidText, filePath)
      | Just uuid <- fromText uuidText,
        Just osPath <- P.encodeUtf filePath =
          (uuid, osPath)
      | otherwise = (nil, P.pack [])

pfOrgIdsElisp :: FilePath -> String -> Int -> Spec
pfOrgIdsElisp path errMsg pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT L.orgIdsElispParser st)
        path
        input
        `shouldFailWith` (errFancy pos . fancy $ MP.ErrorFail errMsg)
  where
    st = L.defaultOrgIdsElispParserState

parseSpec :: Spec
parseSpec = do
  it "empty input" $ do
    MP.runParser
      (runStateT L.orgDocParser L.defaultOrgParserState)
      "empty input"
      ""
      `shouldParse` (L.defaultOrgDoc [], L.defaultOrgParserState)

  before (LU.readFrom "" "lilac.org") $ do
    it "self-parse" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState {L.sourcePath = "lilac.org"})
        "lilac.org"
        `shouldSucceedOn` input

  p
    "0001-blankline"
    (L.defaultOrgDoc [])

  p
    "0001-comments-only"
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
    "0002-doc-metadata-properties-drawer"
    $ L.defaultOrgDoc
      [ ("ID", "7af1aa3e-41ca-4810-a001-2fdf10768f2f")
      ]

  p
    "0002-doc-metadata-properties-drawer-combine"
    $ L.defaultOrgDoc
      [ ("ID", "7af1aa3e-41ca-4810-a001-2fdf10768f2f"),
        ("author", "A. U. Thor"),
        ("property", "header-args+ :noweb no-export"),
        ("title", "An Org Document")
      ]

  pf
    "0002-doc-metadata-properties-drawer-clash"
    "conflicting keys in the top-level PROPERTIES drawer and document metadata key/values: [\"foo\"]"
    134

  pf
    "0004-headings"
    "found 7 asterisks found for this heading, but lilac_maxHeadingLevel = 6"
    46

  ps
    "0004-headings-20"
    L.OrgDoc
      { cells =
          [ plainHeading 1,
            plainHeading 2,
            plainHeading 3,
            plainHeading 4,
            plainHeading 5,
            plainHeading 6,
            plainHeading 7,
            plainHeading 8,
            plainHeading 9,
            plainHeading 10,
            plainHeading 11,
            plainHeading 12
          ],
        meta = M.fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0004-headings-20",
        headingLevel = 12,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf {L.maxHeadingLevel = 20}
      }

  ps
    "0004-headings-unlimited"
    L.OrgDoc
      { cells =
          [ plainHeading 1,
            plainHeading 2,
            plainHeading 3,
            plainHeading 4,
            plainHeading 5,
            plainHeading 6,
            plainHeading 7,
            plainHeading 8,
            plainHeading 9,
            plainHeading 10,
            plainHeading 11,
            plainHeading 12
          ],
        meta = M.fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0004-headings-unlimited",
        headingLevel = 12,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf {L.maxHeadingLevel = 0}
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

  ps
    "0004-properties-drawer-empty"
    L.OrgDoc
      { cells =
          [ L.CHeading
              ( L.Heading
                  { level = 1,
                    body =
                      [ L.PTokenStyledText
                          (L.StyledText {body = "foo", style = L.styleMaskFrom []})
                      ],
                    meta = fromList []
                  }
              )
          ],
        meta = M.fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0004-properties-drawer-empty",
        headingLevel = 1,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

  ps
    "0004-properties-drawer-not-empty"
    L.OrgDoc
      { cells =
          [ L.CHeading
              ( L.Heading
                  { level = 1,
                    body =
                      [ L.PTokenStyledText
                          (L.StyledText {body = "foo", style = L.styleMaskFrom []})
                      ],
                    meta =
                      fromList
                        [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6")
                        ]
                  }
              )
          ],
        meta = M.fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0004-properties-drawer-not-empty",
        headingLevel = 1,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

  ps
    "0004-properties-drawer-not-empty-with-comments"
    L.OrgDoc
      { cells =
          [ L.CHeading
              ( L.Heading
                  { level = 1,
                    body =
                      [ L.PTokenStyledText
                          (L.StyledText {body = "foo", style = L.styleMaskFrom []})
                      ],
                    meta =
                      fromList
                        [ ("ID", "b949ea2e-e6d1-453d-9352-5e85a82cd4e6")
                        ]
                  }
              )
          ],
        meta = M.fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0004-properties-drawer-not-empty-with-comments",
        headingLevel = 1,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
        meta = fromList []
      }

  it "accepts empty input for \"notFollowedBy (char ' ')\"" $ do
    MP.parse
      (MP.notFollowedBy (char ' ') :: Parser ())
      "empty input"
      `shouldSucceedOn` ""

  p
    "0005-styled-strings-bold"
    L.OrgDoc
      { cells =
          [ L.CParagraph
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
              ]
          ],
        meta = M.fromList []
      }

  p
    "0005-styled-strings-complex-punctuation"
    L.OrgDoc
      { cells =
          [ L.CParagraph
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
              ]
          ],
        meta = fromList []
      }

  p
    "0005-styled-strings-complex"
    L.OrgDoc
      { cells =
          [ L.CParagraph
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
              ]
          ],
        meta = fromList []
      }

  p
    "0005-styled-strings-compression"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  (L.StyledText {body = "a b c d e ", style = L.styleMaskFrom []}),
                L.PTokenStyledText
                  (L.StyledText {body = "f g h i j", style = L.styleMaskFrom [L.Bold]})
              ]
          ],
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
        meta = fromList []
      }

  pf
    "0005-styled-strings-leftover-active-styles"
    "leftover active styles: [Bold]"
    15

  ps
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
        meta = fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0005-styled-strings-messy-boundaries",
        headingLevel = 1,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

  p
    "0005-styled-strings-no-style"
    L.OrgDoc
      { cells =
          [ plainParagraph "foo"
          ],
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
                      { target = L.LinkTargetOther "hello",
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
                      { target = L.LinkTargetOther "file:foo/bar",
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
                      { target = L.LinkTargetOther "https://foo.org",
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
                      { target = L.LinkTargetOther "https://bar.net",
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
                      { target = L.LinkTargetOther "http://localhost:3000",
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
                      { target = L.LinkTargetOther "ftp://10.0.0.1",
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
        meta = M.fromList []
      }

  ps
    "0006-links"
    L.OrgDoc
      { cells =
          [ L.CParagraph
              [ L.PTokenStyledText
                  ( L.StyledText
                      { body = "foo ",
                        style = L.styleMaskFrom []
                      }
                  ),
                L.PTokenLink
                  ( L.Link
                      { target = L.LinkTargetOther "link-target-only",
                        name =
                          [ L.StyledText
                              { body = "link-target-only",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                  )
              ],
            L.CHeading
              ( L.Heading
                  { level = 1,
                    body =
                      [ L.PTokenLink
                          ( L.Link
                              { target = L.LinkTargetOther "link-target",
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
                      { target = L.LinkTargetOther "a",
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
        meta = M.fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0006-links",
        headingLevel = 1,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

  pf
    "0007-block-src-without-lang"
    "block type src requires a language specifier"
    11

  ps
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
                        [ ("__block_type", "example"),
                          ("name", "foo")
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
        meta = fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0007-blocks-empty",
        headingLevel = 0,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.fromList ["foo"],
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

  ps
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
                          ("name", "end has no preceding blank line")
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
                          ("name", "end has preceding blank line")
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
              ),
            L.CBlock
              ( L.Block
                  { meta =
                      fromList
                        [ ("__block_type", "src"),
                          ("__src_language", "sh"),
                          ("name", "ignore #+end_example because this is a src block")
                        ],
                    body = [L.BTokenString "#+end_example"]
                  }
              )
          ],
        meta = fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0007-blocks-ending",
        headingLevel = 0,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames =
          Set.fromList
            [ "empty",
              "end has no preceding blank line",
              "end has preceding blank line",
              "end has preceding newline (multiple)",
              "end not in beginning of line",
              "ignore #+end_example because this is a src block"
            ],
        tanglePaths = Set.fromList [],
        orgParserConf = L.defaultOrgParserConf
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
                              { target =
                                  L.LinkTargetNoweb
                                    L.LinkNoweb
                                      { path = "test/Lilac/ParseSpec/0007-blocks-noweb",
                                        isCanonicalPath = True,
                                        name = "foo"
                                      },
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
                              { target =
                                  L.LinkTargetNoweb
                                    L.LinkNoweb
                                      { path = "test/Lilac/ParseSpec/0007-blocks-noweb",
                                        isCanonicalPath = True,
                                        name = "bar"
                                      },
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
        meta = fromList []
      }

  p
    "0007-blocks-noweb-intraline"
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
                      [ L.BTokenString "(",
                        L.BTokenLink
                          ( L.Link
                              { target =
                                  L.LinkTargetNoweb
                                    L.LinkNoweb
                                      { path = "test/Lilac/ParseSpec/0007-blocks-noweb-intraline",
                                        isCanonicalPath = True,
                                        name = "foo"
                                      },
                                name =
                                  [ L.StyledText
                                      { body = "foo",
                                        style = L.styleMaskFrom []
                                      }
                                  ]
                              }
                          ),
                        L.BTokenString ") and \"",
                        L.BTokenLink
                          ( L.Link
                              { target =
                                  L.LinkTargetNoweb
                                    L.LinkNoweb
                                      { path = "test/Lilac/ParseSpec/0007-blocks-noweb-intraline",
                                        isCanonicalPath = True,
                                        name = "bar"
                                      },
                                name =
                                  [ L.StyledText
                                      { body = "bar",
                                        style = L.styleMaskFrom []
                                      }
                                  ]
                              }
                          ),
                        L.BTokenString "\" and <",
                        L.BTokenLink
                          ( L.Link
                              { target =
                                  L.LinkTargetNoweb
                                    L.LinkNoweb
                                      { path = "test/Lilac/ParseSpec/0007-blocks-noweb-intraline",
                                        isCanonicalPath = True,
                                        name = "ok"
                                      },
                                name =
                                  [ L.StyledText
                                      { body = "ok",
                                        style = L.styleMaskFrom []
                                      }
                                  ]
                              }
                          ),
                        L.BTokenString ">"
                      ]
                  }
              )
          ],
        meta = fromList []
      }

  pf
    "0007-blocks-duplicate-names"
    "block name \"foo\" is not unique"
    52

  pf
    "0007-blocks-duplicate-tangle-paths"
    "tangle path \"some/path.sh\" is not unique"
    114

  p
    "0007-blocks-special-commas"
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
                      [ L.BTokenString
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
          ],
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
        meta = fromList []
      }

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
    L.OrgDoc
      { cells =
          [ plainParagraph "1)",
            plainParagraph "2)"
          ],
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
                  body = "1 + 1 = 2"
                },
            L.CMathFragment
              L.MathFragment
                { meta =
                    fromList
                      [ ("__equation_number", "2")
                      ],
                  body = "1 + 1 = 2"
                },
            L.CMathFragment
              L.MathFragment
                { meta = fromList [],
                  body = "2 + 2 = 4"
                },
            L.CMathFragment
              L.MathFragment
                { meta =
                    fromList
                      [ ("name", "eq:x")
                      ],
                  body = "2 + 2 = 4"
                },
            L.CMathFragment
              L.MathFragment
                { meta =
                    fromList
                      [ ("__equation_number", "3"),
                        ("name", "eq:y")
                      ],
                  body = "1 + 1 = 2"
                }
          ],
        meta = fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0010-standalone-math-fragments",
        headingLevel = 0,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 3,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

  ps
    "0010-standalone-math-inside-lists"
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
            L.CMathFragment
              L.MathFragment
                { meta =
                    fromList
                      [ ("name", "eq:x")
                      ],
                  body = "2 + 2 = 4"
                },
            L.CMathFragment
              L.MathFragment
                { meta =
                    fromList
                      [ ("__equation_number", "1"),
                        ("name", "eq:y")
                      ],
                  body = "1 + 1 = 2"
                },
            L.CListItem
              [ L.LItemEnd,
                L.LOrderedEnd
              ]
          ],
        meta = fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0010-standalone-math-inside-lists",
        headingLevel = 0,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 1,
        blockNames = Set.empty,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

  ps
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
                      { target = L.LinkTargetOther "./image/hello.png",
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
                      { target = L.LinkTargetOther "./image/world.png",
                        name =
                          [ L.StyledText
                              { body = "./image/world.png",
                                style = L.styleMaskFrom []
                              }
                          ]
                      }
                }
          ],
        meta = fromList []
      }
    L.OrgParserState
      { sourcePath = "test/Lilac/ParseSpec/0011-figures-and-blocks",
        headingLevel = 0,
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames =
          Set.fromList
            [ "f:f",
              "f:g"
            ],
        tanglePaths = Set.fromList [],
        orgParserConf = L.defaultOrgParserConf
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
                                  { target = L.LinkTargetOther "https://en.wikipedia.org/wiki/Pythagorean_theorem",
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
                                  { target = L.LinkTargetOther "https://en.wikipedia.org/wiki/Mass%E2%80%93energy_equivalence",
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
                                  { target = L.LinkTargetOther "https://en.wikipedia.org/wiki/Pythagorean_theorem",
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
                                  { target = L.LinkTargetOther "https://en.wikipedia.org/wiki/Mass%E2%80%93energy_equivalence",
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
        meta = fromList []
      }

  pOrgIdsElisp
    "0200-org-ids-elisp-file-empty"
    []

  pOrgIdsElisp
    "0200-org-ids-elisp-file-empty-list"
    []

  pOrgIdsElisp
    "0200-org-ids-elisp-file"
    [ ("a1e6d0d5-4540-423b-864e-b96ad06c56eb", "note/raw/recreation.org"),
      ("3c576a02-418b-431b-ab96-e4936e977fb7", "../prog/lilac/lilac.org"),
      ("ce9af5ef-6559-4ffb-82c1-a439214eda6d", "../prog/lilac/lilac.org"),
      ("8f5752c0-6162-4de6-9ceb-f78ba584722c", "../prog/lilac/lilac.org"),
      ("b449a097-d702-4fd6-98de-cb81f7e49177", "../prog/lilac/lilac.org")
    ]

  pOrgIdsElisp
    "0200-org-ids-elisp-file-oneline"
    [ ("a1e6d0d5-4540-423b-864e-b96ad06c56eb", "note/raw/recreation.org"),
      ("3c576a02-418b-431b-ab96-e4936e977fb7", "../prog/lilac/lilac.org"),
      ("ce9af5ef-6559-4ffb-82c1-a439214eda6d", "../prog/lilac/lilac.org"),
      ("8f5752c0-6162-4de6-9ceb-f78ba584722c", "../prog/lilac/lilac.org"),
      ("b449a097-d702-4fd6-98de-cb81f7e49177", "../prog/lilac/lilac.org")
    ]

  pOrgIdsElisp
    "0200-org-ids-elisp-file-caps"
    [ ("a1e6d0d5-4540-423b-864e-b96ad06c56eb", "note/raw/recreation.org")
    ]

  pfOrgIdsElisp
    "0200-org-ids-elisp-file-non-uuid"
    "\"b449a097-d702-4fd6-98de-cb81f7e49177e\" is not a valid UUID"
    336

  pfOrgIdsElisp
    "0200-org-ids-elisp-file-non-uuid-x"
    "\"a1e6d0d5-4540-423b-864e-b96ad06c56ex\" is not a valid UUID"
    66

  pfOrgIdsElisp
    "0200-org-ids-elisp-file-duplicate-uuid"
    "duplicate UUID: \"b449a097-d702-4fd6-98de-cb81f7e49177\""
    335
