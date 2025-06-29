module Lilac.ParseSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Lilac.Parse qualified as L
import Lilac.Types
  ( BToken (BTokenLink, BTokenString),
    Block (Block, body, meta, parentOid),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Figure (Figure, link, meta),
    Heading (Heading, body, level, meta, section),
    Link (Link, name, target),
    LinkNoweb (LinkNoweb, name, oid),
    LinkTarget (LinkTargetNoweb, LinkTargetOther, LinkTargetURI),
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
    OrgDoc (OrgDoc, cells, meta, oid),
    OrgParserConf (maxHeadingLevel),
    OrgParserState
      ( OrgParserState,
        anonBlockNumber,
        blockNames,
        continuationIndent,
        docOid,
        equationNumber,
        goids,
        headingLevel,
        headingSection,
        listLevels,
        listMarkers,
        orgId,
        orgParserConf,
        styleMask,
        tanglePaths
      ),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Style (Bold, Code, Italic, Underline, Verbatim),
    StyledText (StyledText, body, style),
    Table (Table, grid, meta),
    fromText,
    fromWords,
    nilOid,
    nilOidOrgDoc,
  )
import Lilac.Util qualified as LU
import Network.URI qualified as N
import Relude
  ( FilePath,
    Identity,
    Int,
    Maybe (Just, Nothing),
    String,
    Type,
    fromList,
    map,
    one,
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
import System.OsPath qualified as P
import Test.Hspec (Spec, before, describe, it, shouldBe)
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

p :: FilePath -> OrgDoc -> Spec
p path expected = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path check
  where
    check input
      | expected.oid == nilOid =
          action nilOidOrgDoc input
            $ L.defaultOrgParserState {orgId = one $ Just nilOid}
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

pfm :: FilePath -> ET T.Text -> Int -> Spec
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

plainParagraph :: T.Text -> Cell
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

pGOIDs :: FilePath -> [(T.Text, FilePath)] -> Spec
pGOIDs path expected = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT (L.goidsParser $ P.unsafeEncodeUtf path) st)
        path
        input
        `shouldParse` ( M.fromList $ map f expected,
                        L.defaultGOIDsParserState
                      )
  where
    st = L.defaultGOIDsParserState
    f :: (T.Text, FilePath) -> (OID, P.OsPath)
    f (oidText, filePath)
      | Just oid <- fromText oidText,
        Just osPath <- P.encodeUtf filePath =
          (oid, osPath)
      | otherwise = (nilOid, P.pack [])

pfGOIDs :: FilePath -> String -> Int -> Spec
pfGOIDs path errMsg pos = do
  before (LU.readFrom "test/Lilac/ParseSpec/" path) $ do
    it path $ \input -> do
      MP.runParser
        (runStateT (L.goidsParser $ P.unsafeEncodeUtf path) st)
        path
        input
        `shouldFailWith` (errFancy pos . fancy $ MP.ErrorFail errMsg)
  where
    st = L.defaultGOIDsParserState

nilOrgDoc :: [Cell] -> OrgDoc
nilOrgDoc cells =
  OrgDoc
    { oid = nilOid,
      cells = cells,
      meta = M.fromList [("ID", "00000000-0000-0000-0000-000000000000")]
    }

parseSpec :: Spec
parseSpec = do
  it "empty input" $ do
    MP.runParser
      (runStateT L.orgDocParser L.defaultOrgParserState)
      "empty input"
      nilOidOrgDoc
      `shouldParse` (L.defaultOrgDoc nilOid [], L.defaultOrgParserState)

  before (LU.readFrom "" "lilac.org") $ do
    it "self-parse" $ \input -> do
      MP.runParser
        (runStateT L.orgDocParser L.defaultOrgParserState)
        "lilac.org"
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = one $ Just nilOid,
        headingLevel = 0,
        headingSection = [],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

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
    OrgParserState
      { goids = fromList [],
        docOid = fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f,
        orgId = one . Just $ fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f,
        headingLevel = 0,
        headingSection = [],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
    OrgParserState
      { goids = fromList [],
        docOid = fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f,
        orgId = one . Just $ fromWords 0x7af1aa3e41ca4810 0xa0012fdf10768f2f,
        headingLevel = 0,
        headingSection = [],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = NE.fromList $ (take 12 $ repeat Nothing) <> [Just nilOid],
        headingLevel = 12,
        headingSection = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = NE.fromList $ (take 12 $ repeat Nothing) <> [Just nilOid],
        headingLevel = 12,
        headingSection = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
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

  describe "genOrgIdStack" $ do
    it "replaces the head if the nesting level matches (no heading yet)" $ do
      L.genOrgIdStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        (NE.fromList [Nothing])
        `shouldBe` ( NE.fromList
                       [ Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa,
                         Nothing
                       ]
                   )
    it "replaces the head if the nesting level matches (one heading exists)" $ do
      L.genOrgIdStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        (NE.fromList [Just $ fromWords 0x7777777777777777 0x7777777777777777, Nothing])
        `shouldBe` ( NE.fromList
                       [ Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa,
                         Nothing
                       ]
                   )
    it "adds a new head if the heading nests (and this heading has an ID)" $ do
      L.genOrgIdStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        2
        (NE.fromList [Nothing, Nothing])
        `shouldBe` ( NE.fromList
                       [ Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa,
                         Nothing,
                         Nothing
                       ]
                   )
    it "adds a new head if the heading nests (and this heading does not have an ID)" $ do
      L.genOrgIdStack
        Nothing
        2
        (NE.fromList [Nothing, Nothing])
        `shouldBe` ( NE.fromList
                       [ Nothing,
                         Nothing,
                         Nothing
                       ]
                   )
    it "truncates if we unnest (difference of 1 level)" $ do
      L.genOrgIdStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        (NE.fromList [Just $ fromWords 0x7777777777777777 0x7777777777777777, Nothing, Nothing])
        `shouldBe` ( NE.fromList
                       [ Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa,
                         Nothing
                       ]
                   )
    it "truncates if we unnest (multiple levels)" $ do
      L.genOrgIdStack
        Nothing
        1
        (NE.fromList [Nothing, Nothing, Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa, Nothing])
        `shouldBe` ( NE.fromList
                       [ Nothing,
                         Nothing
                       ]
                   )
    it "truncates if we unnest (multiple levels, new heading has ID)" $ do
      L.genOrgIdStack
        (Just "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
        1
        (NE.fromList [Nothing, Nothing, Just $ fromWords 0x7777777777777777 0x7777777777777777, Nothing])
        `shouldBe` ( NE.fromList
                       [ Just $ fromWords 0xaaaaaaaaaaaaaaaa 0xaaaaaaaaaaaaaaaa,
                         Nothing
                       ]
                   )

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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = NE.fromList [Nothing, Just nilOid],
        headingLevel = 1,
        headingSection = [1],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = NE.fromList [Just $ fromWords 0xb949ea2ee6d1453d 0x93525e85a82cd4e6, Just nilOid],
        headingLevel = 1,
        headingSection = [1],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = NE.fromList [Just $ fromWords 0xb949ea2ee6d1453d 0x93525e85a82cd4e6, Just nilOid],
        headingLevel = 1,
        headingSection = [1],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = NE.fromList [Nothing, Just nilOid],
        headingLevel = 1,
        headingSection = [1],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
                  { target = LinkTargetOther "hello",
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
                  { target = LinkTargetOther "file:foo/bar",
                    name =
                      [ StyledText
                          { body = "file:foo/bar",
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
                    { target = LinkTargetOther "link-target-only",
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
                            { target = LinkTargetOther "link-target",
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
                    { target = LinkTargetOther "a",
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = NE.fromList [Nothing, Just nilOid],
        headingLevel = 1,
        headingSection = [1],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
      }

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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = one $ Just nilOid,
        headingLevel = 0,
        headingSection = [],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 0,
        blockNames = Set.fromList ["foo"],
        anonBlockNumber = 2,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = one $ Just nilOid,
        headingLevel = 0,
        headingSection = [],
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
        anonBlockNumber = 0,
        tanglePaths = Set.fromList [],
        orgParserConf = L.defaultOrgParserConf
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
                                LinkTargetNoweb
                                  LinkNoweb
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
                                LinkTargetNoweb
                                  LinkNoweb
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
                                LinkTargetNoweb
                                  LinkNoweb
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
                                LinkTargetNoweb
                                  LinkNoweb
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
                                LinkTargetNoweb
                                  LinkNoweb
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
    "block name \"foo\" is not unique"
    52

  pf
    "0007-blocks-duplicate-tangle-paths"
    "tangle path \"some/path.sh\" is not unique"
    114

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
              { meta = fromList [],
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = one $ Just nilOid,
        headingLevel = 0,
        headingSection = [],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 3,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = one $ Just nilOid,
        headingLevel = 0,
        headingSection = [],
        styleMask = L.styleMaskFrom [],
        listLevels = [],
        listMarkers = [],
        continuationIndent = 0,
        equationNumber = 1,
        blockNames = Set.empty,
        anonBlockNumber = 0,
        tanglePaths = Set.empty,
        orgParserConf = L.defaultOrgParserConf
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
                    [ ("attr_html", ":width 100%"),
                      ("name", "fig:hello")
                    ],
                link =
                  Link
                    { target = LinkTargetOther "./image/hello.png",
                      name =
                        [ StyledText
                            { body = "./image/hello.png",
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
              { meta = fromList [],
                link =
                  Link
                    { target = LinkTargetOther "./image/world.png",
                      name =
                        [ StyledText
                            { body = "./image/world.png",
                              style = L.styleMaskFrom []
                            }
                        ]
                    }
              }
        ]
    )
    OrgParserState
      { goids = fromList [],
        docOid = nilOid,
        orgId = one $ Just nilOid,
        headingLevel = 0,
        headingSection = [],
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
        anonBlockNumber = 0,
        tanglePaths = Set.fromList [],
        orgParserConf = L.defaultOrgParserConf
      }

  p
    "0012-tables-and-prose"
    $ nilOrgDoc
      [ CTable
          Table
            { meta =
                fromList
                  [ ("name", "tbl:famous-equations")
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
                      []
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
                              { target = LinkTargetOther "https://wiki/x",
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
                              { target = LinkTargetOther "https://wiki/y",
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
            }
      ]

  p
    "0012-tables-inside-lists"
    $ nilOrgDoc
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
                    ("bar", "baz")
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
                      []
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
                              { target = LinkTargetOther "https://wiki/x",
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
                              { target = LinkTargetOther "https://wiki/y",
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

  pGOIDs
    "0200-goids-file-empty"
    []

  pGOIDs
    "0200-goids-file-empty-list"
    []

  pGOIDs
    "0200-goids-file"
    [ ("a1e6d0d5-4540-423b-864e-b96ad06c56eb", "./note/raw/recreation.org"),
      ("3c576a02-418b-431b-ab96-e4936e977fb7", "./../prog/lilac/lilac.org"),
      ("ce9af5ef-6559-4ffb-82c1-a439214eda6d", "./../prog/lilac/lilac.org"),
      ("8f5752c0-6162-4de6-9ceb-f78ba584722c", "./../prog/lilac/lilac.org"),
      ("b449a097-d702-4fd6-98de-cb81f7e49177", "./../prog/lilac/lilac.org")
    ]

  pGOIDs
    "0200-goids-file-oneline"
    [ ("a1e6d0d5-4540-423b-864e-b96ad06c56eb", "./a"),
      ("3c576a02-418b-431b-ab96-e4936e977fb7", "./b")
    ]

  pGOIDs
    "0200-goids-file-caps"
    [ ("a1e6d0d5-4540-423b-864e-b96ad06c56eb", "./note/raw/recreation.org")
    ]

  pfGOIDs
    "0200-goids-file-non-oid"
    "\"b449a097-d702-4fd6-98de-cb81f7e49177e\" is not a valid OID"
    336

  pfGOIDs
    "0200-goids-file-non-oid-x"
    "\"a1e6d0d5-4540-423b-864e-b96ad06c56ex\" is not a valid OID"
    66

  pfGOIDs
    "0200-goids-file-duplicate-oid"
    "duplicate OID: \"b449a097-d702-4fd6-98de-cb81f7e49177\""
    335

  pGOIDs
    "0200-goids-file-absolute"
    [ ("a1e6d0d5-4540-423b-864e-b96ad06c56eb", "/foo/bar"),
      ("3c576a02-418b-431b-ab96-e4936e977fb7", "/baz"),
      ("ce9af5ef-6559-4ffb-82c1-a439214eda6d", "/baz"),
      ("8f5752c0-6162-4de6-9ceb-f78ba584722c", "/baz"),
      ("b449a097-d702-4fd6-98de-cb81f7e49177", "/baz")
    ]
