module Lilac.WeaveSpec (spec) where

import Citeproc
  ( Result (resultBibliography, resultCitations, resultWarnings),
  )
import Citeproc qualified as CP
import Data.Text qualified as T
import Lilac.Compile qualified as LC
import Lilac.Parse (styleMaskFrom)
import Lilac.Parse qualified as LP
import Lilac.Types
  ( Link (Link, name, target),
    LinkOid (LinkOid, name, oid),
    LinkTarget
      ( LinkTargetFootnote,
        LinkTargetOid,
        LinkTargetURI
      ),
    MathFragment (MathFragment, body, meta, parentOid),
    PToken
      ( PTokenDivEnd,
        PTokenDivStart,
        PTokenLink,
        PTokenMathFragment
      ),
    ProjectConf (ProjectConf, homepage, name, repo, version),
    Prose (Prose, unProse),
    Style (Bold, Code, Italic, Smallcaps, Superscript, Verbatim),
    StyledText (StyledText, body, style),
    WeaveConf
      ( WeaveConf,
        archive,
        citeprocResult,
        originObjectPath,
        outDir,
        projectConf
      ),
    defaultOrgParserState,
    encodeGPath,
    nilOid,
    nilOidOrgDocPrefix,
    ptext,
    stext,
  )
import Lilac.Util qualified as LU
import Lilac.Weave qualified as LW
import Network.URI qualified as N
import Relude
  ( Either (Left, Right),
    FilePath,
    IO,
    Maybe (Just, Nothing),
    String,
    Text,
    foldl',
    forM_,
    fromList,
    fst,
    map,
    mapM_,
    otherwise,
    pure,
    reverse,
    runStateT,
    sequence_,
    show,
    ($),
    (.),
    (<$>),
    (<>),
    (==),
  )
import System.OsPath qualified as P
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    runIO,
    shouldBe,
    shouldContain,
    shouldStartWith,
  )
import Text.Megaparsec qualified as MP

spec :: Spec
spec = do
  weaveExpect
    "0300-weave-styled-text"
    [ "<p><span id=\"c-0\" class=\"cell-idx\">regular ",
      "<span class=\"bold\">bold</span> ",
      "<span class=\"italic\">italic</span> ",
      "<span class=\"underline\">underline</span> ",
      "<span class=\"verbatim\">verbatim</span> ",
      "<span class=\"code\">code</span> ",
      "<span class=\"verbatim\">&lt;&gt;</span> ",
      "<span class=\"code\">&lt;&gt;</span>",
      "</span></p>"
    ]

  weaveExpect
    "0300-weave-lists"
    $ map
      (T.intercalate "" . map T.strip . T.lines)
      [ """
        <ol>
          <li>
            <p>
              <span id="c-1" class="cell-idx">
                a
              </span>
            </p>
            <ul>
              <li>
                <p>
                  <span id="c-3" class="cell-idx">
                    b
                  </span>
                </p>
              </li>
            </ul>
          </li>
          <li>
            <p>
              <span id="c-5" class="cell-idx">
                c
              </span>
            </p>
          </li>
        </ol>
        """
      ]

  it "0301-weave-external-links-common-prefix" $ do
    mapM_
      ( \(a, b, c) ->
          ( P.joinPath
              $ LW.commonPrefix
                (P.splitDirectories $ P.unsafeEncodeUtf a)
                (P.splitDirectories $ P.unsafeEncodeUtf b)
          )
            `shouldBe` (P.unsafeEncodeUtf c)
      )
      [ ("from.html", "to.html1", ""),
        ("base/from.html", "base/to.html2", "base"),
        ("1base/a/b/from.html", "1base/a/b/to.html3", "1base/a/b"),
        ("2base/a/b/from.html", "2base/a/b/c/to.html4", "2base/a/b"),
        ("3base/a/b/from.html", "3base/a/b/c/d/to.html5", "3base/a/b"),
        ("a/b/c", "x/y/z", "")
      ]

  it "0301-weave-external-links" $ do
    mapM_
      ( \(a, b, c) ->
          LW.getRelativePathWithoutSymlinks
            (P.unsafeEncodeUtf a)
            (P.unsafeEncodeUtf b)
            `shouldBe` (P.unsafeEncodeUtf c)
      )
      [ ("from.html", "to.html1", "to.html1"),
        ("base/from.html", "base/to.html2", "to.html2"),
        ("base/a/b/from.html", "base/a/b/to.html3", "to.html3"),
        ("base/a/b/from.html", "base/a/b/c/to.html4", "c/to.html4"),
        ("base/a/b/from.html", "base/a/b/c/d/to.html5", "c/d/to.html5"),
        ("base/a/b/from.html", "to.html6", "../../../to.html6"),
        ("base/a/b/from.html", "base/a/c/to.html7", "../c/to.html7"),
        ("base/a/b/c/d/from.html", "base/a/x/y/to.html8", "../../../x/y/to.html8")
      ]

  describe "Object extension to org conversion" $ do
    checkObjectExtensionConversion
      "empty case"
      [ ( [],
          []
        ),
        ( ["foo.lobj"],
          ["foo.org"]
        ),
        ( ["x.lobj", "a"],
          ["x.org", "a"]
        ),
        ( ["1.lobj", "b", "a"],
          ["1.org", "b", "a"]
        ),
        --
        ( ["x", "b", "a"],
          [".org", "b", "a"]
        ),
        ( ["", "b", "a"],
          [".org", "b", "a"]
        )
      ]

  expectCiteprocResult
    "0400-citeproc-basic"
    $ Right
      CP.Result
        { resultCitations =
            [ Prose
                { unProse =
                    [ PTokenLink
                        Link
                          { name = [stext "[1]" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-a",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext ", " [],
                      PTokenLink
                        Link
                          { name = [stext "[2]" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-b",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext ", " [],
                      PTokenLink
                        Link
                          { name = [stext "[3]" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-c",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext ", " [],
                      PTokenLink
                        Link
                          { name = [stext "[4]" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-d",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          }
                    ]
                }
            ],
          resultBibliography =
            [ ( "a",
                Prose
                  { unProse =
                      [ PTokenDivStart "left-margin",
                        ptext "[1]" [],
                        PTokenDivEnd,
                        PTokenDivStart "right-inline",
                        ptext "D. E. Knuth, " [],
                        ptext "Literate programming" [Italic],
                        ptext ". in CSLI lecture notes, no. no. 27. Stanford, Calif.: Center for the Study of Language; Information, 1992." [],
                        PTokenDivEnd
                      ]
                  }
              ),
              ( "b",
                Prose
                  { unProse =
                      [ PTokenDivStart "left-margin",
                        ptext "[2]" [],
                        PTokenDivEnd,
                        PTokenDivStart "right-inline",
                        ptext "E. W. Dijkstra, \8220Letters to the editor: Go to statement considered harmful,\8221 " [],
                        ptext "Communications of the ACM" [Italic],
                        ptext ", vol. 11, no. 3, pp. 147\8211\&148, Mar. 1968, doi: " [],
                        PTokenLink
                          Link
                            { name = [stext "10.1145/362929.362947" []],
                              target =
                                LinkTargetURI
                                  N.URI
                                    { N.uriScheme = "https:",
                                      N.uriAuthority =
                                        Just
                                          N.URIAuth
                                            { N.uriUserInfo = "",
                                              N.uriRegName = "doi.org",
                                              N.uriPort = ""
                                            },
                                      N.uriPath = "/10.1145/362929.362947",
                                      N.uriQuery = "",
                                      N.uriFragment = ""
                                    }
                            },
                        ptext "." [],
                        PTokenDivEnd
                      ]
                  }
              ),
              ( "c",
                Prose
                  { unProse =
                      [ PTokenDivStart "left-margin",
                        ptext "[3]" [],
                        PTokenDivEnd,
                        PTokenDivStart "right-inline",
                        ptext "A. M. Turing, \8220On Computable Numbers, with an Application to the Entscheidungsproblem,\8221 " [],
                        ptext "Proceedings of the London Mathematical Society" [Italic],
                        ptext ", vol. s2\8211\&42, no. 1, pp. 230\8211\&265, 1937, doi: " [],
                        PTokenLink
                          Link
                            { name = [stext "10.1112/plms/s2-42.1.230" []],
                              target =
                                LinkTargetURI
                                  N.URI
                                    { N.uriScheme = "https:",
                                      N.uriAuthority =
                                        Just
                                          N.URIAuth
                                            { N.uriUserInfo = "",
                                              N.uriRegName = "doi.org",
                                              N.uriPort = ""
                                            },
                                      N.uriPath = "/10.1112/plms/s2-42.1.230",
                                      N.uriQuery = "",
                                      N.uriFragment = ""
                                    }
                            },
                        ptext "." [],
                        PTokenDivEnd
                      ]
                  }
              ),
              ( "d",
                Prose
                  { unProse =
                      [ PTokenDivStart "left-margin",
                        ptext "[4]" [],
                        PTokenDivEnd,
                        PTokenDivStart "right-inline",
                        ptext "C. E. Shannon, \8220A Mathematical Theory of Communication,\8221 " [],
                        ptext "Bell System Technical Journal" [Italic],
                        ptext ", vol. 27, no. 3, pp. 379\8211\&423, July 1948, doi: " [],
                        PTokenLink
                          Link
                            { name =
                                [ StyledText {body = "10.1002/j.1538-7305.1948.tb01338.x", style = styleMaskFrom []}
                                ],
                              target =
                                LinkTargetURI
                                  N.URI
                                    { N.uriScheme = "https:",
                                      N.uriAuthority =
                                        Just
                                          N.URIAuth
                                            { N.uriUserInfo = "",
                                              N.uriRegName = "doi.org",
                                              N.uriPort = ""
                                            },
                                      N.uriPath = "/10.1002/j.1538-7305.1948.tb01338.x",
                                      N.uriQuery = "",
                                      N.uriFragment = ""
                                    }
                            },
                        ptext "." [],
                        PTokenDivEnd
                      ]
                  }
              )
            ],
          resultWarnings = []
        }

  expectCiteprocResult
    "0400-citeproc-superscript"
    $ Right
      CP.Result
        { resultCitations =
            [ Prose
                { unProse =
                    [ PTokenLink
                        Link
                          { name = [stext "1" [Superscript]],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-a",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          }
                    ]
                }
            ],
          resultBibliography =
            [ ( "a",
                Prose
                  { unProse =
                      [ PTokenDivStart "left-margin",
                        ptext "1. " [],
                        PTokenDivEnd,
                        PTokenDivStart "right-inline",
                        ptext "Knuth DE. Literate programming. Stanford, Calif.: Center for the Study of Language; Information; 1992. (CSLI lecture notes)." [],
                        PTokenDivEnd
                      ]
                  }
              )
            ],
          resultWarnings = []
        }

  expectCiteprocResult
    "0400-citeproc-smallcaps"
    $ Right
      CP.Result
        { resultCitations =
            [ Prose
                { unProse =
                    [ ptext "[" [],
                      PTokenLink
                        Link
                          { name = [stext "Knuth 1992" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-a",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext "]" []
                    ]
                }
            ],
          resultBibliography =
            [ ( "a",
                Prose
                  { unProse =
                      [ ptext "Knuth, D.E." [Smallcaps],
                        ptext " 1992. " [],
                        ptext "Literate programming" [Italic],
                        ptext ". Center for the Study of Language; Information, Stanford, Calif." []
                      ]
                  }
              )
            ],
          resultWarnings = []
        }

  expectCiteprocResult
    "0400-citeproc-multiline"
    $ Right
      CP.Result
        { resultCitations =
            [ Prose
                { unProse =
                    [ ptext "[" [],
                      PTokenLink
                        Link
                          { name = [stext "Knuth 1992" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-a",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext "]" []
                    ]
                }
            ],
          resultBibliography =
            [ ( "a",
                Prose
                  { unProse =
                      [ ptext "Knuth, D.E." [Smallcaps],
                        ptext " 1992. " [],
                        ptext "Literate programming" [Italic],
                        ptext ". Center for the Study of Language; Information, Stanford, Calif." []
                      ]
                  }
              )
            ],
          resultWarnings = []
        }

  expectCiteprocResult
    "0400-citeproc-multiline-complex"
    $ Right
      CP.Result
        { resultCitations =
            [ Prose
                { unProse =
                    [ ptext "[" [],
                      ptext "bold" [Bold],
                      ptext " " [],
                      ptext "italic" [Italic],
                      PTokenLink
                        Link
                          { name = [stext "Knuth 1992" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-a",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext "and really " [],
                      ptext "amazing" [Code],
                      ptext "global suffix]" []
                    ]
                }
            ],
          resultBibliography =
            [ ( "a",
                Prose
                  { unProse =
                      [ ptext "Knuth, D.E." [Smallcaps],
                        ptext " 1992. " [],
                        ptext "Literate programming" [Italic],
                        ptext ". Center for the Study of Language; Information, Stanford, Calif." []
                      ]
                  }
              )
            ],
          resultWarnings = []
        }

  expectCiteprocResult
    "0400-citeproc-multiline-complex-list-item"
    $ Right
      CP.Result
        { resultCitations =
            [ Prose
                { unProse =
                    [ ptext "[global " [],
                      PTokenMathFragment
                        MathFragment
                          { parentOid = nilOid,
                            meta = fromList [],
                            body = "1 + 1\n    = 2"
                          },
                      ptext "bold" [Bold],
                      ptext " " [],
                      ptext "italic" [Italic],
                      PTokenLink
                        Link
                          { name = [stext "1" []],
                            target = LinkTargetFootnote "@a"
                          },
                      PTokenLink
                        Link
                          { name = [stext "Knuth 1992" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-a",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext "and really " [],
                      ptext "amazing" [Code],
                      ptext "; some bit of " [],
                      ptext "@b verbatim" [Verbatim],
                      ptext " for a" [],
                      PTokenLink
                        Link
                          { name = [stext "Turing 1937" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-c",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext "thing global suffix with OID link " [],
                      PTokenLink
                        Link
                          { name = [stext "foo" []],
                            target =
                              LinkTargetOid
                                LinkOid
                                  { oid = nilOid,
                                    name = "foo"
                                  }
                          },
                      ptext "]" []
                    ]
                }
            ],
          resultBibliography =
            [ ( "a",
                Prose
                  { unProse =
                      [ ptext "Knuth, D.E." [Smallcaps],
                        ptext " 1992. " [],
                        ptext "Literate programming" [Italic],
                        ptext ". Center for the Study of Language; Information, Stanford, Calif." []
                      ]
                  }
              ),
              ( "c",
                Prose
                  { unProse =
                      [ ptext "Turing, A.M." [Smallcaps],
                        ptext " 1937. " [],
                        PTokenLink
                          Link
                            { name = [stext "On Computable Numbers, with an Application to the Entscheidungsproblem" []],
                              target =
                                LinkTargetURI
                                  N.URI
                                    { N.uriScheme = "https:",
                                      N.uriAuthority =
                                        Just
                                          N.URIAuth
                                            { N.uriUserInfo = "",
                                              N.uriRegName = "doi.org",
                                              N.uriPort = ""
                                            },
                                      N.uriPath = "/10.1112/plms/s2-42.1.230",
                                      N.uriQuery = "",
                                      N.uriFragment = ""
                                    }
                            },
                        ptext ". " [],
                        ptext "Proceedings of the London Mathematical Society" [Italic],
                        ptext " " [],
                        ptext "s2-42" [Italic],
                        ptext ", 1, 230\8211\&265." []
                      ]
                  }
              )
            ],
          resultWarnings = []
        }

  expectCiteprocResult
    "0400-citeproc-multiline-cell-boundary"
    $ Left "unexpected newline"

  expectCiteprocResult
    "0400-citeproc-multiline-cell-boundary-list-item"
    $ Left "unexpected newline"

  expectCiteprocResult
    "0400-citeproc-style-variations"
    $ Right
      CP.Result
        { resultCitations =
            [ Prose
                { unProse =
                    [ ptext "Knuth [" [],
                      PTokenLink
                        Link
                          { name = [stext "1992" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-a",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext "]" []
                    ]
                },
              Prose
                { unProse =
                    [ ptext "[" [],
                      PTokenLink
                        Link
                          { name = [stext "1937" []],
                            target =
                              LinkTargetURI
                                N.URI
                                  { N.uriScheme = "",
                                    N.uriAuthority = Nothing,
                                    N.uriPath = "#ref-c",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      ptext "]" []
                    ]
                }
            ],
          resultBibliography =
            [ ( "a",
                Prose
                  { unProse =
                      [ ptext "Knuth, D.E." [Smallcaps],
                        ptext " 1992. " [],
                        ptext "Literate programming" [Italic],
                        ptext ". Center for the Study of Language; Information, Stanford, Calif." []
                      ]
                  }
              ),
              ( "c",
                Prose
                  { unProse =
                      [ ptext "Turing, A.M." [Smallcaps],
                        ptext " 1937. " [],
                        PTokenLink
                          Link
                            { name = [stext "On Computable Numbers, with an Application to the Entscheidungsproblem" []],
                              target =
                                LinkTargetURI
                                  N.URI
                                    { N.uriScheme = "https:",
                                      N.uriAuthority =
                                        Just
                                          N.URIAuth
                                            { N.uriUserInfo = "",
                                              N.uriRegName = "doi.org",
                                              N.uriPort = ""
                                            },
                                      N.uriPath = "/10.1112/plms/s2-42.1.230",
                                      N.uriQuery = "",
                                      N.uriFragment = ""
                                    }
                            },
                        ptext ". " [],
                        ptext "Proceedings of the London Mathematical Society" [Italic],
                        ptext " " [],
                        ptext "s2-42" [Italic],
                        ptext ", 1, 230\8211\&265." []
                      ]
                  }
              )
            ],
          resultWarnings = []
        }

getWovenPrettyFromPath :: FilePath -> IO (Either String Text)
getWovenPrettyFromPath path = do
  inputOrgFileGPath <- encodeGPath $ "test/Lilac/WeaveSpec/" <> path
  inputOrgFile <- LU.readFrom "test/Lilac/WeaveSpec/" path
  outDir <- encodeGPath "test/Lilac/WeaveSpec/"
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser defaultOrgParserState)
          path
          (nilOidOrgDocPrefix <> inputOrgFile)
  case parseResult of
    Left errBundle -> pure . Left $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      let object = LC.mkObject orgDoc
      maybeArchive <- LC.mkArchive [(inputOrgFileGPath, object)]
      case maybeArchive of
        Left errMsg -> pure $ Left errMsg
        Right archive -> do
          result <- LW.prepareCitations inputOrgFileGPath object archive
          let projectConf =
                ProjectConf
                  { name = "Test Project",
                    homepage = Nothing,
                    repo = Nothing,
                    version = Nothing
                  }
              weaveConf =
                WeaveConf
                  { projectConf = projectConf,
                    archive = archive,
                    originObjectPath = inputOrgFileGPath,
                    outDir = outDir,
                    citeprocResult = result
                  }
          pure
            . Right
            . LW.renderAsText weaveConf
            $ LW.weaveObject object

searchOrderedNeedles :: Text -> [Text] -> (String, [Expectation])
searchOrderedNeedles haystackRaw needles =
  (\expectations -> ("sees needles", expectations))
    . reverse
    . fst
    $ foldl' f ([], haystackRaw) needles
  where
    f ::
      ([Expectation], Text) ->
      Text ->
      ([Expectation], Text)
    f (testCases, haystack) needle =
      let (discardMe, foundAndRest) = T.breakOn needle haystack
          discardLen = T.length discardMe
          needleStr = T.unpack needle
          foundAndRestStr = T.unpack foundAndRest
          expectation = foundAndRestStr `shouldStartWith` needleStr
          expectationF =
            expectationFailure
              $ "could not find "
              <> (show $ needleStr)
              <> " in haystack:\n"
              <> (T.unpack haystack)
          remainingHaystack = T.drop discardLen haystack
       in if foundAndRest == T.empty
            then (expectationF : testCases, remainingHaystack)
            else (expectation : testCases, remainingHaystack)

weaveExpect :: FilePath -> [Text] -> Spec
weaveExpect path needles = do
  result <- runIO $ getWovenPrettyFromPath path
  describe path $ do
    case result of
      Right haystack -> do
        let (name, expectations) = searchOrderedNeedles haystack needles
        it name $ sequence_ expectations
      Left errMsg -> it "programmer error" $ expectationFailure errMsg

getCiteprocResultFromPath :: FilePath -> IO (Either String (CP.Result Prose))
getCiteprocResultFromPath path = do
  inputOrgFileGPath <- encodeGPath $ "test/Lilac/WeaveSpec/" <> path
  inputOrgFile <- LU.readFrom "test/Lilac/WeaveSpec/" path
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser defaultOrgParserState)
          path
          (nilOidOrgDocPrefix <> inputOrgFile)
  case parseResult of
    Left errBundle -> pure . Left $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      let object = LC.mkObject orgDoc
      maybeArchive <- LC.mkArchive [(inputOrgFileGPath, object)]
      case maybeArchive of
        Left errMsg -> pure $ Left errMsg
        Right archive ->
          Right <$> LW.prepareCitations inputOrgFileGPath object archive

expectCiteprocResult :: FilePath -> Either String (CP.Result Prose) -> Spec
expectCiteprocResult path expected = do
  result <- runIO $ getCiteprocResultFromPath path
  describe path $ do
    if
      | Right got <- result,
        Right expectedResult <- expected ->
          it "matches expected result" $ do
            got `shouldBe` expectedResult
      | Left errMsg <- result,
        Left expectedErrorSubstring <- expected ->
          it "has error substring" $ do
            errMsg `shouldContain` expectedErrorSubstring
      | otherwise ->
          it "unexpected type mismatch" $ do
            result `shouldBe` expected

checkObjectExtensionConversion :: String -> [([Text], [Text])] -> Spec
checkObjectExtensionConversion msg testCases = do
  it msg $ do
    forM_ testCases $ \(input, output) -> do
      LW.convertObjectExtensionToOrg input `shouldBe` output
