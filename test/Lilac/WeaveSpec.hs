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
    LinkTarget (LinkTargetURI),
    PToken (PTokenLink, PTokenStyledText),
    Prose (Prose, unProse),
    Style (Italic),
    StyledText (StyledText, body, style),
    WeaveConf (WeaveConf, archive, citeprocResult, originObjectPath, watch),
    defaultOrgParserState,
    encodeGPath,
    nilOidOrgDoc,
  )
import Lilac.Util qualified as LU
import Lilac.Weave qualified as LW
import Network.URI qualified as N
import Relude
  ( Bool (True),
    Either (Left, Right),
    FilePath,
    IO,
    Maybe (Just, Nothing),
    String,
    Text,
    foldl',
    fst,
    map,
    mapM_,
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
    shouldStartWith,
  )
import Text.Megaparsec qualified as MP

spec :: Spec
spec = weaveSpec

getWovenPrettyFromPath :: FilePath -> IO (Either String Text)
getWovenPrettyFromPath path = do
  inputOrgFileGPath <- encodeGPath $ "test/Lilac/WeaveSpec/" <> path
  inputOrgFile <- LU.readFrom "test/Lilac/WeaveSpec/" path
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser defaultOrgParserState)
          path
          (nilOidOrgDoc <> inputOrgFile)
  case parseResult of
    Left errBundle -> pure . Left $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      object <- LC.mkObject orgDoc
      maybeArchive <- LC.mkArchive [(inputOrgFileGPath, object)]
      case maybeArchive of
        Left errMsg -> pure $ Left errMsg
        Right archive -> do
          result <- LW.prepareCitations inputOrgFileGPath object archive
          let weaveConf =
                WeaveConf
                  { archive = archive,
                    originObjectPath = inputOrgFileGPath,
                    citeprocResult = result,
                    watch = True
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
          (nilOidOrgDoc <> inputOrgFile)
  case parseResult of
    Left errBundle -> pure . Left $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      object <- LC.mkObject orgDoc
      maybeArchive <- LC.mkArchive [(inputOrgFileGPath, object)]
      case maybeArchive of
        Left errMsg -> pure $ Left errMsg
        Right archive ->
          Right <$> LW.prepareCitations inputOrgFileGPath object archive

expectCiteprocResult :: FilePath -> CP.Result Prose -> Spec
expectCiteprocResult path expected = do
  result <- runIO $ getCiteprocResultFromPath path
  describe path $ do
    case result of
      Right got -> do
        it "citations" $ do
          got.resultCitations `shouldBe` expected.resultCitations
        it "bibliography" $ do
          got.resultBibliography `shouldBe` expected.resultBibliography
        it "warnings" $ do
          got.resultWarnings `shouldBe` expected.resultWarnings
      Left errMsg -> it "programmer error" $ expectationFailure errMsg

weaveSpec :: Spec
weaveSpec = do
  weaveExpect
    "0300-weave-styled-text"
    [ "<p>regular ",
      "<span class=\"bold\">bold</span> ",
      "<span class=\"italic\">italic</span> ",
      "<span class=\"underline\">underline</span> ",
      "<span class=\"verbatim\">verbatim</span> ",
      "<span class=\"code\">code</span> ",
      "<span class=\"verbatim\">&lt;&gt;</span> ",
      "<span class=\"code\">&lt;&gt;</span>",
      "</p>"
    ]

  weaveExpect
    "0300-weave-lists"
    $ map
      (T.intercalate "" . map T.strip . T.lines)
      [ """
        <ol>
          <li>
            <p>
              a
            </p>
            <ul>
              <li>
                <p>
                  b
                </p>
              </li>
            </ul>
          </li>
          <li>
            <p>
              c
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

  expectCiteprocResult
    "0400-citeproc-basic"
    CP.Result
      { resultCitations =
          [ Prose
              { unProse =
                  [ PTokenLink
                      Link
                        { name =
                            [ StyledText {body = "[1]", style = styleMaskFrom []}
                            ],
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
                    PTokenStyledText
                      StyledText
                        { body = ", ",
                          style = styleMaskFrom []
                        },
                    PTokenLink
                      Link
                        { name =
                            [ StyledText {body = "[2]", style = styleMaskFrom []}
                            ],
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
                    PTokenStyledText StyledText {body = ", ", style = styleMaskFrom []},
                    PTokenLink
                      Link
                        { name =
                            [ StyledText {body = "[3]", style = styleMaskFrom []}
                            ],
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
                    PTokenStyledText StyledText {body = ", ", style = styleMaskFrom []},
                    PTokenLink
                      Link
                        { name =
                            [ StyledText {body = "[4]", style = styleMaskFrom []}
                            ],
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
                    [ PTokenStyledText StyledText {body = "[1]D. E. Knuth, ", style = styleMaskFrom []},
                      PTokenStyledText StyledText {body = "Literate programming", style = styleMaskFrom [Italic]},
                      PTokenStyledText StyledText {body = ". in CSLI lecture notes, no. no. 27. Stanford, Calif.: Center for the Study of Language; Information, 1992.", style = styleMaskFrom []}
                    ]
                }
            ),
            ( "b",
              Prose
                { unProse =
                    [ PTokenStyledText (StyledText {body = "[2]E. W. Dijkstra, \8220Letters to the editor: Go to statement considered harmful,\8221 ", style = styleMaskFrom []}),
                      PTokenStyledText (StyledText {body = "Communications of the ACM", style = styleMaskFrom [Italic]}),
                      PTokenStyledText (StyledText {body = ", vol. 11, no. 3, pp. 147\8211\&148, Mar. 1968, doi: ", style = styleMaskFrom []}),
                      PTokenLink
                        Link
                          { name =
                              [ StyledText {body = "10.1145/362929.362947", style = styleMaskFrom []}
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
                                    N.uriPath = "/10.1145/362929.362947",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      PTokenStyledText StyledText {body = ".", style = styleMaskFrom []}
                    ]
                }
            ),
            ( "c",
              Prose
                { unProse =
                    [ PTokenStyledText StyledText {body = "[3]A. M. Turing, \8220On Computable Numbers, with an Application to the Entscheidungsproblem,\8221 ", style = styleMaskFrom []},
                      PTokenStyledText StyledText {body = "Proceedings of the London Mathematical Society", style = styleMaskFrom [Italic]},
                      PTokenStyledText StyledText {body = ", vol. s2\8211\&42, no. 1, pp. 230\8211\&265, 1937, doi: ", style = styleMaskFrom []},
                      PTokenLink
                        Link
                          { name =
                              [ StyledText {body = "10.1112/plms/s2-42.1.230", style = styleMaskFrom []}
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
                                    N.uriPath = "/10.1112/plms/s2-42.1.230",
                                    N.uriQuery = "",
                                    N.uriFragment = ""
                                  }
                          },
                      PTokenStyledText StyledText {body = ".", style = styleMaskFrom []}
                    ]
                }
            ),
            ( "d",
              Prose
                { unProse =
                    [ PTokenStyledText StyledText {body = "[4]C. E. Shannon, \8220A Mathematical Theory of Communication,\8221 ", style = styleMaskFrom []},
                      PTokenStyledText StyledText {body = "Bell System Technical Journal", style = styleMaskFrom [Italic]},
                      PTokenStyledText StyledText {body = ", vol. 27, no. 3, pp. 379\8211\&423, July 1948, doi: ", style = styleMaskFrom []},
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
                      PTokenStyledText StyledText {body = ".", style = styleMaskFrom []}
                    ]
                }
            )
          ],
        resultWarnings = []
      }
