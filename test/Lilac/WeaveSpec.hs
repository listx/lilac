module Lilac.WeaveSpec (spec) where

import Data.Text qualified as T
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Types
  ( WeaveConf (WeaveConf, archive, originObjectPath, watch),
    encodeGPath,
    nilOidOrgDoc,
  )
import Lilac.Util qualified as LU
import Lilac.Weave qualified as LW
import Relude
  ( Bool (True),
    Either (Left, Right),
    FilePath,
    IO,
    String,
    Text,
    foldl',
    fst,
    map,
    pure,
    reverse,
    runStateT,
    sequence_,
    show,
    ($),
    (.),
    (<>),
    (==),
  )
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    runIO,
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
          (runStateT LP.orgDocParser LP.defaultOrgParserState)
          path
          (nilOidOrgDoc <> inputOrgFile)
  case parseResult of
    Left errBundle -> pure . Left $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      let object = LC.mkObject orgDoc
      case LC.mkArchive [(inputOrgFileGPath, object)] of
        Left errMsg -> pure $ Left errMsg
        Right archive -> do
          let weaveConf =
                WeaveConf
                  { archive = archive,
                    originObjectPath = inputOrgFileGPath,
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
