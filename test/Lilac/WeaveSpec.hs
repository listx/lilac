module Lilac.WeaveSpec (spec) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Lilac.Parse qualified as LP
import Lilac.Types (OID, OrgParserState (goids), nilOidOrgDoc)
import Lilac.Util qualified as LU
import Lilac.Weave qualified as LW
import Relude
  ( Bool (True),
    Either (Left, Right),
    FilePath,
    IO,
    Map,
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
import System.OsPath (OsPath)
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
  let goids = M.empty :: Map OID OsPath
  inputOrgFile <- LU.readFrom "test/Lilac/WeaveSpec/" path
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser LP.defaultOrgParserState {goids = goids})
          path
          (nilOidOrgDoc <> inputOrgFile)
  case parseResult of
    Left errBundle -> pure . Left $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      let weaveConf =
            LW.WeaveConf
              { orgDoc = orgDoc,
                goids = goids,
                watch = True
              }
      pure
        . Right
        $ LW.renderAsText weaveConf LW.weave

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
