module Lilac.WeaveSpec (spec) where

import Data.Text qualified as T
import Lilac.Parse qualified as LP
import Lilac.Util qualified as LU
import Lilac.Weave qualified as LW
import Relude
  ( Bool (True),
    Either (Left, Right),
    FilePath,
    IO,
    Int,
    String,
    foldl',
    forM_,
    fst,
    map,
    pure,
    reverse,
    runStateT,
    show,
    zip,
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
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Megaparsec qualified as MP

spec :: Spec
spec = weaveSpec

getWovenPrettyFromPath :: FilePath -> FilePath -> IO (Either String T.Text)
getWovenPrettyFromPath path orgIdLocationsFile = do
  goids <- LP.runGoidsParser orgIdLocationsFile
  inputOrgFile <- LU.readFrom "test/Lilac/WeaveSpec/" path
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser LP.defaultOrgParserState {LP.goids = goids})
          path
          inputOrgFile
  case parseResult of
    Left errBundle -> pure . Left $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) ->
      pure
        . Right
        . T.pack
        . renderHtml
        $ LW.weave orgDoc True goids

compareWithoutIndents :: T.Text -> [T.Text] -> [(String, Expectation)]
compareWithoutIndents haystackRaw needles =
  map
    ( \(num, exp) ->
        ("sees needle " <> show num, exp)
    )
    . zip [(1 :: Int) ..]
    . reverse
    . fst
    $ foldl' f ([], haystackWithoutIndents) needlesWithoutIndents
  where
    removeIndents = T.unlines . map T.strip . T.lines
    haystackWithoutIndents = removeIndents haystackRaw
    needlesWithoutIndents = map removeIndents needles
    f ::
      ([Expectation], T.Text) ->
      T.Text ->
      ([Expectation], T.Text)
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

weaveExpect :: FilePath -> FilePath -> [T.Text] -> Spec
weaveExpect path goidsFile needles = do
  result <- runIO $ getWovenPrettyFromPath path goidsFile
  describe path $ do
    case result of
      Right haystack -> do
        let testCases = compareWithoutIndents haystack needles
        forM_ testCases $ \(name, expectation) -> it name expectation
      Left errMsg -> it "programmer error" $ expectationFailure errMsg

weaveSpec :: Spec
weaveSpec = do
  weaveExpect
    "0300-weave-styled-text"
    "test/.orgids"
    [ "<p>",
      "regular",
      """
      <span class="bold">
        bold
      </span>
      """,
      """
      <span class="italic">
        italic
      </span>
      """,
      """
      <span class="underline">
        underline
      </span>
      """,
      """
      <span class="verbatim">
        verbatim
      </span>
      """,
      """
      <span class="code">
        code
      </span>
      """,
      """
      <span class="verbatim">
        &lt;&gt;
      </span>
      """,
      """
      <span class="code">
        &lt;&gt;
      </span>
      """,
      "</p>"
    ]

  weaveExpect
    "0300-weave-lists"
    "test/.orgids"
    [ "<ol>",
      "  <li>",
      "    <p>",
      "      a",
      "    </p>",
      "    <ul>",
      "      <li>",
      "        <p>",
      "          b",
      "        </p>",
      "      </li>",
      "    </ul>",
      "  </li>",
      "  <li>",
      "    <p>",
      "      c",
      "    </p>",
      "  </li>",
      "</ol>"
    ]
