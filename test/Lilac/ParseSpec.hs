module Lilac.ParseSpec (spec) where

import Lilac.Parse
  ( OrgDoc,
    defaultOrgDoc,
    defaultOrgDocWithMeta,
    orgDocParser,
  )
import Relude
import Test.Hspec (Spec, before, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec qualified as MP

p :: String -> FilePath -> OrgDoc -> Spec
p behavior path expected = do
  before (readFileBS path) $ do
    it behavior $ \input -> do
      MP.parse orgDocParser path (decodeUtf8 input) `shouldParse` expected

spec :: Spec
spec =
  describe "parse" $ do
    it "parses empty input as defaultOrgDoc" $ do
      MP.parse orgDocParser "empty input" "" `shouldParse` defaultOrgDoc
    p
      "parses blank line as defaultOrgDoc"
      "test/Lilac/ParseSpec/0001-blankline"
      defaultOrgDoc
    p "parses document metadata" "test/Lilac/ParseSpec/0002-doc-metadata"
      $ defaultOrgDocWithMeta
        [ ("author", "A. U. Thor"),
          ("auto_tangle", "t"),
          ("property", "header-args+ :noweb no-export"),
          ("title", "An Org Document")
        ]
    p
      "parses document metadata (skips comments)"
      "test/Lilac/ParseSpec/0003-doc-metadata-with-comments"
      $ defaultOrgDocWithMeta
        [ ("author", "A. U. Thor"),
          ("auto_tangle", "t"),
          ("property", "header-args+ :noweb no-export"),
          ("title", "An Org Document")
        ]
