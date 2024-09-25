module Lilac.ParseSpec (spec) where

import Lilac.Parse (parseOrgDoc)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses headlines" $ do
      r <- parseOrgDoc "\t  foo bar\n"
      r `shouldBe` ()
