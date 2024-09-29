module Lilac.ParseSpec (spec) where

import Lilac.Parse (defaultOrgDoc, parseOrgDoc)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses headlines" $ do
      let r = parseOrgDoc "\t  foo bar\n"
      r `shouldBe` Right defaultOrgDoc
