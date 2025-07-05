module Lilac.CompileSpec (spec) where

import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Types
  ( LinkOid (LinkOid, name, oid),
    fromWords,
  )
import Lilac.Util qualified as LU
import Relude
  ( Either (Right),
    FilePath,
    pure,
    ($),
  )
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = compileSpec

checkExternalOids :: FilePath -> [LinkOid] -> Spec
checkExternalOids path expectedExternalOids = do
  it path $ do
    (Right od) <- read
    LC.findExternalOids od `shouldBe` expectedExternalOids
  where
    read = do
      input <- LU.readFrom "test/Lilac/CompileSpec/" path
      pure $ LP.parseOrgDoc input

compileSpec :: Spec
compileSpec = do
  checkExternalOids
    "0400-no-external-symbols-code-blocks"
    []

  checkExternalOids
    "0400-no-external-symbols-complex"
    []

  checkExternalOids
    "0400-external-symbols"
    [ LinkOid
        { oid = fromWords 0xffffffff00000000 0x000000000000d001,
          name = "a"
        }
    ]
