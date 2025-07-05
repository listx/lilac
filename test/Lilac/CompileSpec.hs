module Lilac.CompileSpec (spec) where

import Data.Text qualified as T
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Types
  ( LinkOid (LinkOid, name, oid),
    fromWords,
  )
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    FilePath,
    IO,
    foldlM,
    isRight,
    pure,
    show,
    ($),
  )
import Test.Hspec (Spec, it, shouldBe, shouldSatisfy)

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

expectArchiveError :: [FilePath] -> T.Text -> Spec
expectArchiveError paths expected = do
  it (show paths) $ do
    pathObjs <- readObjectsFrom paths
    LC.mkArchive pathObjs `shouldBe` (Left expected)

expectArchiveOk :: [FilePath] -> Spec
expectArchiveOk paths = do
  it (show paths) $ do
    pathObjs <- readObjectsFrom paths
    LC.mkArchive pathObjs `shouldSatisfy` isRight

readObjectsFrom :: [FilePath] -> IO [(FilePath, LC.Object)]
readObjectsFrom = foldlM f []
  where
    f :: [(FilePath, LC.Object)] -> FilePath -> IO [(FilePath, LC.Object)]
    f acc path = do
      (Right od) <- read path
      pure $ (path, LC.mkObject od) : acc
    read path = do
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

  expectArchiveError
    ["0401-broken-links-noweb"]
    """
    broken links:
      id:00000000-0000-0000-0000-00000000d003::x
      id:00000000-0000-0000-0000-00000000d000::c
    """

  expectArchiveError
    ["0401-broken-links-paragraph"]
    """
    broken links:
      id:00000000-0000-0000-0000-00000000d004::foo
    """

  expectArchiveOk
    ["0401-broken-links-paragraph-ok"]

  expectArchiveOk
    [ "0401-broken-links-multi-ok",
      "0401-broken-links-multi-ok-dependency"
    ]
