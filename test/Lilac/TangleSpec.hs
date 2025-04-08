module Lilac.TangleSpec (spec) where

import Data.Graph qualified as G
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Relude
  ( Either (Left, Right),
    FilePath,
    MonadIO,
    decodeUtf8,
    pure,
    readFileBS,
    ($),
    (.),
    (<>),
  )
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = tangleSpec

readFrom :: (MonadIO m) => FilePath -> FilePath -> m T.Text
readFrom prefix path = do
  byteString <- readFileBS $ prefix <> path
  pure $ decodeUtf8 byteString

getSourceCodeFromPath :: (MonadIO m) => FilePath -> m (Either T.Text LT.SourceCode)
getSourceCodeFromPath path = do
  input <- readFrom "test/Lilac/TangleSpec/" path
  case LP.parseOrgDoc input of
    Left errMsg -> pure $ Left errMsg
    Right od -> pure . Right $ LT.getSourceCode od

checkCycles :: FilePath -> [G.Tree LT.BlockName] -> Spec
checkCycles path expectedCycles = do
  it path $ do
    (Right sc) <- getSourceCodeFromPath path
    (LT.findCycles $ M.elems sc.nameToCodeBlock) `shouldBe` expectedCycles

tangleSpec :: Spec
tangleSpec = do
  checkCycles
    "0100-dag-no-loops"
    []
  checkCycles
    "0100-dag-self-loop"
    [G.Node "f:a" []]
  checkCycles
    "0100-dag-self-loops"
    [G.Node "f:a" [], G.Node "f:b" []]
  checkCycles
    "0100-dag-2-node-loop"
    [G.Node "f:a" [G.Node "f:b" []]]
  checkCycles
    "0100-dag-3-node-loop"
    [G.Node "f:a" [G.Node "f:b" [G.Node "f:c" []]]]
  checkCycles
    "0100-dag-complex-loops"
    [G.Node "f:a" [G.Node "f:b" [G.Node "f:c" [], G.Node "f:d" []]], G.Node "f:e" []]
