module Lilac.TangleSpec (spec) where

import Data.Graph qualified as G
import Data.Text qualified as T
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    FilePath,
    IO,
    pure,
    ($),
    (.),
  )
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = tangleSpec

getSourceCodeFromPath :: FilePath -> IO (Either T.Text LT.SourceCode)
getSourceCodeFromPath path = do
  input <- LU.readFrom "test/Lilac/TangleSpec/" path
  case LP.parseOrgDoc input of
    Left errMsg -> pure $ Left errMsg
    Right od -> pure . Right $ LT.getSourceCode od

checkCycles :: FilePath -> [G.Tree LT.BlockName] -> Spec
checkCycles path expectedCycles = do
  it path $ do
    (Right sc) <- getSourceCodeFromPath path
    (LT.findCycles sc) `shouldBe` expectedCycles

checkExpansion :: FilePath -> [(T.Text, T.Text)] -> Spec
checkExpansion path expectedExpansions = do
  it path $ do
    (Right sc) <- getSourceCodeFromPath path
    (LT.expandRootBlocks sc) `shouldBe` expectedExpansions

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
  checkExpansion
    "0101-block-expansion"
    [ ( "equations.txt",
        T.unlines
          [ "a1",
            "b",
            "a2"
          ]
      )
    ]
  checkExpansion
    "0101-block-expansion-deeply-nested"
    [ ( "equations.txt",
        T.unlines
          [ "a1",
            "e",
            "a2"
          ]
      )
    ]
  checkExpansion
    "0101-block-expansion-complex"
    [ ( "equations.txt",
        T.unlines
          [ "a1",
            "b",
            "a2",
            "  c",
            "  c",
            "    d",
            "",
            "    d",
            "    e hello!",
            "  c",
            "a3",
            "g",
            "h"
          ]
      )
    ]
  it "loses information" $ do
    let withoutNewline = T.lines "a\nb"
        withNewline = T.lines "a\nb\n"
    T.unlines withoutNewline `shouldBe` T.unlines withNewline
