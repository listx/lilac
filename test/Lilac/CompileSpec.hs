module Lilac.CompileSpec (spec) where

import Data.Graph qualified as G
import Data.Text qualified as T
import Data.Tree
  ( Tree (Node, rootLabel, subForest),
  )
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Types
  ( GPath,
    LinkOid (LinkOid, name, oid),
    encodeGPath,
    fromWords,
  )
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    FilePath,
    IO,
    first,
    fmap,
    foldlM,
    isRight,
    pure,
    show,
    ($),
    (=<<),
  )
import System.OsPath qualified as P
import Test.Hspec
  ( Spec,
    expectationFailure,
    it,
    runIO,
    shouldBe,
    shouldSatisfy,
  )

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

readObjectsFrom :: [FilePath] -> IO [(GPath, LC.Object)]
readObjectsFrom = foldlM f []
  where
    f :: [(GPath, LC.Object)] -> FilePath -> IO [(GPath, LC.Object)]
    f acc path = do
      compileSpecDir <- P.encodeUtf "test/Lilac/CompileSpec/"
      p <- P.encodeUtf path
      let relPath = P.combine compileSpecDir p
      input <- LU.readFileRobustly relPath
      (Right od) <- pure $ LP.parseOrgDoc input
      relPath' <- encodeGPath =<< P.decodeUtf relPath
      pure $ (relPath', LC.mkObject od) : acc

checkCycles :: [FilePath] -> [G.Tree LinkOid] -> Spec
checkCycles paths expectedCycles = do
  pathObjs <- runIO $ readObjectsFrom paths
  case LC.mkArchive pathObjs of
    Left errMsg -> it "programmer error" $ expectationFailure $ T.unpack errMsg
    Right archive -> it (show paths) $ do
      (LC.findCycles archive) `shouldBe` expectedCycles

checkExpansion :: [FilePath] -> [(FilePath, T.Text)] -> Spec
checkExpansion paths expectedExpansions = do
  pathObjs <- runIO $ readObjectsFrom paths
  case LC.mkArchive pathObjs of
    Left errMsg -> it "programmer error" $ expectationFailure $ T.unpack errMsg
    Right archive -> it (show paths) $ do
      (LC.expandRootBlocks archive)
        `shouldBe` (fmap (first P.unsafeEncodeUtf) expectedExpansions)

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

  checkCycles
    ["0100-dag-no-loops"]
    []

  checkCycles
    ["0100-dag-self-loop"]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e001,
                name = "f:a"
              },
          subForest = []
        }
    ]

  checkCycles
    ["0100-dag-self-loops"]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e003,
                name = "f:a"
              },
          subForest = []
        },
      Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e003,
                name = "f:b"
              },
          subForest = []
        }
    ]

  checkCycles
    ["0100-dag-2-node-loop"]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e004,
                name = "f:a"
              },
          subForest =
            [ Node
                { rootLabel =
                    LinkOid
                      { oid = fromWords 0x0000000000000000 0x000000000000e004,
                        name = "f:b"
                      },
                  subForest = []
                }
            ]
        }
    ]

  checkCycles
    ["0100-dag-3-node-loop"]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e005,
                name = "f:a"
              },
          subForest =
            [ Node
                { rootLabel =
                    LinkOid
                      { oid = fromWords 0x0000000000000000 0x000000000000e005,
                        name = "f:b"
                      },
                  subForest =
                    [ Node
                        { rootLabel =
                            LinkOid
                              { oid = fromWords 0x0000000000000000 0x000000000000e005,
                                name = "f:c"
                              },
                          subForest = []
                        }
                    ]
                }
            ]
        }
    ]

  checkCycles
    ["0100-dag-complex-loops"]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e006,
                name = "f:a"
              },
          subForest =
            [ Node
                { rootLabel =
                    LinkOid
                      { oid = fromWords 0x0000000000000000 0x000000000000e006,
                        name = "f:b"
                      },
                  subForest =
                    [ Node
                        { rootLabel =
                            LinkOid
                              { oid = fromWords 0x0000000000000000 0x000000000000e006,
                                name = "f:c"
                              },
                          subForest = []
                        },
                      Node
                        { rootLabel =
                            LinkOid
                              { oid = fromWords 0x0000000000000000 0x000000000000e006,
                                name = "f:d"
                              },
                          subForest = []
                        }
                    ]
                }
            ]
        },
      Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e006,
                name = "f:e"
              },
          subForest = []
        }
    ]

  checkCycles
    [ "0100-dag-2-node-loop-2-files-parent",
      "0100-dag-2-node-loop-2-files-child"
    ]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000f004,
                name = "a"
              },
          subForest =
            [ Node
                { rootLabel =
                    LinkOid
                      { oid = fromWords 0x0000000000000000 0x000000000000f005,
                        name = "b"
                      },
                  subForest = []
                }
            ]
        }
    ]

  checkCycles
    [ "0100-dag-2-node-loop-3-files-parent",
      "0100-dag-2-node-loop-3-files-middleman",
      "0100-dag-2-node-loop-3-files-child"
    ]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000f006,
                name = "a"
              },
          subForest =
            [ Node
                { rootLabel =
                    LinkOid
                      { oid = fromWords 0x0000000000000000 0x000000000000f007,
                        name = "b"
                      },
                  subForest =
                    [ Node
                        { rootLabel =
                            LinkOid
                              { oid = fromWords 0x0000000000000000 0x000000000000f008,
                                name = "c"
                              },
                          subForest = []
                        }
                    ]
                }
            ]
        }
    ]

  checkExpansion
    ["0101-block-expansion"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "b",
            "a2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-deeply-nested"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "e",
            "a2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-prefix"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "  b1",
            "  b2",
            "  b3",
            "a1",
            "  b1",
            "  b2",
            "  b3"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-prefix-nested"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "  b1",
            "  b2",
            "  b3",
            "    c1",
            "    c2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-prefix-nonspace"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "  -> b1",
            "  -> b2",
            "  -> b3"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-prefix-nonspace-nested"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "  -> b1",
            "  -> b2",
            "  -> b3",
            "  -> -- c1",
            "  -> -- c2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-complex"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
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
            "",
            "h"
          ]
      )
    ]

  checkExpansion
    [ "0101-block-expansion-external-parent",
      "0101-block-expansion-external-child"
    ]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "hello from child",
            "a2"
          ]
      )
    ]

  checkExpansion
    [ "0101-block-expansion-external-parent-headingchild",
      "0101-block-expansion-external-headingchild"
    ]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "hello from child",
            "a2"
          ]
      )
    ]

  checkExpansion
    [ "0101-block-expansion-external-2-hops-parent",
      "0101-block-expansion-external-2-hops-middleman",
      "0101-block-expansion-external-child"
    ]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "hello from middleman",
            "hello from child",
            "a2"
          ]
      )
    ]

  checkExpansion
    [ "0101-block-expansion-external-mutual-a",
      "0101-block-expansion-external-mutual-b"
    ]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "hello from a",
            "hello from b",
            "hello from c"
          ]
      )
    ]

  checkExpansion
    ["0102-pad-between-consecutive-links"]
    [ ( "test/Lilac/CompileSpec/equations.txt",
        T.unlines
          [ "a1",
            "b1",
            "",
            "b2",
            "a2",
            "  b1",
            "",
            "  b2",
            "",
            "  b3",
            "Even if indentation is different, still pad between them.",
            "  b1",
            "",
            "    b2",
            "",
            "      b3",
            "Even pad for the same block repeated multiple times.",
            "  b1",
            "",
            "  b1",
            "",
            "  b1",
            "Lines with multiple links still count as being consecutive.",
            "  b3",
            "",
            "  b3",
            "",
            "  b1 b2",
            "",
            "  b3",
            "",
            "  b3",
            "No padding if only one link...",
            "  b1",
            "...is by itself."
          ]
      )
    ]

  checkExpansion
    ["0103-shebang"]
    [ ( "test/Lilac/CompileSpec/a.sh",
        T.unlines
          [ "#!/usr/bin/env bash",
            "a1"
          ]
      )
    ]

  it "loses information" $ do
    let withoutNewline = T.lines "a\nb"
        withNewline = T.lines "a\nb\n"
    T.unlines withoutNewline `shouldBe` T.unlines withNewline
