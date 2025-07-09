module Lilac.CompileSpec (spec) where

import Data.Graph qualified as G
import Data.Text qualified as T
import Data.Tree
  ( Tree (Node, rootLabel, subForest),
  )
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

checkCycles :: [FilePath] -> [G.Tree LinkOid] -> Spec
checkCycles paths expectedCycles = do
  pathObjs <- runIO $ readObjectsFrom paths
  case LC.mkArchive pathObjs of
    Left errMsg -> it "programmer error" $ expectationFailure $ T.unpack errMsg
    Right archive -> it (show paths) $ do
      (LC.findCycles archive) `shouldBe` expectedCycles

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
