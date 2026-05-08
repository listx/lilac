module Lilac.CompileSpec (spec) where

import Data.Graph qualified as G
import Data.Text qualified as T
import Data.Tree
  ( Tree (Node, rootLabel, subForest),
  )
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Types
  ( GTOC (GTOC),
    LinkOid (LinkOid, name, oid),
    Object,
    decodeGPath,
    emptyProjectConf,
    encodeGPath,
    fromWords,
  )
import Lilac.Types.Internal (GPath (GPath))
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    FilePath,
    Int,
    Maybe (Just, Nothing),
    MonadFail,
    String,
    fail,
    isRight,
    map,
    pure,
    show,
    traverse,
    ($),
    (.),
    (<>),
  )
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    runIO,
    shouldBe,
    shouldSatisfy,
  )

spec :: Spec
spec = do
  checkExternalLinkOids
    "0400-no-external-symbols-code-blocks"
    []

  checkExternalLinkOids
    "0400-no-external-symbols-complex"
    []

  checkExternalLinkOids
    "0400-external-symbols"
    [ LinkOid
        { oid = fromWords 0xffffffff00000000 0x000000000000d001,
          name = "a"
        }
    ]

  checkExternalLinkOids
    "0400-external-symbols-newlines"
    [ LinkOid
        { oid = fromWords 0xffffffff00000000 0x000000000000d001,
          name = "Hello World"
        },
      LinkOid
        { oid = fromWords 0xffffffff00000000 0x000000000000d001,
          name = "a b c d e"
        }
    ]

  expectArchiveError
    ["0401-broken-links-noweb"]
    """
    Found broken links:
      id:00000000-0000-0000-0000-00000000d003::x (test/Lilac/CompileSpec/0401-broken-links-noweb)
      id:00000000-0000-0000-0000-00000000d000::c (test/Lilac/CompileSpec/0401-broken-links-noweb)
    """

  expectArchiveError
    ["0401-broken-links-paragraph"]
    """
    Found broken links:
      id:00000000-0000-0000-0000-00000000d004::foo (test/Lilac/CompileSpec/0401-broken-links-paragraph)
    """

  expectArchiveOk
    ["0401-broken-links-paragraph-ok"]

  expectArchiveError
    ["0401-broken-links-line-label"]
    """
    Found broken links:
      id:00000000-0000-0000-0000-00000000d004::(foo) (test/Lilac/CompileSpec/0401-broken-links-line-label)
    """

  expectArchiveOk
    [ "0401-broken-links-multi-ok",
      "0401-broken-links-multi-ok-dependency"
    ]

  expectArchiveOk
    [ "0401-broken-links-multi-ok-alternates-parent",
      "0401-broken-links-multi-ok-alternates-child"
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
    ["0100-dag-self-loop-heading"]
    [ Node
        { rootLabel =
            LinkOid
              { oid = fromWords 0x0000000000000000 0x000000000000e001,
                name = "f:a"
              },
          subForest = []
        },
      Node
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
    ["0100-dag-2-node-loop-heading"]
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
                              { oid =
                                  fromWords
                                    0x0000000000000000
                                    0x000000000000e005,
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
                              { oid =
                                  fromWords
                                    0x0000000000000000
                                    0x000000000000e006,
                                name = "f:c"
                              },
                          subForest = []
                        },
                      Node
                        { rootLabel =
                            LinkOid
                              { oid =
                                  fromWords
                                    0x0000000000000000
                                    0x000000000000e006,
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
    [ "0100-dag-2-node-loop-2-files-parent-heading",
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
                              { oid =
                                  fromWords
                                    0x0000000000000000
                                    0x000000000000f008,
                                name = "c"
                              },
                          subForest = []
                        }
                    ]
                }
            ]
        }
    ]

  expectGTOC
    ["0105/foo.org"]
    (Left "root index.lobj not found")

  expectGTOC
    [ "0105/foo.org",
      "0105/index.org"
    ]
    (Right . GTOC $ Node (GPath "test/Lilac/CompileSpec/0105/index.lobj") [])

  expectGTOC
    [ "0105/basic/foo.org",
      "0105/basic/bar.org",
      "0105/basic/index.org"
    ]
    ( Right
        . GTOC
        $ Node
          (GPath "test/Lilac/CompileSpec/0105/basic/index.lobj")
          [ Node (GPath "test/Lilac/CompileSpec/0105/basic/foo.lobj") [],
            Node (GPath "test/Lilac/CompileSpec/0105/basic/bar.lobj") []
          ]
    )

  expectGTOC
    [ "0105/invalid-paths/index.org"
    ]
    ( Left
        """
        Invalid GTOC object paths:
          test/Lilac/CompileSpec/0105/invalid-paths/README.lobj
          test/Lilac/CompileSpec/0105/invalid-paths/dir1/index.lobj

        """
    )

  describe "cumulativeOffsets sanity check" $ do
    it "works for empty case" $ do
      let input = [] :: [(Int, String)]
          output = [] :: [(Int, String)]
      LC.cumulativeOffsets input `shouldBe` output
    it "works for simple case" $ do
      let input = [(3, "a"), (9, "b"), (1, "c"), (2, "d")] :: [(Int, String)]
          output = [(0, "a"), (3, "b"), (12, "c"), (13, "d")] :: [(Int, String)]
      LC.cumulativeOffsets input `shouldBe` output

checkExternalLinkOids :: FilePath -> [LinkOid] -> Spec
checkExternalLinkOids path expectedExternalOids = do
  it path $ do
    (Right od) <- read
    LC.findExternalOids od `shouldBe` expectedExternalOids
  where
    read = do
      input <- LU.readFrom "test/Lilac/CompileSpec/" path
      pure $ LP.parseOrgDoc input

expectArchiveError :: [FilePath] -> String -> Spec
expectArchiveError paths expected = do
  it (show paths) $ do
    pathObjs <- LC.readObjectsFrom "test/Lilac/CompileSpec/" paths
    maybeArchive <- LC.mkArchive emptyProjectConf pathObjs
    maybeArchive `shouldBe` (Left expected)

expectArchiveOk :: [FilePath] -> Spec
expectArchiveOk paths = do
  it (show paths) $ do
    pathObjs <- LC.readObjectsFrom "test/Lilac/CompileSpec/" paths
    maybeArchive <- LC.mkArchive emptyProjectConf pathObjs
    maybeArchive `shouldSatisfy` isRight

checkCycles :: [FilePath] -> [G.Tree LinkOid] -> Spec
checkCycles paths expectedCycles = do
  pathObjs <- runIO $ LC.readObjectsFrom "test/Lilac/CompileSpec/" paths
  maybeArchive <- runIO $ LC.mkArchive emptyProjectConf pathObjs
  case maybeArchive of
    Left errMsg -> it "programmer error" $ expectationFailure errMsg
    Right archive -> it (show paths) $ do
      (LC.findCycles archive) `shouldBe` expectedCycles

expectGTOC :: [FilePath] -> Either String GTOC -> Spec
expectGTOC paths expected = do
  it (show paths) $ do
    pathObjs <- LC.readObjectsFrom "test/Lilac/CompileSpec/" paths
    pathObjs' <- orgToLobj pathObjs
    LC.mkGTOC pathObjs' `shouldBe` expected
  where
    orgToLobj ::
      (MonadFail m) =>
      [(GPath, Object)] ->
      m [(GPath, Object)]
    orgToLobj pairs = do
      traverse reencode transformed
      where
        transformed :: [(T.Text, Object)]
        transformed = map transformPath pairs

        transformPath :: (GPath, Object) -> (T.Text, Object)
        transformPath (gp, obj) =
          let path = T.pack $ decodeGPath gp
              newPath =
                if ".org" `T.isSuffixOf` path
                  then (T.dropEnd 4 path) <> ".lobj"
                  else path
           in (newPath, obj)

        reencode :: (MonadFail m) => (T.Text, Object) -> m (GPath, Object)
        reencode (t, obj) =
          case encodeGPath (T.unpack t) of
            Just newGp -> pure (newGp, obj)
            Nothing ->
              fail
                $ "Failed to re-encode path: "
                <> T.unpack t
