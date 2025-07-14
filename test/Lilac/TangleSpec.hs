module Lilac.TangleSpec (spec) where

import Data.Text qualified as T
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Lilac.Types
  ( GPath,
    Object,
    encodeGPath,
  )
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    FilePath,
    IO,
    Text,
    first,
    fmap,
    foldlM,
    pure,
    show,
    ($),
    (=<<),
  )
import System.OsPath qualified as P
import Test.Hspec (Spec, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = tangleSpec

checkExpansion :: [FilePath] -> [(FilePath, Text)] -> Spec
checkExpansion paths expectedExpansions = do
  pathObjs <- runIO $ readObjectsFrom paths
  case LC.mkArchive pathObjs of
    Left errMsg -> it "programmer error" $ expectationFailure errMsg
    Right archive -> it (show paths) $ do
      (LT.expandRootBlocks archive)
        `shouldBe` (fmap (first P.unsafeEncodeUtf) expectedExpansions)

readObjectsFrom :: [FilePath] -> IO [(GPath, Object)]
readObjectsFrom = foldlM f []
  where
    f :: [(GPath, Object)] -> FilePath -> IO [(GPath, Object)]
    f acc path = do
      compileSpecDir <- P.encodeUtf "test/Lilac/TangleSpec/"
      p <- P.encodeUtf path
      let relPath = P.combine compileSpecDir p
      input <- LU.readFileRobustly relPath
      (Right od) <- pure $ LP.parseOrgDoc input
      relPath' <- encodeGPath =<< P.decodeUtf relPath
      pure $ (relPath', LC.mkObject od) : acc

tangleSpec :: Spec
tangleSpec = do
  checkExpansion
    ["0101-block-expansion"]
    [ ( "test/Lilac/TangleSpec/equations.txt",
        T.unlines
          [ "a1",
            "b",
            "a2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-deeply-nested"]
    [ ( "test/Lilac/TangleSpec/equations.txt",
        T.unlines
          [ "a1",
            "e",
            "a2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-prefix"]
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/equations.txt",
        T.unlines
          [ "hello from a",
            "hello from b",
            "hello from c"
          ]
      )
    ]

  it "loses information" $ do
    let withoutNewline = T.lines "a\nb"
        withNewline = T.lines "a\nb\n"
    T.unlines withoutNewline `shouldBe` T.unlines withNewline

  checkExpansion
    ["0102-pad-between-consecutive-links"]
    [ ( "test/Lilac/TangleSpec/equations.txt",
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
    [ ( "test/Lilac/TangleSpec/a.sh",
        T.unlines
          [ "#!/usr/bin/env bash",
            "a1"
          ]
      )
    ]
