module Lilac.TangleSpec (spec) where

import Data.Text qualified as T
import Lilac.Compile qualified as LC
import Lilac.Tangle qualified as LT
import Lilac.Types (emptyProjectConf)
import Relude
  ( Either (Left, Right),
    FilePath,
    Text,
    first,
    fmap,
    mempty,
    show,
    ($),
  )
import System.OsPath qualified as P
import Test.Hspec (Spec, expectationFailure, it, runIO, shouldBe)

spec :: Spec
spec = do
  checkExpansion
    ["0101-block-expansion"]
    [ ( "equations.txt",
        T.unlines
          [ "a1",
            "b",
            "a2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-deeply-nested"]
    [ ( "equations.txt",
        T.unlines
          [ "a1",
            "e",
            "a2"
          ]
      )
    ]

  checkExpansion
    ["0101-block-expansion-prefix"]
    [ ( "equations.txt",
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
    [ ( "equations.txt",
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
    [ ( "equations.txt",
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
    [ ( "equations.txt",
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
            "",
            "h"
          ]
      )
    ]

  checkExpansion
    [ "0101-block-expansion-external-parent",
      "0101-block-expansion-external-child"
    ]
    [ ( "equations.txt",
        T.unlines
          [ "a1",
            "hello from child",
            "a2"
          ]
      )
    ]

  checkExpansion
    [ "0101-block-expansion-external-headingchild-parent",
      "0101-block-expansion-external-headingchild"
    ]
    [ ( "equations.txt",
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
    [ ( "equations.txt",
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
    [ ( "equations.txt",
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
    [ ( "equations.txt",
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
    ["0102-pad-between-consecutive-links-except-embedded"]
    [ ( "equations.txt",
        T.unlines
          [ "a1 b1,",
            "a2 b2,",
            "a3 b3"
          ]
      )
    ]

  checkExpansion
    ["0103-shebang"]
    [ ( "a.sh",
        T.unlines
          [ "#!/usr/bin/env bash",
            "a1"
          ]
      )
    ]

checkExpansion :: [FilePath] -> [(FilePath, Text)] -> Spec
checkExpansion paths expectedExpansions = do
  pathObjs <- runIO $ LC.readObjectsFrom "test/Lilac/TangleSpec/" paths
  maybeArchive <- runIO $ LC.mkArchive emptyProjectConf pathObjs
  case maybeArchive of
    Left errMsg -> it "programmer error" $ expectationFailure errMsg
    Right archive -> it (show paths) $ do
      (LT.expandRootBlocks archive mempty)
        `shouldBe` (fmap (first P.unsafeEncodeUtf) expectedExpansions)
