module Lilac.TangleSpec (spec) where

import Data.Text qualified as T
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Lilac.Util qualified as LU
import Relude
  ( Either (Left, Right),
    FilePath,
    IO,
    first,
    fmap,
    pure,
    ($),
    (<>),
  )
import System.OsPath qualified as P
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = tangleSpec

getSourceCodeFromPath ::
  FilePath ->
  FilePath ->
  IO (Either T.Text LT.SourceCode)
getSourceCodeFromPath path orgIdLocationsFile = do
  goids <- LP.runGoidsParser orgIdLocationsFile
  input <- LU.readFrom "test/Lilac/TangleSpec/" path
  case LP.parseOrgDoc input of
    Left errMsg -> pure $ Left errMsg
    Right od -> do
      sc <- LT.getSourceCodeMulti od ("test/Lilac/TangleSpec/" <> path) goids
      pure $ Right sc

checkExpansion :: FilePath -> FilePath -> [(FilePath, T.Text)] -> Spec
checkExpansion path goidsFile expectedExpansions = do
  it path $ do
    (Right sc) <- getSourceCodeFromPath path goidsFile
    (LT.expandRootBlocks sc)
      `shouldBe` (fmap (first P.unsafeEncodeUtf) expectedExpansions)

tangleSpec :: Spec
tangleSpec = do
  checkExpansion
    "0101-block-expansion"
    "test/.orgids"
    [ ( "test/Lilac/TangleSpec/equations.txt",
        T.unlines
          [ "a1",
            "b",
            "a2"
          ]
      )
    ]

  checkExpansion
    "0101-block-expansion-deeply-nested"
    "test/.orgids"
    [ ( "test/Lilac/TangleSpec/equations.txt",
        T.unlines
          [ "a1",
            "e",
            "a2"
          ]
      )
    ]

  checkExpansion
    "0101-block-expansion-prefix"
    "test/.orgids"
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
    "0101-block-expansion-prefix-nested"
    "test/.orgids"
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
    "0101-block-expansion-prefix-nonspace"
    "test/.orgids"
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
    "0101-block-expansion-prefix-nonspace-nested"
    "test/.orgids"
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
    "0101-block-expansion-complex"
    "test/.orgids"
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
    "0101-block-expansion-external-parent"
    "test/.orgids"
    [ ( "test/Lilac/TangleSpec/equations.txt",
        T.unlines
          [ "a1",
            "hello from child",
            "a2"
          ]
      )
    ]

  checkExpansion
    "0101-block-expansion-external-parent-headingchild"
    "test/.orgids"
    [ ( "test/Lilac/TangleSpec/equations.txt",
        T.unlines
          [ "a1",
            "hello from child",
            "a2"
          ]
      )
    ]

  checkExpansion
    "0101-block-expansion-external-2-hops-parent"
    "test/.orgids"
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
    "0101-block-expansion-external-mutual-a"
    "test/.orgids"
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
    "0102-pad-between-consecutive-links"
    "test/.orgids"
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
    "0103-shebang"
    "test/.orgids"
    [ ( "test/Lilac/TangleSpec/a.sh",
        T.unlines
          [ "#!/usr/bin/env bash",
            "a1"
          ]
      )
    ]
