module Lilac.Util
  ( readFrom,
  )
where

import Data.Text qualified as T
import Data.Text.IO.Utf8 (readFile)
import Relude
  ( FilePath,
    IO,
    ($),
    (<>),
  )

readFrom :: FilePath -> FilePath -> IO T.Text
readFrom prefix path = readFile $ prefix <> path
