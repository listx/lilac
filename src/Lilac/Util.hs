module Lilac.Util
  ( readFrom,
  )
where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Relude
  ( FilePath,
    IO,
    ($),
    (<$>),
    (<>),
  )
import System.File.OsPath qualified as P.IO
import System.OsPath qualified as P

readFrom :: FilePath -> FilePath -> IO T.Text
readFrom prefix path = do
  let fullPath = P.unsafeEncodeUtf $ prefix <> path
  TE.decodeUtf8 <$> P.IO.readFile' fullPath
