module Lilac.Util
  ( readFrom,
    readFileRobustly,
    writeFileRobustly,
    writeOrWarn,
  )
where

import Control.Exception (catch)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import Relude
  ( Bool (True),
    FilePath,
    IO,
    Text,
    otherwise,
    pure,
    putStrLn,
    show,
    stderr,
    ($),
    (<$>),
    (<>),
    (==),
  )
import System.Directory.OsPath qualified as D
import System.File.OsPath qualified as P.IO
import System.IO.Error (IOError)
import System.OsPath (OsPath)
import System.OsPath qualified as P

readFrom :: FilePath -> FilePath -> IO Text
readFrom prefix path = do
  let fullPath = P.unsafeEncodeUtf $ prefix <> path
  TE.decodeUtf8 <$> P.IO.readFile' fullPath

readFileRobustly :: OsPath -> IO Text
readFileRobustly path
  | P.unpack path == [] = pure ""
  | otherwise = do
      catch
        (TE.decodeUtf8 <$> P.IO.readFile' path)
        readHandler
  where
    readHandler :: IOError -> IO Text
    readHandler _ = pure ""

writeFileRobustly :: OsPath -> Text -> IO ()
writeFileRobustly path contentToWrite = do
  D.createDirectoryIfMissing True $ P.takeDirectory path
  catch (P.IO.writeFile' path $ TE.encodeUtf8 contentToWrite) writeHandler
  where
    writeHandler :: IOError -> IO ()
    writeHandler e = putStrLn $ "writeFileRobustly: " <> show e

writeOrWarn :: OsPath -> Text -> IO ()
writeOrWarn path contentToWrite = do
  fileExists <- D.doesFileExist path
  if fileExists
    then T.hPutStrLn stderr $ "path " <> T.show path <> " already exists, skipping"
    else writeFileRobustly path contentToWrite
