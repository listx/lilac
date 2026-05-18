module Lilac.Util
  ( readFrom,
    readFileRobustly,
    writeFileRobustly,
  )
where

import Control.Exception (catch)
import Data.Text.Encoding qualified as TE
import Relude
  ( Bool (True),
    FilePath,
    IO,
    Text,
    otherwise,
    pure,
    putStrLn,
    show,
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
