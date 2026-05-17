module Lilac.Util
  ( readFrom,
    readFileRobustly,
    writeFileRobustly,
  )
where

import Control.Exception (bracketOnError, catch)
import Data.ByteString qualified as BS
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
import System.IO (hClose, openBinaryTempFile)
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
writeFileRobustly osPath contentToWrite = do
  let dir = P.takeDirectory osPath
  D.createDirectoryIfMissing True dir
  catch (writeAtomically osPath contentToWrite) writeHandler
  where
    writeHandler :: IOError -> IO ()
    writeHandler e = putStrLn $ "writeFileRobustly: " <> show e

writeAtomically :: OsPath -> Text -> IO ()
writeAtomically targetPath contentToWrite = do
  let errorPath (tmpPath, handle) = do
        tmpOsPath <- P.encodeUtf tmpPath
        D.removeFile tmpOsPath
        hClose handle
      happyPath (tmpPath, handle) = do
        BS.hPut handle $ TE.encodeUtf8 contentToWrite
        hClose handle
        tmpOsPath <- P.encodeUtf tmpPath
        D.renameFile tmpOsPath targetPath
      dir = P.takeDirectory targetPath
  dirPath <- P.decodeFS dir
  bracketOnError --
    (openBinaryTempFile dirPath "lilac-tmp-.html")
    errorPath
    happyPath
