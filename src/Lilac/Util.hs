module Lilac.Util
  ( prefixWithId,
    readFrom,
    readFileRobustly,
    writeFileRobustly,
  )
where

import Control.Exception (catch)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID.Types (UUID)
import Relude
  ( Bool (True),
    FilePath,
    IO,
    Maybe (Just, Nothing),
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
import System.OsPath qualified as P

readFrom :: FilePath -> FilePath -> IO T.Text
readFrom prefix path = do
  let fullPath = P.unsafeEncodeUtf $ prefix <> path
  TE.decodeUtf8 <$> P.IO.readFile' fullPath

readFileRobustly :: P.OsPath -> IO T.Text
readFileRobustly path
  | P.unpack path == [] = pure ""
  | otherwise = do
      catch
        (TE.decodeUtf8 <$> P.IO.readFile' path)
        readHandler
  where
    readHandler :: IOError -> IO T.Text
    readHandler _ = pure ""

writeFileRobustly :: P.OsPath -> T.Text -> IO ()
writeFileRobustly path contentToWrite = do
  D.createDirectoryIfMissing True $ P.takeDirectory path
  catch (P.IO.writeFile' path $ TE.encodeUtf8 contentToWrite) writeHandler
  where
    writeHandler :: IOError -> IO ()
    writeHandler e = putStrLn $ "writeFileRobustly: " <> show e

prefixWithId :: Maybe UUID -> T.Text -> T.Text
prefixWithId maybeUuid name = case maybeUuid of
  Just uuid -> "id:" <> T.pack (show uuid) <> "::" <> name
  Nothing -> name
