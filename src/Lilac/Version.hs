module Lilac.Version
  ( lilacVersion,
  )
where

import Data.Time.LocalTime (getZonedTime)
import Language.Haskell.TH (Exp, Q, runIO, stringE)
import Relude
  ( IO,
    Maybe (Just, Nothing),
    String,
    concat,
    filter,
    pure,
    show,
    ($),
    (/=),
    (<$>),
    (<>),
    (=<<),
  )
import System.Environment (lookupEnv)
import System.Process (readProcess)

lilacVersion :: Q Exp
lilacVersion = stringE =<< runIO getCombinedVersion

getCombinedVersion :: IO String
getCombinedVersion = do
  gitVersion <- filter (/= '\n') <$> getGitVersion
  timestamp <- getTimestamp
  pure $ concat [gitVersion, " (", timestamp, ")"]

getGitVersion :: IO String
getGitVersion = do
  maybeProjectRoot <- lookupEnv "LILAC_PROJECT_ROOT"
  let changeDir = case maybeProjectRoot of
        Just projectRoot -> ["-C", projectRoot]
        Nothing -> []
      cmd = changeDir <> ["describe", "--abbrev=10", "--always", "--dirty"]
  readProcess "git" cmd ""

getTimestamp :: IO String
getTimestamp = show <$> getZonedTime
