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
  -- LILAC_GIT_VERSION is used in nix-build. In a nix-build, we cannot use
  -- LILAC_PROJECT_ROOT because the nix-build process (or its "git" subprocess)
  -- won't have permission to enter the LILAC_PROJECT_ROOT.
  maybeGitVersion <- lookupEnv "LILAC_GIT_VERSION"
  case maybeGitVersion of
    Just version -> pure version
    Nothing -> do
      -- LILAC_PROJECT_ROOT is used by cabal.
      maybeProjectRoot <- lookupEnv "LILAC_PROJECT_ROOT"
      case maybeProjectRoot of
        Just projectRoot ->
          readProcess
            "git"
            ["-C", projectRoot, "describe", "--abbrev=10", "--always", "--dirty"]
            ""
        Nothing -> pure "unknown"

getTimestamp :: IO String
getTimestamp = show <$> getZonedTime
