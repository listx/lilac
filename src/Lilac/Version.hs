module Lilac.Version
  ( lilacVersion,
  )
where

import Control.Monad.Trans.Cont
  ( callCC,
    evalContT,
  )
import Data.Time.LocalTime (getZonedTime)
import Language.Haskell.TH (Exp, Q, runIO, stringE)
import Relude
  ( IO,
    String,
    concat,
    filter,
    fromMaybe,
    lift,
    not,
    null,
    pure,
    show,
    when,
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

getEnv :: String -> IO String
getEnv env = fromMaybe "" <$> lookupEnv env

getGitVersion :: IO String
getGitVersion = evalContT $ callCC $ \earlyReturn -> do
  -- LILAC_GIT_VERSION is used in nix-build. In a nix-build, we cannot use
  -- LILAC_PROJECT_ROOT because the nix-build process (or its "git" subprocess)
  -- won't have permission to enter the LILAC_PROJECT_ROOT.
  gitVersion <- lift $ getEnv "LILAC_GIT_VERSION"
  when (not $ null gitVersion) $ earlyReturn gitVersion

  -- LILAC_PROJECT_ROOT is used by cabal.
  projectRoot <- lift $ getEnv "LILAC_PROJECT_ROOT"
  when (not $ null projectRoot) $ do
    gitOutput <-
      lift
        $ readProcess
          "git"
          ["-C", projectRoot, "describe", "--abbrev=10", "--always", "--dirty"]
          ""
    earlyReturn gitOutput

  -- Give up if no LILAC_* env var was found.
  pure "unknown"

getTimestamp :: IO String
getTimestamp = show <$> getZonedTime
