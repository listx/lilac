module Lilac.Version
  ( lilacVersion,
  )
where

import Control.Monad.Trans.Cont
  ( ContT,
    callCC,
    evalContT,
  )
import Data.Time.LocalTime (getZonedTime)
import Language.Haskell.TH (Exp, Q, runIO, stringE)
import Relude
  ( IO,
    MonadTrans,
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

getTimestamp :: IO String
getTimestamp = show <$> getZonedTime

getGitVersion :: IO String
getGitVersion = evalContT $ callCC $ \escapeWith -> do
  tryLilacGitVersion escapeWith
  tryLilacProjectRoot escapeWith
  versionFallback

tryLilacGitVersion :: (MonadTrans t) => (String -> t IO ()) -> t IO ()
tryLilacGitVersion escapeWith = do
  gitVersion <- lift $ getEnv "LILAC_GIT_VERSION"
  when (not $ null gitVersion) $ escapeWith gitVersion

tryLilacProjectRoot :: (MonadTrans t) => (String -> t IO ()) -> t IO ()
tryLilacProjectRoot escapeWith = do
  projectRoot <- lift $ getEnv "LILAC_PROJECT_ROOT"
  when (not $ null projectRoot) $ do
    gitOutput <-
      lift
        $ readProcess
          "git"
          ["-C", projectRoot, "describe", "--abbrev=10", "--always", "--dirty"]
          ""
    escapeWith gitOutput

versionFallback :: ContT String IO String
versionFallback = pure "unknown-build-env"

getEnv :: String -> IO String
getEnv env = fromMaybe "" <$> lookupEnv env
