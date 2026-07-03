module Lilac.Serve
  ( fsWatcher,
    liveReloadMiddleware,
  )
where

import Control.Concurrent (forkIO, threadDelay, withMVar)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
  ( TChan,
    TVar,
    dupTChan,
    isEmptyTChan,
    modifyTVar',
    readTChan,
    readTVar,
    writeTChan,
    writeTVar,
  )
import Control.Exception (catch, finally)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (findWithDefault)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Network.HTTP.Types (status200)
import Network.Wai (Middleware, pathInfo, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Relude
  ( Bool (False, True),
    FilePath,
    IO,
    Int,
    LByteString,
    MVar,
    Map,
    Maybe (Just),
    STM,
    Text,
    Type,
    atomically,
    not,
    null,
    otherwise,
    pure,
    putMVar,
    putTextLn,
    readMVar,
    show,
    void,
    when,
    ($),
    (+),
    (.),
    (<$>),
    (<>),
    (==),
  )
import Relude.Extra.Map (delete, insert, lookup)
import System.Directory (doesFileExist)
import System.FSNotify
  ( Event (Added, Modified),
    eventPath,
    watchTree,
    withManager,
  )
import System.FilePath
  ( dropTrailingPathSeparator,
    makeRelative,
    takeExtension,
    (</>),
  )
import System.IO (stderr)

type GenMap :: Type
type GenMap = Map FilePath Int

liveReloadMiddleware :: FilePath -> TChan FilePath -> MVar () -> Bool -> Middleware
liveReloadMiddleware root fsChan logLock verbose baseApp req respond =
  websocketsOr
    WS.defaultConnectionOptions
    (wsHandler fsChan logLock verbose)
    backupApp
    req
    respond
  where
    backupApp req' resp' = do
      let pieces = pathInfo req'
          relPath =
            if null pieces
              then "index.html"
              else T.unpack (T.intercalate "/" pieces)
          fullPath = dropTrailingPathSeparator root </> relPath
      isHtmlFile <-
        if takeExtension fullPath == ".html"
          then doesFileExist fullPath
          else pure False
      if isHtmlFile
        then do
          content <- LBS.readFile fullPath
          resp'
            $ responseLBS
              status200
              [("Content-Type", "text/html")]
              (injectScript content)
        else baseApp req' resp'
    injectScript body =
      let (before, after) = BS.breakSubstring "</body>" (LBS.toStrict body)
          script :: LByteString
          script =
            """
            <script>
            (function() {
                let reloading = false;
                const path = window.location.pathname;
                const wsUrl = (window.location.protocol === 'https:' ? 'wss://' : 'ws://')
                              + window.location.host
                              + '/lilac-ws';

                const scrollSelectors = ['nav.toc', 'main', 'aside.sidebar']; //
                const storageKey = 'dev-scroll-map';

                let lastScrollTime = 0;
                window.addEventListener('scroll', () => { //
                  lastScrollTime = Date.now();
                }, true);

                function setScrollPositions(map) {
                  for (let sel in map) {
                    let el = document.querySelector(sel);
                    if (el) el.scrollTop = map[sel];
                  }
                }

                function getScrollPositions(selectors) {
                  let map = {};
                  selectors.forEach(sel => {
                    let el = document.querySelector(sel);
                    if (el) map[sel] = el.scrollTop;
                  });
                  return map;
                }

                const rawData = localStorage.getItem(storageKey);
                if (rawData) {
                  const prevScrollPositions = JSON.parse(rawData);
                  setTimeout(() => {
                    setScrollPositions(prevScrollPositions); //
                    localStorage.removeItem(storageKey);
                  }, 200); //
                }

                //
                function reloadPage() {
                  const recentlyScrolled = (Date.now() - lastScrollTime) < 1500; //

                  if (recentlyScrolled) { //
                    setTimeout(reloadPage, 500); //
                    return;
                  }

                  const currentScrollPositions = getScrollPositions(scrollSelectors);
                  localStorage.setItem(storageKey,
                                       JSON.stringify(currentScrollPositions)); //

                  window.location.reload(); //
                }

                function connectWebSocket() {
                    const ws = new WebSocket(wsUrl); //

                    const trigger = () => {
                        if (!reloading) {
                            reloading = true;
                            console.log("Lilac: Initiating reload...");
                            reloadPage();
                        }
                    };

                    ws.onopen = () => {
                        console.log("Lilac: WebSocket connected. Monitoring path:", path);
                        ws.send(path); //

                        //
                        const heartbeat = setInterval(() => {
                            if (ws.readyState === WebSocket.OPEN) {
                                ws.send('LILAC_WS_HEARTBEAT_FROM_JS_CLIENT');
                            } else {
                                clearInterval(heartbeat);
                            }
                        }, 30000);
                    };

                    ws.onmessage = (msg) => {
                        if (msg.data === 'reload') {
                            console.log("Lilac: Reload signal received."); //
                            trigger();
                        }
                    };

                    ws.onclose = (e) => {
                        if (!reloading) {
                            console.log("Lilac: WebSocket closed (Code " + e.code + ").");
                            console.log("Lilac: Reconnecting in 2s...");
                            setTimeout(connectWebSocket, 2000); //
                        }
                    };

                    ws.onerror = (err) => {
                        console.error("Lilac: WebSocket error observed:", err);
                        ws.close(); //
                    };
                }

                connectWebSocket();
            })();
            </script>
            """
       in if BS.null after
            then body
            else LBS.fromStrict before <> script <> LBS.fromStrict after

fsWatcher ::
  FilePath ->
  TChan FilePath ->
  TVar GenMap ->
  MVar () ->
  MVar () ->
  MVar () ->
  Bool ->
  IO ()
fsWatcher dir fsChan activePaths shutdownMVar fsWatcherDone logLock verbose = do
  let signalThreadExit = do
        logVerboseLn logLock verbose "done."
        putMVar fsWatcherDone ()
  fsAction `finally` signalThreadExit
  where
    fsAction =
      withManager $ \mgr -> do
        let normalizedDir = dropTrailingPathSeparator dir
        void
          $ watchTree --
            mgr
            normalizedDir
            isHtmlFsEvent
            --
            (debouncePerPath activePaths (signalFsEvent normalizedDir fsChan))
        logVerboseLn
          logLock
          verbose
          $ "[fsWatcher] Listening to "
          <> T.pack dir
          <> "..."
        readMVar shutdownMVar --
        logVerboseLn logLock verbose "[fsWatcher] Received shutdown signal."
        logVerbose logLock verbose "[fsWatcher] Cleaning up resources... "
    --
    isHtmlFsEvent e = case e of
      Added {}
      Modified {} -> takeExtension (eventPath e) == ".html"
      _ -> False
    signalFsEvent :: FilePath -> TChan FilePath -> Event -> IO ()
    signalFsEvent watchedDir chan event = do
      let relPath = makeRelative watchedDir $ eventPath event
      logVerboseLn logLock verbose
        $ "[fsWatcher] Detected event: "
        <> T.pack relPath
        <> " ("
        <> show event
        <> ")"
      atomically $ writeTChan chan relPath --

debouncePerPath :: TVar GenMap -> (Event -> IO ()) -> Event -> IO ()
debouncePerPath genMap action e = do
  let path = eventPath e
  currentGen <- atomically $ do
    m <- readTVar genMap
    let nextGen = findWithDefault 0 path m + 1 --
    writeTVar genMap $ insert path nextGen m
    pure nextGen

  void . forkIO $ do
    threadDelay 500_000 --
    shouldFire <- atomically $ do
      m <- readTVar genMap
      case lookup path m of --
        Just latestGen | latestGen == currentGen -> do
          modifyTVar' genMap $ delete path
          pure True
        _ -> pure False --
    when shouldFire $ action e

wsHandler :: TChan FilePath -> MVar () -> Bool -> WS.ServerApp
wsHandler fsChan logLock verbose pending = do
  conn <- WS.acceptRequest pending --
  msg <- WS.receiveData conn :: IO Text --
  if msg == "LILAC_WS_HEARTBEAT_FROM_JS_CLIENT"
    then do
      logVerboseLn logLock verbose $ "[wsHandler] Ignored heartbeat (ON INIT): " <> msg
      WS.sendClose conn ("Path expected" :: Text) --
    else handleClientConnection conn fsChan msg logLock verbose

drainChannel :: TChan a -> STM ()
drainChannel chan = do
  isEmpty <- isEmptyTChan chan
  when (not isEmpty) $ do
    _ <- readTChan chan
    drainChannel chan

handleClientConnection ::
  WS.Connection ->
  TChan FilePath ->
  Text ->
  MVar () ->
  Bool ->
  IO ()
handleClientConnection conn fsChan msg logLock verbose = do
  let browserLocation
        | T.null cleaned = "index.html"
        | otherwise = cleaned
        where
          cleaned = T.dropAround (== '/') msg
  logVerboseLn
    logLock
    verbose
    $ "[wsHandler] Browser connected: "
    <> browserLocation

  fsPathChan <- atomically $ do
    chan <- dupTChan fsChan --
    drainChannel chan
    pure chan

  --
  let fsListenerThread = waitForMatch conn fsPathChan browserLocation

  --
  let heartbeatHandlerThread = do
        heartbeat <- WS.receiveData conn :: IO Text
        logVerboseLn
          logLock
          verbose
          $ "[wsHandler] Ignored heartbeat: "
          <> heartbeat
        heartbeatHandlerThread --

  --
  (withAsync fsListenerThread $ \_ -> heartbeatHandlerThread)
    `catch` \(e :: WS.ConnectionException) -> do
      logVerboseLn logLock verbose
        $ "[wsHandler] Browser disconnected: "
        <> browserLocation
        <> " (Reason: "
        <> T.pack (show e)
        <> ")"

waitForMatch :: WS.Connection -> TChan FilePath -> Text -> IO ()
waitForMatch conn fsPathChan browserLocation = do
  modifiedFile <- T.pack <$> atomically (readTChan fsPathChan) --
  if (browserLocation == modifiedFile)
    then do
      t <- getTimestamp
      let time = "[" <> t <> "]"
          announcement =
            time
              <> " FS event detected, reloading "
              <> browserLocation
              <> "."
      putTextLn announcement
      WS.sendTextData conn ("reload" :: Text) --
    else
      waitForMatch conn fsPathChan browserLocation --

getTimestamp :: IO Text
getTimestamp = do
  now <- getZonedTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" now
  pure $ T.pack timeStr

logVerbose :: MVar () -> Bool -> Text -> IO ()
logVerbose lock verbose msg = when verbose $ do
  t <- getTimestamp
  withMVar lock $ \_ ->
    T.hPutStr stderr $ "[" <> t <> "] " <> msg

logVerboseLn :: MVar () -> Bool -> Text -> IO ()
logVerboseLn lock verbose msg = when verbose $ do
  t <- getTimestamp
  withMVar lock $ \_ ->
    T.hPutStrLn stderr $ "[" <> t <> "] " <> msg
