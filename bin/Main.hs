module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( newBroadcastTChanIO,
    newTVarIO,
  )
import Control.Exception (finally)
import Data.Aeson (ToJSON, eitherDecodeStrictText, encodeFile)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.FileEmbed (embedFile)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Serve (fsWatcher, liveReloadMiddleware)
import Lilac.Tangle qualified as LT
import Lilac.Types
  ( Archive (objects),
    ArchiveOpts (ArchiveOpts, inputs, outFile),
    Block (meta),
    Cell (CBlock),
    CompileOpts (CompileOpts, inputs),
    GPath,
    GlobalOpts
      ( GlobalOpts,
        color,
        projectConf,
        subcommand,
        verbose
      ),
    InitOpts (InitOpts, onlyLocalImports),
    LintOpts (LintOpts, inputs),
    Object (orgDoc),
    OrgDoc (cells),
    ProjectConf
      ( ProjectConf,
        bibliography,
        citationStyle,
        homepage,
        htmlHead,
        name,
        nonTangledPathspecs,
        repo,
        version
      ),
    RawProjectConf
      ( bibliography,
        citationStyle,
        homepage,
        htmlHead,
        name,
        nonTangledPathspecs,
        repo,
        version
      ),
    ServeOpts (ServeOpts, dir, port),
    Subcommand
      ( ComArchive,
        ComCompile,
        ComInit,
        ComLint,
        ComServe,
        ComTangle,
        ComWeave
      ),
    TangleOpts (TangleOpts, inputs, outDir),
    WeaveConf
      ( WeaveConf,
        archive,
        citeprocResult,
        originObjectPath,
        outDir,
        projectConf,
        rootRelativePrefix
      ),
    WeaveOpts
      ( WeaveOpts,
        inputs,
        outDir,
        rootRelativePrefix,
        writeCss,
        writeJs
      ),
    decodeGPath,
    defaultOrgParserState,
    encodeGPath,
  )
import Lilac.Util qualified as LU
import Lilac.Version (lilacVersion)
import Lilac.Weave qualified as LW
import Lilac.Weave.Css qualified as LWC
import Network.URI qualified as N
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Options.Applicative
  ( Parser,
    ReadM,
    argument,
    command,
    customExecParser,
    eitherReader,
    flag,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    infoOption,
    long,
    metavar,
    option,
    prefs,
    progDesc,
    short,
    showDefault,
    showHelpOnEmpty,
    some,
    str,
    value,
  )
import Options.Applicative.Builder (InfoMod)
import Relude
  ( Bool (False, True),
    ByteString,
    Either (Left, Right),
    FilePath,
    IO,
    Int,
    LByteString,
    Maybe (Just, Nothing),
    MonadFail,
    String,
    Text,
    Type,
    concat,
    concatMap,
    decodeUtf8,
    fail,
    fmap,
    foldlM,
    forM_,
    fromMaybe,
    intercalate,
    length,
    map,
    mapM,
    mapM_,
    mapMaybe,
    mempty,
    newEmptyMVar,
    newMVar,
    not,
    null,
    otherwise,
    pure,
    putLBS,
    putMVar,
    putStr,
    putStrLn,
    putTextLn,
    readMaybe,
    reverse,
    runStateT,
    sequence_,
    show,
    sort,
    takeMVar,
    toString,
    when,
    ($),
    (&&),
    (+),
    (.),
    (/=),
    (<),
    (<$>),
    (<*>),
    (<>),
    (=<<),
    (==),
    (>),
    (||),
  )
import Relude.Extra.Map (lookup)
import System.Console.ANSI qualified as C
import System.Directory.OsPath qualified as D
import System.Exit (die, exitFailure)
import System.OsPath (OsPath)
import System.OsPath qualified as P
import System.Process.Typed (proc, readProcessStdout_)
import Text.Megaparsec qualified as MP
import Toml (decode)
import Toml.Schema
  ( Result (Failure, Success),
  )

readOsPath :: ReadM OsPath
readOsPath = f =<< str
  where
    f :: (MonadFail m) => FilePath -> m OsPath
    f maybeOsPath
      | Just osPath <- P.encodeUtf maybeOsPath,
        P.isValid osPath =
          pure osPath
      | otherwise = fail $ "invalid OsPath: " <> maybeOsPath

readGPath :: ReadM GPath
readGPath = encodeGPath =<< str

type OptionHandler :: Type -> Type
type OptionHandler = Options.Applicative.Parser

globalOptionsHandler :: OptionHandler GlobalOpts
globalOptionsHandler =
  GlobalOpts
    <$> subcommandHandler
    <*> ( option
            readOsPath
            ( long "project-conf"
                <> metavar "PROJECT_CONF"
                <> value (P.unsafeEncodeUtf "Lilac.toml")
                <> showDefault
                <> help
                  "Project configuration file, in TOML."
            )
        )
    <*> ( flag
            True
            False
            ( long "no-color"
                <> help
                  "Do not print in color (i.e., suppress ANSI escape codes)."
            )
        )
    <*> ( flag
            False
            True
            ( long "verbose"
                <> help
                  "Be more descriptive about what's happening."
            )
        )

subcommandHandler :: OptionHandler Subcommand
subcommandHandler =
  hsubparser --
    $ metavar "SUBCOMMAND"
    <> cmdCompile
    <> cmdArchive
    <> cmdTangle
    <> cmdWeave
    <> cmdServe
    <> cmdLint
    <> cmdInit
  where
    cmdCompile =
      command
        "compile"
        $ info
          (ComCompile <$> compileOptsHandler)
          (progDesc "Compile Org files into objects.")
    cmdArchive =
      command
        "archive"
        $ info
          (ComArchive <$> archiveOptsHandler)
          (progDesc "Combine objects into a single archive.")
    cmdTangle =
      command
        "tangle"
        $ info
          (ComTangle <$> tangleOptsHandler)
          (progDesc "Extract source code from archives.")
    cmdWeave =
      command
        "weave"
        $ info
          (ComWeave <$> weaveOptsHandler)
          (progDesc "Generate HTML documentation from archives.")
    cmdServe =
      command
        "serve"
        $ info
          (ComServe <$> serveOptsHandler)
          (progDesc "Serve a directory of HTML files.")
    cmdLint =
      command
        "lint"
        $ info
          (ComLint <$> lintOptsHandler)
          (progDesc "Look for tracked files that are not tangled by Lilac.")
    cmdInit =
      command
        "init"
        $ info
          (ComInit <$> initOptsHandler)
          (progDesc "Initialize a new (empty) Lilac project.")

tangleOptsHandler :: OptionHandler TangleOpts
tangleOptsHandler =
  TangleOpts
    <$> (some $ argument readGPath (metavar "ARCHIVES..."))
    <*> ( option
            readOsPath
            ( long "out-dir"
                <> metavar "DIR"
                <> value mempty
                <> showDefault
                <> help
                  ( toString @String
                      $ decodeUtf8
                        $(embedFile "src/Lilac/cli/tangle/out-dir.txt")
                  )
            )
        )

weaveOptsHandler :: OptionHandler WeaveOpts
weaveOptsHandler =
  WeaveOpts
    <$> (some $ argument readOsPath (metavar "ARCHIVES..."))
    <*> ( option
            readOsPath
            ( long "out-dir"
                <> metavar "DIR"
                <> value (P.unsafeEncodeUtf ".")
                <> showDefault
                <> help --
                  ( toString @String
                      $ decodeUtf8
                        $(embedFile "src/Lilac/cli/weave/out-dir.txt")
                  )
            )
        )
    <*> ( option
            readOsPath
            ( long "root-relative-prefix"
                <> metavar "DIR"
                <> value (P.unsafeEncodeUtf "/")
                <> showDefault
                <> help --
                  ( toString @String
                      $ decodeUtf8
                        $(embedFile "src/Lilac/cli/weave/root-relative-prefix.txt")
                  )
            )
        )
    <*> ( flag
            False
            True
            ( long "write-css"
                <> help
                  "Write CSS file. This does not require Org mode input."
            )
        )
    <*> ( flag
            False
            True
            ( long "write-js"
                <> help
                  "Write JS file. This does not require Org mode input."
            )
        )

compileOptsHandler :: OptionHandler CompileOpts
compileOptsHandler =
  CompileOpts
    <$> (some $ argument readOsPath (metavar "ORG_FILES..."))

archiveOptsHandler :: OptionHandler ArchiveOpts
archiveOptsHandler =
  ArchiveOpts
    <$> ( some
            $ argument
              readGPath
              ( metavar "OBJECTS..."
                  <> help
                    "Objects to combine into an archive."
              )
        )
    <*> ( option
            readOsPath
            ( long "out-file"
                <> metavar "FILE"
                <> value (P.unsafeEncodeUtf "out.larc")
                <> showDefault
                <> help
                  "Output file path for the archive."
            )
        )

lintOptsHandler :: OptionHandler LintOpts
lintOptsHandler =
  LintOpts
    <$> (some $ argument readOsPath (metavar "ARCHIVES..."))

serveOptsHandler :: OptionHandler ServeOpts
serveOptsHandler =
  ServeOpts
    <$> (argument readOsPath (metavar "DIR"))
    <*> ( option
            portReader
            ( long "port"
                <> metavar "PORT"
                <> value 8080
                <> showDefault
                <> help
                  "Port to serve over."
            )
        )
  where
    portReader :: ReadM Int
    portReader = eitherReader $ \s ->
      case readMaybe s of
        Just n | n > 0 && n < 65536 -> Right n
        Just _ -> Left "Port must be between 1 and 65535"
        Nothing -> Left "Must be a valid integer"

initOptsHandler :: OptionHandler InitOpts
initOptsHandler =
  InitOpts
    <$> ( flag
            False
            True
            ( long "only-local-imports"
                <> help
                  ( toString @String
                      $ decodeUtf8
                        $(embedFile "src/Lilac/cli/init/only-local-imports.txt")
                  )
            )
        )

boldWhite :: [C.SGR]
boldWhite =
  [ C.SetColor C.Foreground C.Vivid C.White,
    C.SetConsoleIntensity C.BoldIntensity
  ]

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) mainOptions
  lilac opts
  where
    mainOptions =
      info
        (helper <*> versionOptionHandler <*> globalOptionsHandler)
        lilacDesc
    lilacDesc :: InfoMod a
    lilacDesc =
      fullDesc
        <> header
          "lilac - Literate programming with Orgmode and Noweb-style blocks."
    versionOptionHandler :: OptionHandler (a -> a)
    versionOptionHandler =
      infoOption
        $(lilacVersion)
        (long "version" <> short 'v' <> help "Show version.")

lilac :: GlobalOpts -> IO ()
lilac globalOpts = case globalOpts.subcommand of
  ComArchive archiveOpts -> lilacArchive globalOpts archiveOpts
  ComCompile compileOpts -> lilacCompile globalOpts compileOpts
  ComTangle tangleOpts -> lilacTangle globalOpts tangleOpts
  ComWeave weaveOpts -> lilacWeave globalOpts weaveOpts
  ComLint lintOpts -> lilacLint globalOpts lintOpts
  ComServe serveOpts -> lilacServe globalOpts serveOpts
  ComInit initOpts -> lilacInit globalOpts initOpts

readProjectConf :: OsPath -> IO ProjectConf
readProjectConf path = do
  input <- LU.readFileRobustly path
  case parseProjectConf input of
    Left errMsg -> do
      putTextLn "Could not read Lilac.toml"
      die errMsg
    Right projectConf -> pure projectConf

parseProjectConf :: Text -> Either String ProjectConf
parseProjectConf input = case parseRawProjectConf input of
  Left errMsg -> Left errMsg
  Right rawProjectConf ->
    Right
      ProjectConf
        { name = rawProjectConf.name,
          homepage = N.parseURI $ T.unpack rawProjectConf.homepage,
          repo = N.parseURI $ T.unpack rawProjectConf.repo,
          version =
            if T.null rawProjectConf.version
              then Nothing
              else Just rawProjectConf.version,
          nonTangledPathspecs = rawProjectConf.nonTangledPathspecs,
          bibliography = fmap T.unpack $ fromMaybe [] rawProjectConf.bibliography,
          citationStyle = fmap T.unpack $ rawProjectConf.citationStyle,
          htmlHead = rawProjectConf.htmlHead
        }

parseRawProjectConf :: Text -> Either String RawProjectConf
parseRawProjectConf input =
  case Toml.decode input of
    Failure errs -> Left ("TOML Error:\n" <> intercalate "\n" errs)
    Success _ c -> Right c

lilacCompile :: GlobalOpts -> CompileOpts -> IO ()
lilacCompile _globalOpts compileOpts = do
  objsAndPaths <- mapM genObject compileOpts.inputs
  mapM_ writeObjToPath objsAndPaths
  where
    genObject inputPath = do
      input <- LU.readFileRobustly inputPath
      let parseResult =
            MP.runParser
              (runStateT LP.orgDocParser defaultOrgParserState) --
              (show inputPath)
              input
      case parseResult of
        Left errBundle -> do
          die $ MP.errorBundlePretty errBundle
        Right (orgDoc, _) -> do
          let obj = LC.mkObject orgDoc --
          pure (obj, inputPath)
    writeObjToPath :: (ToJSON a) => (a, OsPath) -> IO ()
    writeObjToPath (obj, inputPath) = do
      outPath <- do
        let base = P.replaceExtension inputPath $ P.unsafeEncodeUtf "lobj"
        P.decodeUtf base
      encodeFile outPath obj

lilacArchive :: GlobalOpts -> ArchiveOpts -> IO ()
lilacArchive globalOpts archiveOpts = do
  projectConf <- readProjectConf globalOpts.projectConf
  pathsObjects <- mapM readObjectDirectly archiveOpts.inputs
  maybeArchive <- LC.mkArchive projectConf pathsObjects
  case maybeArchive of
    Left errMsg -> die errMsg
    Right archive -> do
      let cycles = LC.findCycles archive
      when (not $ null cycles) $ do
        die
          $ "Found cycles: "
          <> intercalate
            "\n"
            (map show cycles)
      outPath <- do
        let base =
              P.replaceExtension archiveOpts.outFile
                $ P.unsafeEncodeUtf "larc"
        P.decodeUtf base
      encodeFile outPath archive
  where
    readObjectDirectly :: GPath -> IO (GPath, Object)
    readObjectDirectly gPath = do
      let fp = decodeGPath gPath
          inputPath = P.unsafeEncodeUtf fp
      text <- LU.readFileRobustly inputPath
      case eitherDecodeStrictText text :: Either String Object of
        Left errMsg -> die errMsg
        Right obj -> pure (gPath, obj)

lilacTangle :: GlobalOpts -> TangleOpts -> IO ()
lilacTangle globalOpts tangleOpts = do
  mapM_ tangleArchiveFile tangleOpts.inputs
  where
    tangleArchiveFile gPath = do
      let fp = decodeGPath gPath
          inputPath = P.unsafeEncodeUtf fp
      text <- LU.readFileRobustly inputPath
      case eitherDecodeStrictText text :: Either String Archive of
        Left errMsg -> die errMsg
        Right archive -> do
          when (globalOpts.verbose) $ do
            putStrLn' $ "Tangling " <> fp
          tangle globalOpts tangleOpts archive
          when (globalOpts.verbose)
            $ putStrLn' "Finished tangling."
    putStrLn' =
      if globalOpts.color
        then putStrLnColor boldWhite
        else putStrLn

tangle :: GlobalOpts -> TangleOpts -> Archive -> IO ()
tangle globalOpts tangleOpts archive = do
  let blocks = sort $ LT.expandRootBlocks archive tangleOpts.outDir
  putStrLn' $ "Blocks to process: " <> (show $ length blocks)
  (tangled, tangledPaths, skipped) <- foldlM f (0, [], 0) blocks
  putStrLn' $ "Tangled: " <> (show tangled)
  when (tangled > 0) $ mapM_ printTangled $ reverse tangledPaths
  putStrLn' $ "Skipped: " <> (show skipped)
  where
    f :: (Int, [OsPath], Int) -> (OsPath, Text) -> IO (Int, [OsPath], Int)
    f (tangled, tangledPaths, skipped) (path, contentToWrite) = do
      alreadyExists <- D.doesPathExist path
      existingContent <- LU.readFileRobustly path
      let g
            | (alreadyExists && existingContent /= contentToWrite)
                || not alreadyExists = do
                LU.writeFileRobustly path contentToWrite
                pure (tangled + 1, path : tangledPaths, skipped)
            | otherwise = pure (tangled, tangledPaths, skipped + 1)
       in g
    printTangled :: OsPath -> IO ()
    printTangled path = do
      fp <- P.decodeUtf path
      putStrLn $ "  " <> fp
    putStrLn' =
      if globalOpts.color
        then putStrLnColor boldWhite
        else putStrLn

lilacWeave :: GlobalOpts -> WeaveOpts -> IO ()
lilacWeave globalOpts weaveOpts = do
  projectConf <- readProjectConf globalOpts.projectConf
  when weaveOpts.writeCss writeCssFile
  when weaveOpts.writeJs writeJsFile
  mapM_ (weaveArchiveFile projectConf weaveOpts.outDir) $ weaveOpts.inputs
  where
    writeCssFile = do
      cssPath <- do
        cssDir <- P.encodeUtf "css"
        css <- P.encodeUtf "default.css" --
        pure $ P.joinPath [weaveOpts.outDir, cssDir, css]
      LU.writeFileRobustly cssPath $ LWC.genCss
    writeJsFile = do
      jsPath <- do
        jsDir <- P.encodeUtf "js"
        js <- P.encodeUtf "lilac.js"
        pure $ P.joinPath [weaveOpts.outDir, jsDir, js]
      LU.writeFileRobustly jsPath lilacJs
    weaveArchiveFile projectConf outDir inputPath = do
      text <- LU.readFileRobustly inputPath
      case eitherDecodeStrictText text :: Either String Archive of
        Left errMsg -> die errMsg
        Right archive -> do
          fp <- P.decodeUtf inputPath
          outDir' <- encodeGPath =<< P.decodeUtf outDir
          rootRelativePrefix <-
            encodeGPath =<< P.decodeUtf weaveOpts.rootRelativePrefix
          putStrLn $ "Weaving " <> fp
          sequence_
            $ M.foldlWithKey'
              ( weaveToDisk
                  projectConf
                  outDir'
                  rootRelativePrefix
                  archive
              )
              []
              archive.objects
          putStrLn "Finished weaving."

weaveToDisk ::
  ProjectConf ->
  GPath ->
  GPath ->
  Archive ->
  [IO ()] ->
  GPath ->
  Object ->
  [IO ()]
weaveToDisk projectConf outDir rootRelativePrefix archive acc objPath obj =
  let fp = decodeGPath objPath
      ext = P.unsafeEncodeUtf "html"
      outPath = P.replaceExtension (P.unsafeEncodeUtf fp) ext
      action = do
        outDirOsPath <- P.encodeUtf $ decodeGPath outDir
        result <- LW.prepareCitations projectConf objPath obj archive
        let weaveConf =
              WeaveConf
                { projectConf = projectConf,
                  archive = archive,
                  originObjectPath = objPath,
                  outDir = outDir,
                  rootRelativePrefix = rootRelativePrefix,
                  citeprocResult = result
                }
            outPathFinal = P.combine outDirOsPath outPath
        LU.writeFileRobustly outPathFinal
          . LW.renderAsText weaveConf
          $ LW.weaveObject obj
   in action : acc

lilacJs :: Text
lilacJs = TE.decodeUtf8 $(embedFile "lilac.js.injectme")

lilacServe :: GlobalOpts -> ServeOpts -> IO ()
lilacServe globalOpts serveOpts = do
  targetDir <- P.decodeUtf =<< D.makeAbsolute serveOpts.dir
  fsChan <- newBroadcastTChanIO
  activePaths <- newTVarIO mempty
  shutdownSignal <- newEmptyMVar
  fsWatcherDone <- newEmptyMVar
  logLock <- newMVar ()

  _ <-
    forkIO
      $ fsWatcher --
        targetDir
        fsChan
        activePaths
        shutdownSignal
        fsWatcherDone
        logLock
        globalOpts.verbose

  putTextLn $ "Serving Lilac at http://localhost:" <> show serveOpts.port
  putTextLn "Press Ctrl-C to shut down."

  let settings = setPort serveOpts.port defaultSettings
      server =
        liveReloadMiddleware
          targetDir
          fsChan
          logLock
          globalOpts.verbose
          $ staticApp (defaultFileServerSettings targetDir)
      teardown = do
        putTextLn "\nLilac server has stopped."
        putMVar shutdownSignal ()
        takeMVar fsWatcherDone
        putTextLn "Bye!"

  runSettings settings server --
    `finally` teardown --

lilacInit :: GlobalOpts -> InitOpts -> IO ()
lilacInit globalOpts initOpts = do
  let write p = LU.writeOrWarn (P.unsafeEncodeUtf p) --
  uuid <- nextRandom --
  when (globalOpts.verbose) $ do
    putStrLn $ "Using UUID " <> show uuid <> " for index.org"
  let indexOrg = mkIndexOrg initOpts uuid --
  when (globalOpts.verbose) $ do
    putStrLn "Writing index.org"
  write
    "index.org"
    indexOrg
  when (globalOpts.verbose) $ do
    putStrLn "Writing Lilac.toml"
  let lilacToml =
        decodeUtf8 @Text @ByteString $(embedFile "src/Lilac/init/Lilac.toml")
          <> if initOpts.onlyLocalImports
            then
              "\n"
                <> decodeUtf8
                  @Text
                  @ByteString
                  $(embedFile "src/Lilac/init/Lilac.toml.htmlHead.toml")
                <> "\n"
            else ""
  write
    "Lilac.toml"
    lilacToml
  when (globalOpts.verbose) $ do
    putStrLn "Tangling files out of index.org"
  bootstrapLilac initOpts --

mkIndexOrg :: InitOpts -> UUID -> Text
mkIndexOrg initOpts uuid =
  ":PROPERTIES:\n:ID:       "
    <> show uuid
    <> "\n:END:\n"
    <> decodeUtf8
      @Text
      @ByteString
      $(embedFile "src/Lilac/init/index.org")
    <> maybeHtmlHead
    <> decodeUtf8
      @Text
      @ByteString
      $(embedFile "src/Lilac/init/index.org-after-Lilac.toml")
    <> maybeVendorScripts
  where
    maybeHtmlHead =
      if initOpts.onlyLocalImports
        then
          "\n"
            <> decodeUtf8
              @Text
              @ByteString
              $(embedFile "src/Lilac/init/Lilac.toml.htmlHead.toml")
            <> "\n"
        else ""
    maybeVendorScripts =
      if initOpts.onlyLocalImports
        then
          "\n"
            <> decodeUtf8
              @Text
              @ByteString
              $(embedFile "src/Lilac/init/vendor-mathjax.org")
            <> "\n\n"
            <> decodeUtf8
              @Text
              @ByteString
              $(embedFile "src/Lilac/init/vendor-source-fonts.org")
            <> "\n"
        else ""

bootstrapLilac :: InitOpts -> IO ()
bootstrapLilac initOpts = do
  let runLilac :: [String] -> IO ()
      runLilac args = do
        outputLBS <- readProcessStdout_ $ proc "lilac" args
        putLBS outputLBS
  runLilac ["compile", "index.org"]
  runLilac ["archive", "index.lobj", "--out-file", "hello-world.larc"]
  runLilac ["tangle", "hello-world.larc", "--out-dir", "."] --
  when initOpts.onlyLocalImports $ do
    putLBS =<< readProcessStdout_ (proc "bash" ["vendor-mathjax.sh"])
    putLBS =<< readProcessStdout_ (proc "bash" ["vendor-source-fonts.sh"])
  putLBS =<< readProcessStdout_ (proc "make" [])

lilacLint :: GlobalOpts -> LintOpts -> IO ()
lilacLint globalOpts lintOpts = do
  projectConf <- readProjectConf globalOpts.projectConf
  rawOutput <- gitLsFiles projectConf.nonTangledPathspecs
  let trackedFiles =
        sort
          . map (TE.decodeUtf8Lenient . LBS.toStrict)
          $ LBS.lines rawOutput
  lintArchives lintOpts.inputs trackedFiles
  where
    lintArchives :: [OsPath] -> [Text] -> IO ()
    lintArchives inputPaths trackedFiles = do
      --
      tangledFiles <-
        sort . concat <$> mapM getTangledPathsFromArchive inputPaths
      let diff = diffSorted tangledFiles trackedFiles
      when (not $ null diff) $ do
        putStrLn "Tangled files and tracked files do not match."
        putStrLn "---------------------------------------------"
        forM_ diff $ \(b, path) -> do
          let prefix =
                if b
                  then "tracked but not tangled: "
                  else "tangled but not tracked: "
          putTextLn $ prefix <> path
        exitFailure
    getTangledPathsFromArchive :: OsPath -> IO [Text]
    getTangledPathsFromArchive inputPath = do
      text <- LU.readFileRobustly inputPath
      case eitherDecodeStrictText text :: Either String Archive of
        Left errMsg -> die errMsg
        Right archive -> do
          let getTangledPaths :: Object -> [Text]
              getTangledPaths obj = mapMaybe getTangledPath obj.orgDoc.cells
          pure $ concatMap getTangledPaths archive.objects
    getTangledPath :: Cell -> Maybe Text
    getTangledPath = \case
      CBlock b -> lookup "tangle" b.meta
      _ -> Nothing

gitLsFiles :: [Text] -> IO LByteString
gitLsFiles pathspecs = do
  let args = ["ls-files", "--", "."] <> (map T.unpack pathspecs)
      config = proc "git" args
  readProcessStdout_ config

diffSorted :: [Text] -> [Text] -> [(Bool, Text)]
diffSorted [] [] = []
diffSorted xs [] = map (False,) xs
diffSorted [] ys = map (True,) ys
diffSorted (x : xs) (y : ys)
  | x == y = diffSorted xs ys
  | x < y = (False, x) : diffSorted xs (y : ys)
  | otherwise = (True, y) : diffSorted (x : xs) ys

putStrColor :: [C.SGR] -> String -> IO ()
putStrColor sgrs s = do
  C.setSGR sgrs
  putStr s
  C.setSGR [C.Reset]

putStrLnColor :: [C.SGR] -> String -> IO ()
putStrLnColor sgrs s = do
  putStrColor sgrs s
  putStrLn ""
