module Main (main) where

import Data.Aeson (ToJSON, eitherDecodeStrictText, encodeFile)
import Data.Map.Strict qualified as M
import Data.Version (showVersion)
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Lilac.Types
  ( Archive (objects),
    GPath,
    Object,
    ProjectConf,
    WeaveConf
      ( WeaveConf,
        archive,
        citeprocResult,
        originObjectPath,
        outDir,
        projectConf
      ),
    decodeGPath,
    defaultOrgParserState,
    encodeGPath,
  )
import Lilac.Util qualified as LU
import Lilac.Version (lilacVersion)
import Lilac.Weave qualified as LW
import Lilac.Weave.Css qualified as LWC
import Options.Applicative
  ( Parser,
    ReadM,
    argument,
    command,
    customExecParser,
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
    (<*>),
  )
import Options.Applicative.Builder (InfoMod)
import Paths_lilac (version)
import Relude
  ( Bool (False, True),
    Either (Left, Right),
    FilePath,
    IO,
    Int,
    Maybe (Just),
    MonadFail,
    String,
    Text,
    Type,
    concat,
    fail,
    foldlM,
    length,
    mapM,
    mapM_,
    mempty,
    not,
    otherwise,
    pure,
    putStr,
    putStrLn,
    reverse,
    runStateT,
    sequence_,
    show,
    sort,
    when,
    ($),
    (&&),
    (+),
    (.),
    (/=),
    (<$>),
    (<>),
    (=<<),
    (>),
    (||),
  )
import System.Console.ANSI qualified as C
import System.Directory.OsPath qualified as D
import System.Exit (die)
import System.OsPath (OsPath)
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP

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

type GlobalOpts :: Type
data GlobalOpts = GlobalOpts
  { subcommand :: Subcommand,
    color :: Bool,
    verbose :: Bool
  }

type Subcommand :: Type
data Subcommand
  = Tangle TangleOpts
  | Weave WeaveOpts
  | Compile CompileOpts
  | Archive ArchiveOpts

type TangleOpts :: Type
data TangleOpts = TangleOpts
  { inputs :: [GPath],
    outDir :: OsPath
  }

type WeaveOpts :: Type
data WeaveOpts = WeaveOpts
  { inputs :: [OsPath],
    outDir :: OsPath,
    projectConf :: OsPath
  }

type CompileOpts :: Type
data CompileOpts = CompileOpts
  { inputs :: [OsPath]
  }

type ArchiveOpts :: Type
data ArchiveOpts = ArchiveOpts
  { inputs :: [GPath],
    outFile :: OsPath
  }

type OptionHandler :: Type -> Type
type OptionHandler = Options.Applicative.Parser

globalOptionsHandler :: OptionHandler GlobalOpts
globalOptionsHandler =
  GlobalOpts
    <$> subcommandHandler
    <*> ( flag
            True
            False
            ( long "no-color"
                <> help
                  """
                  Do not print in color (i.e., suppress ANSI escape codes).
                  """
            )
        )
    <*> ( flag
            False
            True
            ( long "verbose"
                <> help
                  """
                  Be more descriptive about what's happening.
                  """
            )
        )

subcommandHandler :: OptionHandler Subcommand
subcommandHandler =
  hsubparser
    $ metavar "SUBCOMMAND"
    <> cmdArchive
    <> cmdCompile
    <> cmdTangle
    <> cmdWeave
  where
    cmdArchive =
      command
        "archive"
        $ info
          (Archive <$> archiveOptsHandler)
          (progDesc "Combine objects into a single archive.")
    cmdCompile =
      command
        "compile"
        $ info
          (Compile <$> compileOptsHandler)
          (progDesc "Compile Org files into objects.")
    cmdTangle =
      command
        "tangle"
        $ info
          (Tangle <$> tangleOptsHandler)
          (progDesc "Extract source code from archives.")
    cmdWeave =
      command
        "weave"
        $ info
          (Weave <$> weaveOptsHandler)
          (progDesc "Generate HTML documentation from archives.")

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
                  """
                  Directory used for tangling. It's the starting point from
                  which all of the code blocks' ":tangle" header will be a
                  child of. Typically you'd want this to point to your repo's
                  root directory, so you can tangle a file in any location in
                  the repo root, regardless of where your Org files are authored
                  and checked into version control.
                  """
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
                <> help
                  """
                  Directory for saving the HTML files.
                  """
            )
        )
    <*> ( option
            readOsPath
            ( long "project-conf"
                <> metavar "PROJECT_CONF"
                <> value (P.unsafeEncodeUtf ".lilac.json")
                <> showDefault
                <> help
                  """
                  Project configuration file.
                  """
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
                    """
                    Objects to combine into an archive.
                    """
              )
        )
    <*> ( option
            readOsPath
            ( long "out-file"
                <> metavar "FILE"
                <> value (P.unsafeEncodeUtf "out.larc")
                <> showDefault
                <> help
                  """
                  Output file path for the archive.
                  """
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
          """
          lilac - Literate programming with Orgmode and Noweb-style blocks.
          """
    versionOptionHandler :: OptionHandler (a -> a)
    versionOptionHandler =
      let cabalVersion = version
       in infoOption
            (concat [showVersion cabalVersion, "-", $(lilacVersion)])
            (long "version" <> short 'v' <> help "Show version.")

lilac :: GlobalOpts -> IO ()
lilac globalOpts = case globalOpts.subcommand of
  Archive archiveOpts -> lilacArchive archiveOpts
  Compile compileOpts -> lilacCompile globalOpts compileOpts
  Tangle tangleOpts -> lilacTangle globalOpts tangleOpts
  Weave weaveOpts -> lilacWeave weaveOpts

lilacCompile :: GlobalOpts -> CompileOpts -> IO ()
lilacCompile _globalOpts compileOpts = do
  objsAndPaths <- mapM genObject compileOpts.inputs
  mapM_ writeObjToPath objsAndPaths
  where
    genObject inputPath = do
      input <- LU.readFileRobustly inputPath
      let parseResult =
            MP.runParser
              (runStateT LP.orgDocParser defaultOrgParserState)
              (show inputPath)
              input
      case parseResult of
        Left errBundle -> do
          die $ MP.errorBundlePretty errBundle
        Right (orgDoc, _) -> do
          obj <- LC.mkObject orgDoc
          pure (obj, inputPath)
    writeObjToPath :: (ToJSON a) => (a, OsPath) -> IO ()
    writeObjToPath (obj, inputPath) = do
      outPath <- do
        let base = P.replaceExtension inputPath $ P.unsafeEncodeUtf "lobj"
        P.decodeUtf base
      encodeFile outPath obj

lilacArchive :: ArchiveOpts -> IO ()
lilacArchive archiveOpts = do
  pathsObjects <- mapM readObjectDirectly archiveOpts.inputs
  maybeArchive <- LC.mkArchive pathsObjects
  case maybeArchive of
    Left errMsg -> die errMsg
    Right archive -> do
      outPath <- do
        let base = P.replaceExtension archiveOpts.outFile $ P.unsafeEncodeUtf "larc"
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

lilacWeave :: WeaveOpts -> IO ()
lilacWeave weaveOpts = do
  projectConf <- readProjectConf
  writeCssFile
  mapM_ (weaveArchiveFile projectConf weaveOpts.outDir) $ weaveOpts.inputs
  where
    readProjectConf = do
      input <- LU.readFileRobustly weaveOpts.projectConf
      case LW.parseProjectConf input of
        Left errMsg -> die errMsg
        Right projectConf -> pure projectConf
    writeCssFile = do
      cssPath <- do
        cssDir <- P.encodeUtf "css"
        css <- P.encodeUtf "default.css"
        pure $ P.joinPath [weaveOpts.outDir, cssDir, css]
      LU.writeFileRobustly cssPath $ LWC.genCss
    weaveArchiveFile projectConf outDir inputPath = do
      text <- LU.readFileRobustly inputPath
      case eitherDecodeStrictText text :: Either String Archive of
        Left errMsg -> die errMsg
        Right archive -> do
          fp <- P.decodeUtf inputPath
          outDir' <- encodeGPath =<< P.decodeUtf outDir
          putStrLn $ "Weaving " <> fp
          sequence_
            $ M.foldlWithKey'
              (weaveToDisk projectConf outDir' archive)
              []
              archive.objects
          putStrLn "Finished weaving."

weaveToDisk ::
  ProjectConf ->
  GPath ->
  Archive ->
  [IO ()] ->
  GPath ->
  Object ->
  [IO ()]
weaveToDisk projectConf outDir archive acc objPath obj =
  let fp = decodeGPath objPath
      ext = P.unsafeEncodeUtf "html"
      outPath = P.replaceExtension (P.unsafeEncodeUtf fp) ext
      action = do
        result <- LW.prepareCitations objPath obj archive
        let weaveConf =
              WeaveConf
                { projectConf = projectConf,
                  archive = archive,
                  originObjectPath = objPath,
                  outDir = outDir,
                  citeprocResult = result
                }
        LU.writeFileRobustly outPath
          . LW.renderAsText weaveConf
          $ LW.weaveObject obj
   in action : acc

putStrColor :: [C.SGR] -> String -> IO ()
putStrColor sgrs s = do
  C.setSGR sgrs
  putStr s
  C.setSGR [C.Reset]

putStrLnColor :: [C.SGR] -> String -> IO ()
putStrLnColor sgrs s = do
  putStrColor sgrs s
  putStrLn ""
