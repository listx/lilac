module Main (main) where

import Data.Aeson (ToJSON, eitherDecodeStrictText)
import Data.Aeson.Encode.Pretty qualified as EP
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Version (showVersion)
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Lilac.Types
  ( Archive,
    GPath,
    Object,
    OrgDoc,
    decodeGPath,
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
    strOption,
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
    show,
    sort,
    when,
    writeFileLBS,
    ($),
    (&&),
    (+),
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
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP

readOsPath :: ReadM P.OsPath
readOsPath = f =<< str
  where
    f :: (MonadFail m) => FilePath -> m P.OsPath
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
  { inputs :: [GPath]
  }

type WeaveOpts :: Type
data WeaveOpts = WeaveOpts
  { inputs :: [FilePath],
    outDir :: FilePath,
    watch :: Bool
  }

type CompileOpts :: Type
data CompileOpts = CompileOpts
  { inputs :: [P.OsPath]
  }

type ArchiveOpts :: Type
data ArchiveOpts = ArchiveOpts
  { inputs :: [GPath],
    outFile :: P.OsPath
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
          (progDesc "Create archive from object files.")
    cmdCompile =
      command
        "compile"
        $ info
          (Compile <$> compileOptsHandler)
          (progDesc "Compile Org file into object file.")
    cmdTangle =
      command
        "tangle"
        $ info
          (Tangle <$> tangleOptsHandler)
          (progDesc "Extract source code from files.")
    cmdWeave =
      command
        "weave"
        $ info
          (Weave <$> weaveOptsHandler)
          (progDesc "Generate HTML representation.")

tangleOptsHandler :: OptionHandler TangleOpts
tangleOptsHandler =
  TangleOpts
    <$> (some $ argument readGPath (metavar "FILES..."))

weaveOptsHandler :: OptionHandler WeaveOpts
weaveOptsHandler =
  WeaveOpts
    <$> (some $ argument str (metavar "FILES..."))
    <*> ( strOption
            ( long "out-dir"
                <> metavar "DIR"
                <> value "."
                <> showDefault
                <> help
                  """
                  Directory for saving the HTML file.
                  """
            )
        )
    <*> ( flag
            False
            True
            ( long "watch"
                <> help
                  """
                  Inject JS to make the HTML page live-reload.
                  Uses https://livejs.com/live.js.
                  """
            )
        )

compileOptsHandler :: OptionHandler CompileOpts
compileOptsHandler =
  CompileOpts
    <$> (some $ argument readOsPath (metavar "FILES..."))

archiveOptsHandler :: OptionHandler ArchiveOpts
archiveOptsHandler =
  ArchiveOpts
    <$> (some $ argument readGPath (metavar "FILES..."))
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
            (long "version" <> short 'v' <> help "Show version")

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
              (runStateT LP.orgDocParser LP.defaultOrgParserState)
              (show inputPath)
              input
      case parseResult of
        Left errBundle -> do
          die $ MP.errorBundlePretty errBundle
        Right (orgDoc, _) -> do
          pure (LC.mkObject orgDoc, inputPath)
    writeObjToPath :: (ToJSON a) => (a, P.OsPath) -> IO ()
    writeObjToPath (obj, inputPath) = do
      outPath <- do
        let base = P.takeBaseName inputPath
            objExtension = P.unsafeEncodeUtf "lobj"
        P.decodeUtf $ P.addExtension base objExtension
      writeJSONPretty outPath obj

lilacArchive :: ArchiveOpts -> IO ()
lilacArchive archiveOpts = do
  pathsObjects <- mapM readObjectDirectly archiveOpts.inputs
  case LC.mkArchive pathsObjects of
    Left errMsg -> die errMsg
    Right archive -> do
      outPath <- do
        let base = P.takeBaseName archiveOpts.outFile
            objExtension = P.unsafeEncodeUtf "larc"
        P.decodeUtf $ P.addExtension base objExtension
      writeJSONPretty outPath archive
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
          tangle globalOpts archive
          when (globalOpts.verbose)
            $ putStrLn' "Finished tangling."
    putStrLn' =
      if globalOpts.color
        then putStrLnColor boldWhite
        else putStrLn

tangle :: GlobalOpts -> Archive -> IO ()
tangle globalOpts archive = do
  let blocks = sort $ LT.expandRootBlocks archive
  putStrLn' $ "Blocks to process: " <> (show $ length blocks)
  (tangled, tangledPaths, skipped) <- foldlM f (0, [], 0) blocks
  putStrLn' $ "Tangled: " <> (show tangled)
  when (tangled > 0) $ mapM_ printTangled $ reverse tangledPaths
  putStrLn' $ "Skipped: " <> (show skipped)
  where
    f :: (Int, [P.OsPath], Int) -> (P.OsPath, T.Text) -> IO (Int, [P.OsPath], Int)
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
    printTangled :: P.OsPath -> IO ()
    printTangled path = putStrLn $ "  " <> show path
    putStrLn' =
      if globalOpts.color
        then putStrLnColor boldWhite
        else putStrLn

lilacWeave :: WeaveOpts -> IO ()
lilacWeave weaveOpts = do
  writeCssFile
  mapM_ weaveOrgFile $ weaveOpts.inputs
  where
    writeCssFile = do
      cssPath <- do
        dir <- P.encodeUtf weaveOpts.outDir
        cssDir <- P.encodeUtf "css"
        css <- P.encodeUtf "default.css"
        pure $ P.joinPath [dir, cssDir, css]
      LU.writeFileRobustly cssPath $ LWC.genCss
    weaveOrgFile inputOrgFile = do
      input <- LU.readFrom "" inputOrgFile
      let parseResult =
            MP.runParser
              (runStateT LP.orgDocParser LP.defaultOrgParserState)
              inputOrgFile
              input
      case parseResult of
        Left errBundle -> do
          die $ MP.errorBundlePretty errBundle
        Right (orgDoc, _) -> do
          putStrLn $ "Weaving " <> inputOrgFile
          outPath <- do
            dir <- P.encodeUtf weaveOpts.outDir
            base <- P.takeBaseName <$> P.encodeUtf inputOrgFile
            let withoutExtension = P.joinPath [dir, base]
                html = P.unsafeEncodeUtf "html"
            pure $ P.addExtension withoutExtension html
          weaveToDisk outPath orgDoc weaveOpts.watch
          putStrLn "Finished weaving."

weaveToDisk :: P.OsPath -> OrgDoc -> Bool -> IO ()
weaveToDisk fp od watch = do
  let weaveConf =
        LW.WeaveConf
          { LW.orgDoc = od,
            LW.goids = M.empty,
            LW.watch = watch
          }
  LU.writeFileRobustly fp
    $ LW.renderAsText weaveConf LW.weave

writeJSONPretty :: (ToJSON a) => FilePath -> a -> IO ()
writeJSONPretty path marshalMe =
  writeFileLBS
    path
    $ EP.encodePretty'
      EP.Config
        { EP.confIndent = EP.Spaces 2,
          EP.confCompare = mempty,
          EP.confNumFormat = EP.Generic,
          EP.confTrailingNewline = True
        }
      marshalMe

putStrColor :: [C.SGR] -> String -> IO ()
putStrColor sgrs s = do
  C.setSGR sgrs
  putStr s
  C.setSGR [C.Reset]

putStrLnColor :: [C.SGR] -> String -> IO ()
putStrLnColor sgrs s = do
  putStrColor sgrs s
  putStrLn ""
