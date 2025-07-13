module Main (main) where

import Data.Aeson (ToJSON, eitherDecodeStrictText, encodeFile)
import Data.Version (showVersion)
import Lilac.Compile qualified as LC
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Lilac.Types
  ( GPath,
    Object,
    OrgParserState
      ( goids
      ),
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
    Maybe (Just),
    MonadFail,
    String,
    Type,
    concat,
    fail,
    foldlM,
    mapM,
    mapM_,
    not,
    null,
    otherwise,
    pure,
    putStrLn,
    runStateT,
    show,
    when,
    ($),
    (<$>),
    (<>),
    (=<<),
  )
import System.Exit (die)
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP
import Text.Pretty.Simple qualified as PP

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
    orgIdLocationsFile :: FilePath,
    color :: Bool,
    verbose :: Bool
  }

type Subcommand :: Type
data Subcommand
  = Check CheckOpts
  | Tangle TangleOpts
  | Weave WeaveOpts
  | Compile CompileOpts
  | Archive ArchiveOpts
  | WriteGOIDs WriteGOIDsOpts

type CheckOpts :: Type
data CheckOpts = CheckOpts
  { input :: FilePath,
    dumpJson :: Bool
  }

type TangleOpts :: Type
data TangleOpts = TangleOpts
  { inputs :: [FilePath]
  }

type WeaveOpts :: Type
data WeaveOpts = WeaveOpts
  { inputs :: [FilePath],
    outDir :: FilePath,
    watch :: Bool
  }

type WriteGOIDsOpts :: Type
data WriteGOIDsOpts = WriteGOIDsOpts
  { outFile :: FilePath,
    inputs :: [FilePath]
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
    <*> ( strOption
            ( long "org-id-locations-file"
                <> metavar "FILE"
                <> value ".orgids"
                <> showDefault
                <> help
                  """
                  File with Org ID locations. See 'org-id-locations-file' in
                  Emacs documentation for more information.
                  """
            )
        )
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
    <> cmdCheck
    <> cmdCompile
    <> cmdTangle
    <> cmdWeave
    <> cmdWriteGOIDs
  where
    cmdArchive =
      command
        "archive"
        $ info
          (Archive <$> archiveOptsHandler)
          (progDesc "Create archive from object files.")
    cmdCheck =
      command
        "check"
        $ info
          (Check <$> checkOptsHandler)
          (progDesc "Check input Org file for errors.")
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
    cmdWriteGOIDs =
      command
        "write-goids"
        $ info
          (WriteGOIDs <$> writeGOIDsOptsHandler)
          (progDesc "Generate global Org IDs from a list of Org files.")

checkOptsHandler :: OptionHandler CheckOpts
checkOptsHandler =
  CheckOpts
    <$> (argument str (metavar "FILE"))
    <*> ( flag
            False
            True
            ( long "dump-json"
                <> help
                  """
                  Dump Org file as JSON. This is useful for debugging.
                  """
            )
        )

tangleOptsHandler :: OptionHandler TangleOpts
tangleOptsHandler =
  TangleOpts
    <$> (some $ argument str (metavar "FILES..."))

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

writeGOIDsOptsHandler :: OptionHandler WriteGOIDsOpts
writeGOIDsOptsHandler =
  WriteGOIDsOpts
    <$> ( strOption
            ( long "out-file"
                <> metavar "FILE"
                <> value ".orgids"
                <> showDefault
                <> help
                  """
                  File to save global Org IDs.
                  """
            )
        )
    <*> (some $ argument str (metavar "FILES..."))

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
  Check checkOpts -> lilacCheck globalOpts checkOpts
  Compile compileOpts -> lilacCompile globalOpts compileOpts
  Tangle tangleOpts -> lilacTangle globalOpts tangleOpts
  Weave weaveOpts -> lilacWeave globalOpts weaveOpts
  WriteGOIDs writeGOIDsOpts -> lilacWriteGOIDs writeGOIDsOpts

lilacCheck :: GlobalOpts -> CheckOpts -> IO ()
lilacCheck globalOpts checkOpts = do
  goids <- LP.runGoidsParser globalOpts.orgIdLocationsFile
  input <- LU.readFrom "" checkOpts.input
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser LP.defaultOrgParserState {goids = goids})
          checkOpts.input
          input
  case parseResult of
    Left errBundle -> do
      die $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      sourceCode <- LT.getSourceCodeMulti orgDoc checkOpts.input goids
      let cycles = LT.findCycles sourceCode
      when (not $ null cycles) $ do
        die $ "Found cycles: " <> show cycles
      putStrLn $ checkOpts.input <> ": OK"
      when (checkOpts.dumpJson) $ do
        outPath <- do
          base <- P.takeBaseName <$> P.encodeUtf checkOpts.input
          let json = P.unsafeEncodeUtf "json"
          P.decodeUtf $ P.addExtension base json
        encodeFile outPath orgDoc
      when (globalOpts.verbose) $ do
        let prettyPrint =
              if globalOpts.color
                then PP.pPrintForceColor
                else PP.pPrintNoColor
        prettyPrint orgDoc

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
      encodeFile outPath obj

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
  goids <- LP.runGoidsParser globalOpts.orgIdLocationsFile
  mapM_ (tangleOrgFile goids) tangleOpts.inputs
  where
    tangleOrgFile goids inputOrgFile = do
      input <- LU.readFrom "" inputOrgFile
      let parseResult =
            MP.runParser
              (runStateT LP.orgDocParser LP.defaultOrgParserState {goids = goids})
              inputOrgFile
              input
      case parseResult of
        Left errBundle -> do
          die $ MP.errorBundlePretty errBundle
        Right (orgDoc, _) -> do
          sourceCode <- LT.getSourceCodeMulti orgDoc inputOrgFile goids
          let cycles = LT.findCycles sourceCode
          when (not $ null cycles) $ do
            die $ "Found cycles: " <> show cycles
          putStrLn $ "Tangling " <> inputOrgFile
          LT.tangle sourceCode
          putStrLn "Finished tangling."

lilacWeave :: GlobalOpts -> WeaveOpts -> IO ()
lilacWeave globalOpts weaveOpts = do
  writeCssFile
  goids <- LP.runGoidsParser globalOpts.orgIdLocationsFile
  mapM_ (weaveOrgFile goids) $ weaveOpts.inputs
  where
    writeCssFile = do
      cssPath <- do
        dir <- P.encodeUtf weaveOpts.outDir
        cssDir <- P.encodeUtf "css"
        css <- P.encodeUtf "default.css"
        pure $ P.joinPath [dir, cssDir, css]
      LU.writeFileRobustly cssPath $ LWC.genCss
    weaveOrgFile goids inputOrgFile = do
      input <- LU.readFrom "" inputOrgFile
      let parseResult =
            MP.runParser
              (runStateT LP.orgDocParser LP.defaultOrgParserState {goids = goids})
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
          LW.weaveToDisk outPath orgDoc weaveOpts.watch goids
          putStrLn "Finished weaving."

lilacWriteGOIDs :: WriteGOIDsOpts -> IO ()
lilacWriteGOIDs writeGOIDsOpts = do
  outFile <- P.encodeUtf $ writeGOIDsOpts.outFile
  inputs <- foldlM readInputs [] writeGOIDsOpts.inputs
  LU.writeFileRobustly outFile $ LP.extractGOIDs inputs
  where
    readInputs acc orgFileStr = do
      orgFileOsPath <- P.encodeUtf orgFileStr
      orgFileContent <- LU.readFileRobustly orgFileOsPath
      pure $ (orgFileOsPath, orgFileContent) : acc
