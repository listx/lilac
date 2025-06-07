module Main (main) where

import Data.Version (showVersion)
import Lilac.Parse qualified as LP
import Lilac.Tangle qualified as LT
import Lilac.Util qualified as LU
import Lilac.Version (lilacVersion)
import Lilac.Weave qualified as LW
import Options.Applicative
  ( Parser,
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
    Type,
    concat,
    foldlM,
    mapM_,
    not,
    null,
    pure,
    putStrLn,
    runStateT,
    show,
    when,
    ($),
    (<$>),
    (<>),
  )
import System.Exit (die)
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP
import Text.Pretty.Simple qualified as PP

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
  | WriteGOIDs WriteGOIDsOpts

type CheckOpts :: Type
data CheckOpts = CheckOpts
  { input :: FilePath
  }

type TangleOpts :: Type
data TangleOpts = TangleOpts
  { inputs :: [FilePath]
  }

type WeaveOpts :: Type
data WeaveOpts = WeaveOpts
  { inputs :: [FilePath],
    outDir :: FilePath
  }

type WriteGOIDsOpts :: Type
data WriteGOIDsOpts = WriteGOIDsOpts
  { outFile :: FilePath,
    inputs :: [FilePath]
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
    <> cmdCheck
    <> cmdTangle
    <> cmdWeave
    <> cmdWriteGOIDs
  where
    cmdCheck =
      command
        "check"
        $ info
          (Check <$> checkOptsHandler)
          (progDesc "Check input Org file for errors.")
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
  Check checkOpts -> lilacCheck globalOpts checkOpts
  Tangle tangleOpts -> lilacTangle globalOpts tangleOpts
  Weave weaveOpts -> lilacWeave weaveOpts
  WriteGOIDs writeGOIDsOpts -> lilacWriteGOIDs writeGOIDsOpts

lilacCheck :: GlobalOpts -> CheckOpts -> IO ()
lilacCheck globalOpts checkOpts = do
  goids <- LP.runGoidsParser globalOpts.orgIdLocationsFile
  input <- LU.readFrom "" checkOpts.input
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser LP.defaultOrgParserState {LP.goids = goids})
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
      when (globalOpts.verbose) $ do
        let prettyPrint =
              if globalOpts.color
                then PP.pPrintForceColor
                else PP.pPrintNoColor
        prettyPrint orgDoc

lilacTangle :: GlobalOpts -> TangleOpts -> IO ()
lilacTangle globalOpts tangleOpts = do
  goids <- LP.runGoidsParser globalOpts.orgIdLocationsFile
  mapM_ (tangleOrgFile goids) tangleOpts.inputs
  where
    tangleOrgFile goids inputOrgFile = do
      input <- LU.readFrom "" inputOrgFile
      let parseResult =
            MP.runParser
              (runStateT LP.orgDocParser LP.defaultOrgParserState {LP.goids = goids})
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

lilacWeave :: WeaveOpts -> IO ()
lilacWeave weaveOpts = do
  mapM_ weaveOrgFile $ weaveOpts.inputs
  where
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
          LW.weaveToDisk outPath orgDoc
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
