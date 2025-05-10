module Main (main) where

import Data.Map.Strict qualified as M
import Data.UUID.Types (UUID)
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
    Maybe (Just, Nothing),
    Type,
    concat,
    map,
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
import System.Directory (canonicalizePath)
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

type CheckOpts :: Type
data CheckOpts = CheckOpts
  { input :: FilePath
  }

type TangleOpts :: Type
data TangleOpts = TangleOpts
  { input :: FilePath
  }

type WeaveOpts :: Type
data WeaveOpts = WeaveOpts
  { input :: FilePath,
    outDir :: FilePath
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
                <> help "file with Org ID locations; see 'org-id-locations-file' in Emacs"
            )
        )
    <*> ( flag
            True
            False
            ( long "no-color"
                <> help "do not print in color (i.e., suppress ANSI escape codes)"
            )
        )
    <*> ( flag
            False
            True
            ( long "verbose"
                <> help "be more descriptive about what's happening"
            )
        )

subcommandHandler :: OptionHandler Subcommand
subcommandHandler =
  hsubparser
    $ metavar "SUBCOMMAND"
    <> cmdCheck
    <> cmdTangle
    <> cmdWeave
  where
    cmdCheck =
      command
        "check"
        $ info
          (Check <$> checkOptsHandler)
          (progDesc "check input Org file for errors")
    cmdTangle =
      command
        "tangle"
        $ info
          (Tangle <$> tangleOptsHandler)
          (progDesc "generate source code")
    cmdWeave =
      command
        "weave"
        $ info
          (Weave <$> weaveOptsHandler)
          (progDesc "generate JSON representation")

checkOptsHandler :: OptionHandler CheckOpts
checkOptsHandler =
  CheckOpts
    <$> (argument str (metavar "FILE"))

tangleOptsHandler :: OptionHandler TangleOpts
tangleOptsHandler =
  TangleOpts
    <$> (argument str (metavar "FILE"))

weaveOptsHandler :: OptionHandler WeaveOpts
weaveOptsHandler =
  WeaveOpts
    <$> (argument str (metavar "FILE"))
    <*> ( strOption
            ( long "out-dir"
                <> metavar "DIR"
                <> value "."
                <> showDefault
                <> help "directory for saving the orgdoc.json file"
            )
        )

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
        <> header "lilac - literate programming with Org and Noweb-style blocks"
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

runOrgIdsEslipParser :: GlobalOpts -> IO (M.Map UUID P.OsPath)
runOrgIdsEslipParser globalOpts = do
  let path = case P.encodeUtf globalOpts.orgIdLocationsFile of
        Nothing -> P.pack []
        Just p -> p
  input <- LU.readFileRobustly path
  let parseResult =
        MP.runParser
          (runStateT (LP.orgIdsElispParser path) LP.defaultOrgIdsElispParserState)
          globalOpts.orgIdLocationsFile
          input
  case parseResult of
    Left errBundle -> do
      die $ MP.errorBundlePretty errBundle
    Right (ids, _) -> pure ids

lilacCheck :: GlobalOpts -> CheckOpts -> IO ()
lilacCheck globalOpts checkOpts = do
  input <- LU.readFrom "" checkOpts.input
  canonicalPath <- canonicalizePath checkOpts.input
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser LP.defaultOrgParserState {LP.sourcePath = canonicalPath})
          checkOpts.input
          input
  case parseResult of
    Left errBundle -> do
      die $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      let cycles = LT.findCycles $ LT.getSourceCode orgDoc checkOpts.input
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
  ids <- runOrgIdsEslipParser globalOpts
  input <- LU.readFrom "" tangleOpts.input
  canonicalPath <- canonicalizePath tangleOpts.input
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser LP.defaultOrgParserState {LP.sourcePath = canonicalPath})
          tangleOpts.input
          input
  case parseResult of
    Left errBundle -> do
      die $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      let sourceCode = LT.getSourceCode orgDoc tangleOpts.input
          cycles = LT.findCycles sourceCode
      when (not $ null cycles) $ do
        die $ "Found cycles: " <> show cycles
      putStrLn $ "Tangling " <> tangleOpts.input
      LT.tangle sourceCode
      putStrLn "Finished tangling."

lilacWeave :: WeaveOpts -> IO ()
lilacWeave weaveOpts = do
  input <- LU.readFrom "" weaveOpts.input
  canonicalPath <- canonicalizePath weaveOpts.input
  let parseResult =
        MP.runParser
          (runStateT LP.orgDocParser LP.defaultOrgParserState {LP.sourcePath = canonicalPath})
          weaveOpts.input
          input
  case parseResult of
    Left errBundle -> do
      die $ MP.errorBundlePretty errBundle
    Right (orgDoc, _) -> do
      putStrLn $ "Weaving " <> weaveOpts.input
      let outPath =
            P.joinPath
              $ map P.unsafeEncodeUtf [weaveOpts.outDir, "orgdoc.json"]
      outPath' <- P.decodeUtf outPath
      LW.weaveToDisk outPath' orgDoc
      putStrLn "Finished weaving."
