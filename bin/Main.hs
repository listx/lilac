module Main (main) where

import Data.Version (showVersion)
import Lilac.Parse qualified as L
import Lilac.Version (lilacVersion)
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
    showHelpOnEmpty,
    str,
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
    decodeUtf8,
    pure,
    putStrLn,
    readFileBS,
    runStateT,
    when,
    ($),
    (<$>),
    (<>),
  )
import Text.Megaparsec qualified as MP
import Text.Pretty.Simple qualified as PP

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) mainOptions
  lilac opts
  where
    mainOptions =
      info
        (helper <*> versionOption <*> programOptions)
        programDescription
    programDescription :: InfoMod a
    programDescription =
      fullDesc
        <> header "lilac - literate programming with Org and Noweb-style blocks"
    versionOption :: OptionHandler (a -> a)
    versionOption =
      let cabalVersion = version
       in infoOption
            (concat [showVersion cabalVersion, "-", $(lilacVersion)])
            (long "version" <> short 'v' <> help "Show version")

lilac :: GlobalOpts -> IO ()
lilac globalOpts = case globalOpts.subcommand of
  Check checkOpts -> lilacCheck globalOpts checkOpts
  Tangle -> putStrLn "'tangle' is not implemented yet"
  Weave -> putStrLn "'weave' is not implemented yet"

lilacCheck :: GlobalOpts -> CheckOpts -> IO ()
lilacCheck globalOpts checkOpts = do
  bytes <- readFileBS checkOpts.input
  let parseResult =
        MP.runParser
          (runStateT L.orgDocParser L.defaultOrgParserState)
          checkOpts.input
          (decodeUtf8 bytes)
  case parseResult of
    Left errBundle -> putStrLn $ MP.errorBundlePretty errBundle
    Right orgDoc -> do
      putStrLn $ checkOpts.input <> ": OK"
      when (globalOpts.verbose) $ do
        let prettyPrint =
              if globalOpts.color
                then PP.pPrintForceColor
                else PP.pPrintNoColor
        prettyPrint orgDoc

type GlobalOpts :: Type
data GlobalOpts = GlobalOpts
  { subcommand :: Subcommand,
    color :: Bool,
    verbose :: Bool
  }

type Subcommand :: Type
data Subcommand
  = Check CheckOpts
  | Tangle
  | Weave

type CheckOpts :: Type
data CheckOpts = CheckOpts
  { input :: FilePath
  }

type OptionHandler :: Type -> Type
type OptionHandler = Options.Applicative.Parser

programOptions :: OptionHandler GlobalOpts
programOptions =
  GlobalOpts
    <$> subcommandHandler
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
          (pure Tangle)
          (progDesc "generate source code")
    cmdWeave =
      command
        "weave"
        $ info
          (pure Weave)
          (progDesc "generate documentation")

checkOptsHandler :: OptionHandler CheckOpts
checkOptsHandler =
  CheckOpts
    <$> (argument str (metavar "FILE"))
