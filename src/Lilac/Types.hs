module Lilac.Types
  ( Archive (..),
    Block (..),
    BToken (..),
    Cell (..),
    Citation (..),
    Cite (..),
    Figure (..),
    Footnote (..),
    GPath,
    Heading (..),
    Level (..),
    Link (..),
    LinkOid (..),
    LinkTarget (..),
    ListMarker (..),
    MathFragment (..),
    Object (..),
    OID (..),
    OrgDoc (..),
    OrgParser,
    OrgParserConf (..),
    OrgParserState (..),
    PToken (..),
    Prose (..),
    Style (..),
    StyleMask (..),
    StyledText (..),
    Table (..),
    WeaveConf (..),
    decodeGPath,
    defaultOrgParserConf,
    encodeGPath,
    Lilac.Types.fromText,
    fromWords,
    markingChars,
    markingCharsInvisible,
    nilOid,
    nilOidOrgDoc,
    protocolsEmacs,
    protocolsWeb,
  )
where

import Data.Aeson
  ( FromJSON,
    FromJSONKey,
    FromJSONKeyFunction (FromJSONKeyTextParser),
    ToJSON,
    ToJSONKey,
    fromJSONKey,
    parseJSON,
    toJSON,
    toJSONKey,
  )
import Data.Aeson.Types
  ( Parser,
    Value (String),
    toJSONKeyText,
    withText,
  )
import Data.Bits.Bitwise (toListLE)
import Data.Text qualified as T
import Data.UUID.Types (UUID, fromText, fromWords64)
import GHC.Generics (Generic)
import Network.URI qualified as N
import Relude
  ( Bool,
    Char,
    Enum,
    Eq,
    FilePath,
    Identity,
    Int,
    Map,
    Maybe (Just, Nothing),
    MonadFail,
    Ord,
    Set,
    Show,
    StateT,
    Text,
    Type,
    Void,
    Word64,
    Word8,
    catMaybes,
    fail,
    map,
    maybe,
    otherwise,
    pure,
    show,
    toEnum,
    zip,
    ($),
    (.),
    (<>),
    (>),
  )
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP
import Prelude qualified (show)

type BToken :: Type
data BToken
  = BTokenString Text
  | BTokenPadding
  | BTokenLink Link
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Block :: Type
data Block = Block
  { parentOid :: OID,
    meta :: Map Text Text,
    body :: [BToken]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Cell :: Type
data Cell
  = CHeading Heading
  | CListItem [ListMarker]
  | CParagraph Prose
  | CBlock Block
  | CMathFragment MathFragment
  | CFigure Figure
  | CTable Table
  | CFootnote Footnote
  | CBibliography ()
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Figure :: Type
data Figure = Figure
  { meta :: Map Text Text,
    caption :: Prose,
    link :: Link
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Heading :: Type
data Heading = Heading
  { level :: Int,
    section :: [Int],
    body :: Prose,
    meta :: Map Text Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Level :: Type
data Level = Level
  { indent :: Int,
    number :: Maybe Int
  }
  deriving stock (Eq, Show)

type Link :: Type
data Link = Link
  { name :: [StyledText],
    target :: LinkTarget
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type LinkOid :: Type
data LinkOid = LinkOid
  { oid :: OID,
    name :: Text
  }
  deriving stock (Eq, Generic, Ord)

instance ToJSON LinkOid where
  toJSON linkOid = Data.Aeson.Types.String (T.show linkOid)

instance ToJSONKey LinkOid where
  toJSONKey = toJSONKeyText show

instance FromJSON LinkOid where
  parseJSON = withText "LinkOid" parseJSONLinkOid

instance FromJSONKey LinkOid where
  fromJSONKey = FromJSONKeyTextParser parseJSONLinkOid

instance Show LinkOid where
  show (LinkOid (OID uuid) name)
    | T.null name = "id:" <> show uuid
    | otherwise = "id:" <> show uuid <> "::" <> T.unpack name

parseJSONLinkOid :: Text -> Parser LinkOid
parseJSONLinkOid t = do
  oidAndNameText <- maybe (fail "invalid LinkOid") pure (T.stripPrefix "id:" t)
  let oidText = T.take 36 oidAndNameText
      maybeNameText = T.drop 36 oidAndNameText
      name =
        if T.length maybeNameText > 2
          then T.drop 2 maybeNameText
          else ""
  case Lilac.Types.fromText oidText of
    Nothing -> fail "invalid LinkOid"
    Just oid -> pure $ LinkOid oid name

type LinkTarget :: Type
data LinkTarget
  = LinkTargetOid LinkOid
  | LinkTargetFootnote Text
  | LinkTargetURI N.URI
  | LinkTargetFilePath FilePath
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

type ListMarker :: Type
data ListMarker
  = LOrderedStart
  | LOrderedEnd
  | LUnorderedStart
  | LUnorderedEnd
  | LItemStart
  | LItemEnd
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type MathFragment :: Type
data MathFragment = MathFragment
  { meta :: Map Text Text,
    body :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type OrgDoc :: Type
data OrgDoc = OrgDoc
  { oid :: OID,
    cells :: [Cell],
    footnoteNumbers :: Map Text Int,
    cslPath :: Maybe FilePath,
    bibPaths :: [FilePath],
    citations :: [Citation],
    meta :: Map Text Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type OrgParserConf :: Type
data OrgParserConf = OrgParserConf
  { maxHeadingLevel :: Int,
    maxListLevel :: Int
  }
  deriving stock (Eq, Show)

defaultOrgParserConf :: OrgParserConf
defaultOrgParserConf =
  OrgParserConf
    { maxHeadingLevel = 6,
      maxListLevel = 6
    }

type OrgParserState :: Type
data OrgParserState = OrgParserState
  { docOid :: OID,
    headingOidStack :: [Maybe OID],
    headingLevel :: Int,
    headingSection :: [Int],
    insideHeading :: Bool,
    styleMask :: StyleMask,
    insideCodeBlock :: Bool,
    insideLinkDisplayName :: Bool,
    listLevels :: [Level],
    listMarkers :: [ListMarker],
    continuationIndent :: Int,
    equationNumber :: Int,
    anonEquationNumber :: Int,
    cellNames :: Set Text,
    anonBlockNumber :: Int,
    figureNumber :: Int,
    tableNumber :: Int,
    footnoteNumber :: Int,
    footnoteNumbers :: Map Text Int,
    footnoteDefinitions :: Map Text Prose,
    prevCellIsFootnote :: Bool,
    tanglePaths :: Set Text,
    cslPath :: Maybe FilePath,
    bibPaths :: [FilePath],
    insideCitation :: Bool,
    bracketBalance :: Int,
    insideCitePrefix :: Bool,
    citations :: [Citation],
    orgParserConf :: OrgParserConf
  }
  deriving stock (Eq, Show)

type Prose :: Type
newtype Prose = Prose
  { unProse :: [PToken]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type PToken :: Type
data PToken
  = PTokenStyledText StyledText
  | PTokenLink Link
  | PTokenCitation Int
  | PTokenMathFragment MathFragment
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Style :: Type
data Style = Bold | Italic | Underline | Verbatim | Code
  deriving stock (Enum, Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

type StyledText :: Type
data StyledText = StyledText
  { body :: Text,
    style :: StyleMask
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Table :: Type
data Table = Table
  { meta :: Map Text Text,
    caption :: Prose,
    grid :: Map (Int, Int) Prose
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Show StyleMask where
  show (StyleMask a) = "styleMaskFrom " <> x
    where
      x =
        Prelude.show
          . catMaybes
          . map
            ( \(i, b) ->
                if b
                  then (Just $ (toEnum i :: Style))
                  else Nothing
            )
          . zip [0 ..]
          $ toListLE a

type StyleMask :: Type
newtype StyleMask = StyleMask Word8
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Footnote :: Type
data Footnote = Footnote
  { name :: Text,
    body :: Prose
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Citation :: Type
data Citation = Citation
  { styleVariation :: (Maybe Text, Maybe Text),
    cites :: [Cite],
    prefix :: Prose,
    suffix :: Prose
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Cite :: Type
data Cite = Cite
  { id :: Text,
    locator :: Maybe (Text, Text),
    prefix :: Prose,
    suffix :: Prose
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type OID :: Type
newtype OID = OID UUID
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON)

instance Show OID where
  show (OID uuid) = Prelude.show uuid

nilOid :: OID
nilOid = fromWords 0 0

nilOidOrgDoc :: Text
nilOidOrgDoc =
  """
  :PROPERTIES:
  :ID:       00000000-0000-0000-0000-000000000000
  :END:

  """

fromWords :: Word64 -> Word64 -> OID
fromWords w1 w2 = OID $ fromWords64 w1 w2

fromText :: Text -> Maybe OID
fromText t = case Data.UUID.Types.fromText t of
  Just uuid -> Just $ OID uuid
  Nothing -> Nothing

type OrgParser :: Type -> Type
type OrgParser = StateT OrgParserState (MP.ParsecT Void Text Identity)

type GPath :: Type
newtype GPath = GPath FilePath
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (ToJSON)

instance ToJSONKey GPath where
  toJSONKey = toJSONKeyText $ \(GPath p) -> T.pack p

instance FromJSON GPath where
  parseJSON = withText "GPath" $ \t -> do
    pure . GPath $ T.unpack t

instance FromJSONKey GPath where
  fromJSONKey = FromJSONKeyTextParser $ \t -> do
    pure . GPath $ T.unpack t

encodeGPath :: (MonadFail m) => FilePath -> m GPath
encodeGPath fp
  | Just osp <- P.encodeUtf fp,
    P.isValid osp =
      pure $ GPath fp
  | otherwise = fail $ "invalid file path: " <> show fp

decodeGPath :: GPath -> FilePath
decodeGPath (GPath fp) = fp

protocolsEmacs :: [Text]
protocolsEmacs =
  map
    (<> ":")
    [ "file"
    ]

protocolsWeb :: [Text]
protocolsWeb =
  map
    (<> "://")
    [ "http",
      "https",
      "file",
      "ftp",
      "mailto",
      "tel"
    ]

type WeaveConf :: Type
data WeaveConf = WeaveConf
  { archive :: Archive,
    originObjectPath :: GPath,
    watch :: Bool
  }

type Object :: Type
data Object = Object
  { orgDoc :: OrgDoc,
    symbols :: Map LinkOid Cell,
    externalOids :: [LinkOid]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Archive :: Type
data Archive = Archive
  { objects :: Map GPath Object,
    symbols :: Map LinkOid Cell,
    symbolPath :: Map LinkOid GPath
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

markingChars :: [Char]
markingChars = "#*" <> markingCharsInvisible

markingCharsInvisible :: [Char]
markingCharsInvisible = "!^_$/"
