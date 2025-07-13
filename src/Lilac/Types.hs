module Lilac.Types
  ( Block (..),
    BToken (..),
    Cell (..),
    Figure (..),
    GPath,
    Heading (..),
    Level (..),
    Link (..),
    LinkOid (..),
    LinkTarget (..),
    ListMarker (..),
    MathFragment (..),
    OID (..),
    OrgDoc (..),
    OrgParser,
    OrgParserConf (..),
    OrgParserState (..),
    PToken (..),
    Prose,
    Style (..),
    StyleMask (..),
    StyledText (..),
    Table (..),
    decodeGPath,
    defaultOrgParserConf,
    encodeGPath,
    Lilac.Types.fromText,
    fromWords,
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
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID.Types (UUID, fromText, fromWords64)
import GHC.Generics (Generic)
import Network.URI qualified as N
import Relude
  ( Bool,
    Enum,
    Eq,
    FilePath,
    Identity,
    Int,
    Maybe (Just, Nothing),
    MonadFail,
    Ord,
    Show,
    StateT,
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
  = BTokenString T.Text
  | BTokenPadding
  | BTokenLink Link
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Block :: Type
data Block = Block
  { parentOid :: OID,
    meta :: M.Map T.Text T.Text,
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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Figure :: Type
data Figure = Figure
  { meta :: M.Map T.Text T.Text,
    link :: Link
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Heading :: Type
data Heading = Heading
  { level :: Int,
    section :: [Int],
    body :: Prose,
    meta :: M.Map T.Text T.Text
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
    name :: T.Text
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

parseJSONLinkOid :: T.Text -> Parser LinkOid
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
  | LinkTargetURI N.URI
  | LinkTargetOther T.Text T.Text
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
  { meta :: M.Map T.Text T.Text,
    body :: T.Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type OrgDoc :: Type
data OrgDoc = OrgDoc
  { oid :: OID,
    cells :: [Cell],
    meta :: M.Map T.Text T.Text
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
  { goids :: M.Map OID P.OsPath,
    docOid :: OID,
    headingOidStack :: [Maybe OID],
    headingLevel :: Int,
    headingSection :: [Int],
    styleMask :: StyleMask,
    insideCodeBlock :: Bool,
    insideLinkDisplayName :: Bool,
    listLevels :: [Level],
    listMarkers :: [ListMarker],
    continuationIndent :: Int,
    equationNumber :: Int,
    cellNames :: Set.Set T.Text,
    anonBlockNumber :: Int,
    tanglePaths :: Set.Set T.Text,
    orgParserConf :: OrgParserConf
  }
  deriving stock (Eq, Show)

type PToken :: Type
data PToken
  = PTokenStyledText StyledText
  | PTokenLink Link
  | PTokenMathFragment MathFragment
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Style :: Type
data Style = Bold | Italic | Underline | Verbatim | Code
  deriving stock (Enum, Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

type StyledText :: Type
data StyledText = StyledText
  { body :: T.Text,
    style :: StyleMask
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Table :: Type
data Table = Table
  { meta :: M.Map T.Text T.Text,
    grid :: M.Map (Int, Int) Prose
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

type OID :: Type
newtype OID = OID UUID
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON)

instance Show OID where
  show (OID uuid) = Prelude.show uuid

nilOid :: OID
nilOid = fromWords 0 0

nilOidOrgDoc :: T.Text
nilOidOrgDoc =
  """
  :PROPERTIES:
  :ID:       00000000-0000-0000-0000-000000000000
  :END:

  """

fromWords :: Word64 -> Word64 -> OID
fromWords w1 w2 = OID $ fromWords64 w1 w2

fromText :: T.Text -> Maybe OID
fromText t = case Data.UUID.Types.fromText t of
  Just uuid -> Just $ OID uuid
  Nothing -> Nothing

type OrgParser :: Type -> Type
type OrgParser = StateT OrgParserState (MP.ParsecT Void T.Text Identity)

type Prose :: Type
type Prose = [PToken]

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

protocolsEmacs :: [T.Text]
protocolsEmacs =
  map
    (<> ":")
    [ "file"
    ]

protocolsWeb :: [T.Text]
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
