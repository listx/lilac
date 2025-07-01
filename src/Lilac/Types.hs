module Lilac.Types
  ( Block (..),
    BToken (..),
    Cell (..),
    Figure (..),
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
    defaultOrgParserConf,
    Lilac.Types.fromText,
    fromWords,
    nilOid,
    nilOidOrgDoc,
    protocolsEmacs,
    protocolsWeb,
  )
where

import Data.Aeson (ToJSON)
import Data.Bits.Bitwise (toListLE)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID.Types (UUID, fromText, fromWords64)
import GHC.Generics (Generic)
import Network.URI qualified as N
import Relude
  ( Enum,
    Eq,
    Identity,
    Int,
    Maybe (Just, Nothing),
    Ord,
    Show,
    StateT,
    Type,
    Void,
    Word64,
    Word8,
    catMaybes,
    map,
    toEnum,
    zip,
    ($),
    (.),
    (<>),
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

instance ToJSON BToken

type Block :: Type
data Block = Block
  { parentOid :: OID,
    meta :: M.Map T.Text T.Text,
    body :: [BToken]
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON Block

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

instance ToJSON Cell

type Figure :: Type
data Figure = Figure
  { meta :: M.Map T.Text T.Text,
    link :: Link
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON Figure

type Heading :: Type
data Heading = Heading
  { level :: Int,
    section :: [Int],
    body :: Prose,
    meta :: M.Map T.Text T.Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON Heading

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

instance ToJSON Link

type LinkOid :: Type
data LinkOid = LinkOid
  { oid :: OID,
    name :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Show)

instance ToJSON LinkOid

type LinkTarget :: Type
data LinkTarget
  = LinkTargetOid LinkOid
  | LinkTargetURI N.URI
  | LinkTargetOther T.Text
  deriving stock (Eq, Generic, Ord, Show)

instance ToJSON LinkTarget

type ListMarker :: Type
data ListMarker
  = LOrderedStart
  | LOrderedEnd
  | LUnorderedStart
  | LUnorderedEnd
  | LItemStart
  | LItemEnd
  deriving stock (Eq, Generic, Show)

instance ToJSON ListMarker

type MathFragment :: Type
data MathFragment = MathFragment
  { meta :: M.Map T.Text T.Text,
    body :: T.Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON MathFragment

type OrgDoc :: Type
data OrgDoc = OrgDoc
  { oid :: OID,
    cells :: [Cell],
    meta :: M.Map T.Text T.Text
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON OrgDoc

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
    listLevels :: [Level],
    listMarkers :: [ListMarker],
    continuationIndent :: Int,
    equationNumber :: Int,
    blockNames :: Set.Set T.Text,
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

instance ToJSON PToken

type Style :: Type
data Style = Bold | Italic | Underline | Verbatim | Code
  deriving stock (Enum, Eq, Generic, Ord, Show)

instance ToJSON Style

type StyledText :: Type
data StyledText = StyledText
  { body :: T.Text,
    style :: StyleMask
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON StyledText

type Table :: Type
data Table = Table
  { meta :: M.Map T.Text T.Text,
    grid :: M.Map (Int, Int) Prose
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON Table

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

instance ToJSON StyleMask

type OID :: Type
newtype OID = OID UUID
  deriving stock (Eq, Ord, Generic)

instance ToJSON OID

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
