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
    ProjectConf (..),
    PseudoEdge (..),
    RawProjectConf (..),
    Prose (..),
    Style (..),
    StyleMask (..),
    StyledText (..),
    Table (..),
    WeaveConf (..),
    cellIdxToName,
    cellIdxToObjCell,
    decodeGPath,
    defaultOrgParserConf,
    defaultOrgParserState,
    encodeGPath,
    Lilac.Types.fromText,
    fromWords,
    getProseContents,
    getSectionText,
    markingChars,
    markingCharsInvisible,
    nilOid,
    nilOidOrgDoc,
    protocolsEmacs,
    protocolsWeb,
    ptext,
    stext,
    styleMaskFrom,
  )
where

import Citeproc qualified as CP
import Citeproc.CslJson (CslJson)
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
import Data.Bits (setBit)
import Data.Bits.Bitwise (toListLE)
import Data.Graph qualified as G
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID.Types (UUID, fromText, fromWords64)
import GHC.Generics (Generic)
import Network.URI qualified as N
import Relude
  ( Bool (False),
    Char,
    Enum,
    Eq,
    FilePath,
    Identity,
    Int,
    Map,
    Maybe (Just, Nothing),
    MonadFail,
    Monoid,
    Ord,
    Semigroup,
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
    foldl',
    fromEnum,
    map,
    maybe,
    mempty,
    otherwise,
    pure,
    reverse,
    show,
    toEnum,
    zip,
    (!!?),
    ($),
    (-),
    (.),
    (<>),
    (>),
  )
import Relude.Extra.Map (lookup)
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP
import Prelude qualified (show)

type BToken :: Type
data BToken
  = BTokenString Text
  | BTokenPadding
  | BTokenLink Link
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Block :: Type
data Block = Block
  { parentOid :: OID,
    meta :: Map Text Text,
    body :: [BToken]
  }
  deriving stock (Eq, Ord, Generic, Show)
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
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Figure :: Type
data Figure = Figure
  { parentOid :: OID,
    meta :: Map Text Text,
    caption :: Prose,
    link :: Link
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Heading :: Type
data Heading = Heading
  { level :: Int,
    section :: [Int],
    body :: Prose,
    meta :: Map Text Text
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getProseContents :: Prose -> Text
getProseContents prose = join $ foldl' f [] prose.unProse
  where
    join = T.intercalate "" . reverse
    f acc = \case
      PTokenStyledText styledText -> styledText.body : acc
      PTokenLink link ->
        let linkText =
              join
                $ foldl'
                  (\acc' n -> n.body : acc')
                  []
                  link.name
         in linkText : acc
      PTokenMathFragment mathFragment -> mathFragment.body : acc
      PTokenCitation {}
      PTokenDivStart {}
      PTokenDivEnd {} -> acc

getSectionText :: Heading -> Text
getSectionText heading =
  T.intercalate "."
    . map T.show
    . reverse
    $ heading.section

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
  deriving stock (Eq, Generic, Ord, Show)
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
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type MathFragment :: Type
data MathFragment = MathFragment
  { parentOid :: OID,
    meta :: Map Text Text,
    body :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

type OrgDoc :: Type
data OrgDoc = OrgDoc
  { oid :: OID,
    cells :: [Cell],
    pseudoEdges :: [PseudoEdge],
    footnoteNumbers :: Map Text Int,
    cslPath :: Maybe FilePath,
    bibPaths :: [FilePath],
    citations :: [Citation],
    meta :: Map Text Text
  }
  deriving stock (Eq, Ord, Generic, Show)
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
    { maxHeadingLevel = 3,
      maxListLevel = 3
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
    cellNumber :: Int,
    pseudoEdges :: [PseudoEdge],
    anonBlockNumber :: Int,
    figureNumber :: Int,
    tableNumber :: Int,
    footnoteNumber :: Int,
    footnoteNumbers :: Map Text Int,
    footnoteDefinitions :: Map Text Prose,
    insideFootnoteDefinition :: Maybe Text,
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

defaultOrgParserState :: OrgParserState
defaultOrgParserState =
  OrgParserState
    { docOid = nilOid,
      headingOidStack = [],
      headingLevel = 0,
      headingSection = [],
      insideHeading = False,
      styleMask = styleMaskFrom [],
      insideCodeBlock = False,
      insideLinkDisplayName = False,
      listLevels = [],
      listMarkers = [],
      continuationIndent = 0,
      equationNumber = 0,
      anonEquationNumber = 0,
      cellNames = Set.empty,
      cellNumber = 0,
      pseudoEdges = [],
      anonBlockNumber = 0,
      figureNumber = 0,
      tableNumber = 0,
      footnoteNumber = 0,
      footnoteNumbers = M.empty,
      footnoteDefinitions = M.empty,
      insideFootnoteDefinition = Nothing,
      prevCellIsFootnote = False,
      tanglePaths = Set.empty,
      cslPath = Nothing,
      bibPaths = [],
      insideCitation = False,
      bracketBalance = 0,
      insideCitePrefix = False,
      citations = [],
      orgParserConf = defaultOrgParserConf
    }

type Prose :: Type
newtype Prose = Prose
  { unProse :: [PToken]
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup Prose where
  x <> y = Prose $ x.unProse <> y.unProse

instance Monoid Prose where
  mempty = Prose []

type PToken :: Type
data PToken
  = PTokenStyledText StyledText
  | PTokenLink Link
  | PTokenCitation Int
  | PTokenMathFragment MathFragment
  | PTokenDivStart Text
  | PTokenDivEnd
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

type Style :: Type
data Style
  = Bold
  | Italic
  | Underline
  | Verbatim
  | Code
  | Smallcaps
  | Subscript
  | Superscript
  deriving stock (Enum, Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

type StyledText :: Type
data StyledText = StyledText
  { body :: Text,
    style :: StyleMask
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

stext :: Text -> [Style] -> StyledText
stext body styles = StyledText {body = body, style = styleMaskFrom styles}

ptext :: Text -> [Style] -> PToken
ptext body styles = PTokenStyledText $ stext body styles

type Table :: Type
data Table = Table
  { parentOid :: OID,
    meta :: Map Text Text,
    caption :: Prose,
    grid :: Map (Int, Int) Prose
  }
  deriving stock (Eq, Ord, Generic, Show)
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
  deriving stock (Eq, Generic, Ord)
  deriving anyclass (FromJSON, ToJSON)

styleMaskFrom :: [Style] -> StyleMask
styleMaskFrom ss = StyleMask $ foldl' f (0 :: Word8) ss
  where
    f :: Word8 -> Style -> Word8
    f acc s = setBit acc (fromEnum s)

type Footnote :: Type
data Footnote = Footnote
  { name :: Text,
    body :: Prose
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Citation :: Type
data Citation = Citation
  { styleVariation :: (Maybe Text, Maybe Text),
    cites :: [Cite],
    noteNumber :: Maybe Int,
    prefix :: Prose,
    suffix :: Prose
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Cite :: Type
data Cite = Cite
  { id :: Text,
    locator :: Maybe (Text, Text),
    prefix :: Prose,
    suffix :: Prose
  }
  deriving stock (Eq, Ord, Generic, Show)
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

type PseudoEdge :: Type
newtype PseudoEdge = PseudoEdge (Int, LinkOid)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

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
  { projectConf :: ProjectConf,
    archive :: Archive,
    originObjectPath :: GPath,
    citeprocResult :: CP.Result Prose,
    watch :: Bool
  }

type Object :: Type
data Object = Object
  { orgDoc :: OrgDoc,
    symbols :: Map LinkOid Int,
    externalOids :: [LinkOid]
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Archive :: Type
data Archive = Archive
  { objects :: Map GPath Object,
    symbols :: Map LinkOid Int,
    backlinks :: Map GPath (Map GPath [G.Edge]),
    offsets :: IntMap GPath,
    csls :: Map FilePath Text,
    bibs :: Map FilePath [CP.Reference (CslJson Text)]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

cellIdxToObjCell :: Archive -> Int -> Maybe (Object, Cell)
cellIdxToObjCell archive globalCellIdx
  | Just (offset, path) <- IntMap.lookupLE globalCellIdx archive.offsets,
    Just obj <- lookup path archive.objects,
    Just cell <- obj.orgDoc.cells !!? (globalCellIdx - offset) =
      Just (obj, cell)
  | otherwise = Nothing

cellIdxToName :: Archive -> Int -> Int -> Maybe Text
cellIdxToName archive globalCellIdx offset
  | Just (_, cell) <- cellIdxToObjCell archive globalCellIdx =
      case cell of
        CHeading heading ->
          Just $ (getSectionText heading) <> " " <> (getProseContents heading.body)
        CParagraph _ -> Just $ "Paragraph" <> localCellIdx
        CFigure figure
          | Just name <- lookup "name" figure.meta ->
              Just name
          | Just figureNum <- lookup "__figure_number" figure.meta ->
              Just $ "Figure " <> figureNum
          | otherwise -> Just "Figure"
        CTable table
          | Just name <- lookup "name" table.meta ->
              Just name
          | Just tableNum <- lookup "__table_number" table.meta ->
              Just $ "Table " <> tableNum
          | otherwise -> Just "Table"
        CFootnote _ -> Just $ "Footnote" <> localCellIdx
        CBlock block
          | Just name <- lookup "name" block.meta ->
              Just name
          | otherwise -> Just $ "Anonymous Block" <> localCellIdx
        CMathFragment mathFragment
          | Just name <- lookup "name" mathFragment.meta ->
              Just name
          | Just eqNum <- lookup "__equation_number" mathFragment.meta ->
              Just $ "Equation " <> eqNum
          | Just eqNum <- lookup "__equation_anon_number" mathFragment.meta ->
              Just $ "Anonymous Equation " <> eqNum
          | otherwise -> Just "Math Fragment"
        CListItem {}
        CBibliography {} -> Nothing
  | otherwise = Nothing
  where
    localCellIdx = " (Cell " <> (T.show $ globalCellIdx - offset) <> ")"

type ProjectConf :: Type
data ProjectConf = ProjectConf
  { name :: Text,
    homepage :: Maybe N.URI,
    repo :: Maybe N.URI,
    version :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type RawProjectConf :: Type
data RawProjectConf = RawProjectConf
  { name :: Text,
    homepage :: Text,
    repo :: Text,
    version :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

markingChars :: [Char]
markingChars = "#*" <> markingCharsInvisible

markingCharsInvisible :: [Char]
markingCharsInvisible = "!^_$/"
