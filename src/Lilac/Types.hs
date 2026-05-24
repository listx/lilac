module Lilac.Types
  ( Archive (..),
    ArchiveOpts (..),
    Block (..),
    BToken (..),
    Cell (..),
    Citation (..),
    Cite (..),
    Command (..),
    CompileOpts (..),
    Figure (..),
    Footnote (..),
    GCI (..),
    GlobalOpts (..),
    GPath,
    GTOC (..),
    GlossaryTerm (..),
    Heading (..),
    LCI (..),
    Level (..),
    Link (..),
    LinkOid (..),
    LinkTarget (..),
    LintOpts (..),
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
    ServeOpts (..),
    Subcommand (..),
    Style (..),
    StyleMask (..),
    StyledText (..),
    Table (..),
    TangleOpts (..),
    WeaveConf (..),
    WeaveOpts (..),
    decodeGPath,
    defaultOrgParserConf,
    defaultOrgParserState,
    emptyGPath,
    emptyProjectConf,
    encodeGPath,
    Lilac.Types.fromText,
    fromWords,
    gciToObjCell,
    gciToObjPath,
    getCellName,
    getProseContents,
    getSectionNumber,
    markingChars,
    markingCharsInvisible,
    nilOid,
    nilOidOrgDocPrefix,
    protocolsEmacs,
    protocolsWeb,
    ptext,
    stext,
    stripLinks,
    stripStyles,
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
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Types
  ( Parser,
    Value (String),
    toJSONKeyText,
    withText,
  )
import Data.Bits (setBit)
import Data.Bits.Bitwise (toListLE)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tree (Tree)
import Data.UUID.Types (UUID, fromText, fromWords64)
import GHC.Generics (Generic)
import Lilac.Types.Internal (GPath (GPath))
import Network.URI qualified as N
import Relude
  ( Bool (False),
    Char,
    Enum,
    Eq,
    FilePath,
    Hashable,
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
    abs,
    catMaybes,
    concatMap,
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
    sort,
    toEnum,
    zip,
    (!!?),
    ($),
    (-),
    (.),
    (<),
    (<$>),
    (<*>),
    (<>),
    (>),
  )
import Relude.Extra.Map (keys, lookup)
import System.OsPath (OsPath)
import System.OsPath qualified as P
import Text.Megaparsec qualified as MP
import Prelude qualified (show)

type BToken :: Type
data BToken
  = BTokenString Text
  | BTokenPadding
  | BTokenLinkTarget LinkTarget
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Block :: Type
data Block = Block
  { parentOid :: OID, --
    meta :: Map Text Text, --
    body :: [BToken] --
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
  | CGlossaryTerm GlossaryTerm
  | CCommand Command
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Command :: Type
data Command
  = PrintBibliography
  | PrintGlossary Bool
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Figure :: Type
data Figure = Figure
  { parentOid :: OID, --
    meta :: Map Text Text, --
    caption :: Prose, --
    link :: Link --
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Heading :: Type
data Heading = Heading
  { level :: Int,
    section :: [Int],
    body :: Prose,
    meta :: Map Text Text --
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

getSectionNumber :: Heading -> Text
getSectionNumber heading =
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
    name :: Text --
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
  | LinkTargetLineLabel LinkOid
  | LinkTargetFootnote Text
  | LinkTargetURI N.URI
  | LinkTargetFilePath FilePath --
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
  { oid :: OID, --
    meta :: Map Text Text, --
    cells :: [Cell], --
    pseudoEdges :: [PseudoEdge], --
    footnoteRefs :: Map Text (Int, LCI), --
    glossary :: Map Text LCI, --
    lineLabels :: Map Text Int, --
    cslPath :: Maybe FilePath, --
    bibPaths :: [FilePath], --
    citations :: [Citation] --
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON) --

type LCI :: Type
newtype LCI = LCI
  { unLCI :: Int
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON)

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
  { docOid :: OID, --
    headingOidStack :: [Maybe OID], --
    headingLevel :: Int, --
    headingSectionNumber :: [Int], --
    insideHeading :: Bool, --
    cellNames :: Set Text, --
    cellNumber :: LCI, --
    pseudoEdges :: [PseudoEdge], --
    tanglePaths :: Set Text, --
    insideCodeBlock :: Bool, --
    anonBlockNumber :: Int, --
    lineLabelNumber :: Int, --
    lineLabels :: Map Text Int, --
    styleMask :: StyleMask, --
    insideLinkDisplayName :: Bool, --
    insideParagraph :: Bool, --
    listLevels :: [Level], --
    listMarkers :: [ListMarker], --
    continuationIndent :: Int, --
    footnoteNumber :: Int, --
    footnoteRefs :: Map Text (Int, LCI), --
    footnoteDefinitions :: Map Text Prose, --
    insideFootnoteDefinition :: Maybe Text, --
    prevCellIsFootnote :: Bool, --
    glossary :: Map Text LCI, --
    glossaryTermOffset :: Int, --
    cslPath :: Maybe FilePath, --
    bibPaths :: [FilePath], --
    insideCitation :: Bool, --
    insideCitePrefix :: Bool, --
    bracketBalance :: Int, --
    citations :: [Citation], --
    equationNumber :: Int, --
    anonEquationNumber :: Int, --
    figureNumber :: Int, --
    tableNumber :: Int, --
    orgParserConf :: OrgParserConf --
  }
  deriving stock (Eq, Show)

defaultOrgParserState :: OrgParserState
defaultOrgParserState =
  OrgParserState
    { docOid = nilOid,
      headingOidStack = [],
      headingLevel = 0,
      headingSectionNumber = [],
      insideHeading = False,
      cellNames = Set.empty,
      cellNumber = LCI 0,
      pseudoEdges = [],
      tanglePaths = Set.empty,
      insideCodeBlock = False,
      anonBlockNumber = 0,
      lineLabelNumber = 0,
      lineLabels = M.empty,
      styleMask = styleMaskFrom [],
      insideLinkDisplayName = False,
      insideParagraph = False,
      listLevels = [],
      listMarkers = [],
      continuationIndent = 0,
      footnoteNumber = 0,
      footnoteRefs = M.empty,
      footnoteDefinitions = M.empty,
      insideFootnoteDefinition = Nothing,
      prevCellIsFootnote = False,
      glossary = M.empty,
      glossaryTermOffset = 0,
      cslPath = Nothing,
      bibPaths = [],
      insideCitation = False,
      insideCitePrefix = False,
      bracketBalance = 0,
      citations = [],
      equationNumber = 0,
      anonEquationNumber = 0,
      figureNumber = 0,
      tableNumber = 0,
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
  { parentOid :: OID, --
    meta :: Map Text Text,
    caption :: Prose, --
    grid :: Map (Int, Int) Prose --
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

type GlossaryTerm :: Type
data GlossaryTerm = GlossaryTerm
  { term :: Prose,
    definition :: Prose
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
  { id :: Text, --
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

nilOidOrgDocPrefix :: Text
nilOidOrgDocPrefix =
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
newtype PseudoEdge = PseudoEdge (LCI, LinkOid)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type OrgParser :: Type -> Type
type OrgParser = StateT OrgParserState (MP.ParsecT Void Text Identity)

encodeGPath :: (MonadFail m) => FilePath -> m GPath
encodeGPath fp
  | Just osp <- P.encodeUtf fp,
    P.isValid osp =
      pure $ GPath fp
  | otherwise = fail $ "invalid file path: " <> show fp

decodeGPath :: GPath -> FilePath
decodeGPath (GPath fp) = fp

emptyGPath :: GPath
emptyGPath = GPath ""

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
    originObjectPath :: GPath, --
    outDir :: GPath,
    rootRelativePrefix :: GPath,
    citeprocResult :: CP.Result Prose
  }

type Object :: Type
data Object = Object
  { orgDoc :: OrgDoc,
    symbols :: Map LinkOid LCI, --
    externalOids :: [LinkOid] --
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type Archive :: Type
data Archive = Archive
  { objects :: Map GPath Object, --
    symbols :: Map LinkOid GCI, --
    ancestry :: Map GCI [GCI], --
    glossary :: Map (Text, GPath) LCI, --
    backlinks :: Map GPath (Map GPath [(GCI, GCI)]), --
    offsets :: IntMap GPath, --
    gtoc :: GTOC, --
    csls :: Map FilePath Text, --
    bibs :: Map FilePath [CP.Reference (CslJson Text)] --
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type GCI :: Type
newtype GCI = GCI
  { unGCI :: Int
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, FromJSONKey, Hashable, ToJSON, ToJSONKey)

gciToObjCell :: Archive -> GCI -> Maybe (Object, Cell)
gciToObjCell archive gci
  | Just (offset, path) <- IntMap.lookupLE gci.unGCI archive.offsets,
    Just obj <- lookup path archive.objects,
    Just cell <- obj.orgDoc.cells !!? (gci.unGCI - offset) =
      Just (obj, cell)
  | otherwise = Nothing

gciToObjPath :: Archive -> GCI -> Maybe GPath
gciToObjPath archive gci
  | gci.unGCI < 0 =
      (sort $ keys archive.objects) !!? (abs gci.unGCI - 1)
  | Just (_, path) <- IntMap.lookupLE gci.unGCI archive.offsets =
      Just path
  | otherwise = Nothing

stripLinks :: Prose -> Prose
stripLinks (Prose pTokens) = Prose (concatMap stripLink pTokens)
  where
    stripLink :: PToken -> [PToken]
    stripLink = \case
      PTokenLink link -> map (PTokenStyledText $) link.name
      pToken -> [pToken]

stripStyles :: Prose -> Prose
stripStyles (Prose pTokens) = Prose (concatMap stripStyle pTokens)
  where
    stripStyle :: PToken -> [PToken]
    stripStyle = \case
      PTokenStyledText styledText -> [ptext styledText.body []]
      pToken -> [pToken]

getCellName :: Cell -> Maybe Prose
getCellName = \case
  CFigure figure
    | Just name <- lookup "name" figure.meta -> verbatim name
    | otherwise -> Nothing
  CTable table
    | Just name <- lookup "name" table.meta -> verbatim name
    | otherwise -> Nothing
  CFootnote footnote ->
    Just
      $ Prose
        [ ptext "Footnote (" [],
          ptext ("fn:" <> footnote.name) [Verbatim],
          ptext ")" []
        ]
  CBlock block
    | Just name <- lookup "name" block.meta ->
        verbatim name
    | otherwise -> Nothing
  CMathFragment mathFragment
    | Just name <- lookup "name" mathFragment.meta ->
        verbatim name
    | otherwise -> Nothing
  CHeading {}
  CParagraph {}
  CGlossaryTerm {}
  CListItem {}
  CCommand {} -> Nothing
  where
    verbatim body = Just $ Prose [ptext body [Verbatim]]

type GTOC :: Type
newtype GTOC = GTOC
  { unGTOC :: Tree GPath
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, FromJSONKey, Hashable, ToJSON, ToJSONKey)

type ProjectConf :: Type
data ProjectConf = ProjectConf
  { name :: Text, --
    homepage :: Maybe N.URI, --
    repo :: Maybe N.URI, --
    version :: Maybe Text, --
    nonTangledPathspecs :: [Text], --
    bibliography :: [FilePath], --
    citationStyle :: Maybe FilePath --
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type RawProjectConf :: Type
data RawProjectConf = RawProjectConf
  { name :: Text,
    homepage :: Text,
    repo :: Text,
    version :: Text,
    nonTangledPathspecs :: [Text],
    bibliography :: [Text],
    citationStyle :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON)

instance FromJSON RawProjectConf where
  parseJSON = withObject "RawProjectConf" $ \v ->
    RawProjectConf
      <$> v
      .: "name"
      <*> v
      .: "homepage"
      <*> v
      .: "repo"
      <*> v
      .: "version"
      <*> v
      .: "nonTangledPathspecs"
      <*> v
      .:? "bibliography"
      .!= []
      <*> v
      .:? "citationStyle"

emptyProjectConf :: ProjectConf
emptyProjectConf =
  ProjectConf
    { name = "Test Project",
      homepage = Nothing,
      repo = Nothing,
      version = Nothing,
      nonTangledPathspecs = [],
      bibliography = [],
      citationStyle = Nothing
    }

type GlobalOpts :: Type
data GlobalOpts = GlobalOpts
  { subcommand :: Subcommand,
    projectConf :: OsPath,
    color :: Bool,
    verbose :: Bool
  }

type Subcommand :: Type
data Subcommand
  = ComCompile CompileOpts
  | ComArchive ArchiveOpts
  | ComTangle TangleOpts
  | ComWeave WeaveOpts
  | ComLint LintOpts
  | ComServe ServeOpts

type ArchiveOpts :: Type
data ArchiveOpts = ArchiveOpts
  { inputs :: [GPath],
    outFile :: OsPath
  }

type CompileOpts :: Type
data CompileOpts = CompileOpts
  { inputs :: [OsPath]
  }

type LintOpts :: Type
data LintOpts = LintOpts
  { inputs :: [OsPath] --
  }

type ServeOpts :: Type
data ServeOpts = ServeOpts
  { dir :: OsPath,
    port :: Int
  }

type TangleOpts :: Type
data TangleOpts = TangleOpts
  { inputs :: [GPath],
    outDir :: OsPath
  }

type WeaveOpts :: Type
data WeaveOpts = WeaveOpts
  { inputs :: [OsPath],
    outDir :: OsPath,
    rootRelativePrefix :: OsPath,
    writeCss :: Bool,
    writeJs :: Bool
  }

markingChars :: [Char]
markingChars = "#*" <> markingCharsInvisible

markingCharsInvisible :: [Char]
markingCharsInvisible = "!^_$/"
