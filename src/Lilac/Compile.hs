module Lilac.Compile (mkObject) where

import Data.Aeson (ToJSON)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lilac.Types
  ( BToken (BTokenLink, BTokenPadding, BTokenString),
    Block (body, meta, parentOid),
    Cell (CBlock, CFigure, CHeading, CListItem, CMathFragment, CParagraph, CTable),
    Figure (meta),
    Heading (body, meta),
    Link (target),
    LinkNoweb (name, oid),
    LinkTarget (LinkTargetNoweb, LinkTargetOther, LinkTargetURI),
    MathFragment (meta),
    OID,
    OrgDoc (cells, oid),
    PToken (PTokenLink, PTokenMathFragment, PTokenStyledText),
    Table (grid, meta),
  )
import Lilac.Util qualified as LU
import Relude
  ( Eq,
    Maybe (Just, Nothing),
    Show,
    Type,
    catMaybes,
    concatMap,
    foldl',
    map,
    or,
    otherwise,
    sort,
    ($),
    (.),
    (<>),
  )
import Relude.Extra.Map qualified as EM

type Object :: Type
data Object = Object
  { orgDoc :: OrgDoc,
    symbols :: M.Map T.Text Cell,
    externalLinkTargets :: [LinkTarget]
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON Object

mkObject :: OrgDoc -> Object
mkObject orgDoc =
  Object
    { orgDoc = orgDoc,
      symbols = symbols',
      externalLinkTargets = targets
    }
  where
    symbols' = mkSymbols orgDoc
    targets = findExternalOrgLinkTargets orgDoc symbols'

mkSymbols :: OrgDoc -> M.Map T.Text Cell
mkSymbols orgDoc = M.fromList $ concatMap f orgDoc.cells
  where
    f cell = case cell of
      CBlock block -> getSymbols block.parentOid block.meta cell
      CHeading heading
        | Just oidText <- EM.lookup "ID" heading.meta ->
            [("id:" <> oidText, cell)]
        | otherwise -> []
      CFigure figure -> getSymbols Nothing figure.meta cell
      CMathFragment mathFragment -> getSymbols Nothing mathFragment.meta cell
      CTable table -> getSymbols Nothing table.meta cell
      CListItem {}
      CParagraph {} -> []
    getSymbols :: Maybe OID -> M.Map T.Text T.Text -> Cell -> [(T.Text, Cell)]
    getSymbols parentOid cellMeta cell =
      concatMap requireName $ catMaybes [parentOid, Just $ orgDoc.oid]
      where
        requireName oid
          | Just name <- EM.lookup "name" cellMeta =
              [ (LU.prefixWithId (Just oid) name, cell),
                (name, cell)
              ]
          | otherwise = []

findExternalOrgLinkTargets :: OrgDoc -> M.Map T.Text Cell -> [LinkTarget]
findExternalOrgLinkTargets orgDoc symbols =
  sort
    . Set.toList
    . Set.fromList
    $ concatMap f orgDoc.cells
  where
    f cell = case cell of
      CBlock block -> foldl' g [] block.body
      CHeading heading -> foldl' h [] heading.body
      CParagraph prose -> foldl' h [] prose
      CTable table -> M.foldl (foldl' h) [] table.grid
      CFigure {}
      CListItem {}
      CMathFragment {} -> []
    g acc = \case
      BTokenLink link -> getExternal acc link
      BTokenString {}
      BTokenPadding -> acc
    h acc = \case
      PTokenLink link -> getExternal acc link
      PTokenStyledText {}
      PTokenMathFragment {} -> acc
    getExternal acc link = case link.target of
      LinkTargetNoweb linkNoweb ->
        let target = LU.prefixWithId linkNoweb.oid linkNoweb.name
         in if M.member target symbols
              then acc
              else link.target : acc
      LinkTargetOther raw
        | or $ map (`T.isPrefixOf` raw) webProtocols -> acc
        | or $ map (`T.isPrefixOf` raw) aliasedWebProtocols -> acc
        | M.member raw symbols -> acc
        | otherwise -> link.target : acc
      LinkTargetURI {} -> acc

webProtocols :: [T.Text]
webProtocols =
  map
    (<> "://")
    [ "http",
      "https",
      "file",
      "ftp",
      "mailto",
      "tel"
    ]

aliasedWebProtocols :: [T.Text]
aliasedWebProtocols =
  map
    (<> ":")
    [ "file"
    ]
