module Lilac.Parse
  ( defaultOrgDoc,
    defaultOrgDocWithMeta,
    orgDocParser,
    parseOrgDoc,
    OrgDoc,
  )
where

import Data.Char (isAsciiLower)
import Data.Graph qualified as G
import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Void
import Relude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type OrgDoc :: Type
data OrgDoc = OrgDoc
  { cells :: [Cell],
    code :: [G.Graph],
    codeBlocks :: IM.IntMap Block,
    meta :: M.Map T.Text T.Text
  }
  deriving stock (Eq, Show)

type Cell :: Type
data Cell
  = CHeading Heading
  | CProse Prose
  | CBlock Block
  | CListItem ListItem
  | CMathFragment MathFragment
  | CFigure Figure
  | CTable Table
  deriving stock (Eq, Show)

type Heading :: Type
data Heading = Heading
  { level :: Int,
    body :: Prose,
    meta :: M.Map T.Text T.Text
  }
  deriving stock (Eq, Show)

type Prose :: Type
type Prose = [PToken]

type PToken :: Type
data PToken
  = PTokenStyledString StyledString
  | PTokenLink Link
  | PTokenMathFragment MathFragment
  deriving stock (Eq, Show)

type StyledString :: Type
data StyledString = StyledString
  { body :: T.Text,
    style :: Style
  }
  deriving stock (Eq, Show)

type Style :: Type
data Style = Plain | Bold | Italic | Code | Verbatim
  deriving stock (Eq, Show)

type Link :: Type
data Link = Link
  { typ :: LinkType,
    target :: T.Text,
    name :: [StyledString]
  }
  deriving stock (Eq, Show)

type LinkType :: Type
data LinkType
  = LinkNoweb
  | LinkHeading
  | LinkIdProp
  | LinkNameKeyword
  | LinkFootnote
  | LinkExternal
  deriving stock (Eq, Show)

type Block :: Type
data Block = Block
  { meta :: M.Map T.Text T.Text,
    body :: [BToken]
  }
  deriving stock (Eq, Show)

type BToken :: Type
data BToken
  = BTokenString T.Text
  | BTokenLink Link
  deriving stock (Eq, Show)

type ListItem :: Type
data ListItem = ListItem
  { level :: Int,
    ordered :: Bool,
    body :: Prose
  }
  deriving stock (Eq, Show)

type MathFragment :: Type
data MathFragment = MathFragment
  { meta :: M.Map T.Text T.Text,
    body :: T.Text
  }
  deriving stock (Eq, Show)

type Figure :: Type
data Figure = Figure
  { meta :: M.Map T.Text T.Text,
    link :: Link
  }
  deriving stock (Eq, Show)

type Table :: Type
data Table = Table
  { meta :: M.Map T.Text T.Text,
    grid :: M.Map (Int, Int) Prose,
    gridSize :: (Int, Int)
  }
  deriving stock (Eq, Show)

parseOrgDoc :: T.Text -> Either T.Text OrgDoc
parseOrgDoc input = case MP.parse orgDocParser "" input of
  Left err -> Left . T.pack $ MP.errorBundlePretty err
  Right m -> Right m

type Megaparser :: Type -> Type
type Megaparser = MP.Parsec Void T.Text

spaceConsumer :: Megaparser ()
spaceConsumer =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    blockCommentParser
  where
    whiteSpaceParser :: Megaparser ()
    whiteSpaceParser = space1
    singleLineCommentParser :: Megaparser ()
    singleLineCommentParser = (L.skipLineComment "# ")
    blockCommentParser :: Megaparser ()
    blockCommentParser = (L.skipBlockComment "#+begin_comment" "#+end_comment")

symbol :: T.Text -> Megaparser T.Text
symbol = L.symbol spaceConsumer

lexeme :: Megaparser a -> Megaparser a
lexeme = L.lexeme spaceConsumer

orgDocParser :: Megaparser OrgDoc
orgDocParser = do
  _ <- optional spaceConsumer
  meta <- docMetaParser
  MP.eof
  pure
    $ OrgDoc
      { cells = [],
        code = [],
        codeBlocks = IM.fromList [],
        meta = meta
      }

defaultOrgDoc :: OrgDoc
defaultOrgDoc =
  OrgDoc
    { cells = [],
      code = [],
      codeBlocks = IM.fromList [],
      meta = M.fromList []
    }

defaultOrgDocWithMeta :: [(T.Text, T.Text)] -> OrgDoc
defaultOrgDocWithMeta metas =
  OrgDoc
    { cells = [],
      code = [],
      codeBlocks = IM.fromList [],
      meta = M.fromList metas
    }

docMetaEntryParser :: Megaparser (T.Text, T.Text)
docMetaEntryParser = do
  _ <- string "#+"
  key <- MP.takeWhile1P (Just "keyword letter") isKeywordLetter
  _ <- symbol ":"
  value <- MP.takeWhile1P (Just "value letter") (\c -> c /= '\n')
  pure (key, value)
  where
    isKeywordLetter c = or $ map ($ c) [isAsciiLower, (== '_')]

docMetaParser :: Megaparser (M.Map T.Text T.Text)
docMetaParser = do
  metaKVs <- many (lexeme docMetaEntryParser)
  pure $ M.fromList metaKVs
