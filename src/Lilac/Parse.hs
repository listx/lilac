module Lilac.Parse
  ( defaultOrgDoc,
    defaultOrgDocWithMeta,
    defaultOrgParserState,
    orgDocParser,
    parseOrgDoc,
    OrgDoc (..),
    Cell (..),
    Heading (..),
    Link (..),
    LinkType (..),
    MathFragment (..),
    PToken (..),
    Style (..),
    StyledString (..),
  )
where

import Data.Char (isAsciiLower)
import Data.Graph qualified as G
import Data.IntMap qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Relude
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char, space1, string)
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
data Style = Plain | Bold | Italic | Underline | Verbatim | Code | Strikethrough
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
parseOrgDoc input = case MP.runParser
  (runStateT orgDocParser defaultOrgParserState)
  ""
  input of
  Left err -> Left . T.pack $ MP.errorBundlePretty err
  Right (orgDoc, _orgParserState) -> Right orgDoc

type OrgParser :: Type -> Type
type OrgParser = StateT OrgParserState (MP.Parsec Void T.Text)

type OrgParserState :: Type
data OrgParserState = OrgParserState
  { styleStack :: [Style]
  }
  deriving stock (Eq, Show)

defaultOrgParserState :: OrgParserState
defaultOrgParserState =
  OrgParserState
    { styleStack = []
    }

spaceConsumer :: OrgParser ()
spaceConsumer =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    blockCommentParser
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = space1
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = (L.skipLineComment "# ")
    blockCommentParser :: OrgParser ()
    blockCommentParser = (L.skipBlockComment "#+begin_comment" "#+end_comment")

symbol :: T.Text -> OrgParser T.Text
symbol = L.symbol spaceConsumer

lexeme :: OrgParser a -> OrgParser a
lexeme = L.lexeme spaceConsumer

spaceConsumerNoNewline :: OrgParser ()
spaceConsumerNoNewline =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    empty
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = void $ some (char ' ' <|> char '\t')
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = (L.skipLineComment "# ")

symbolNoNewline :: T.Text -> OrgParser T.Text
symbolNoNewline = L.symbol spaceConsumerNoNewline

lexemeNoNewline :: OrgParser a -> OrgParser a
lexemeNoNewline = L.lexeme spaceConsumerNoNewline

spaceConsumerNoDoubleNewline :: OrgParser ()
spaceConsumerNoDoubleNewline =
  L.space
    whiteSpaceParser
    singleLineCommentParser
    empty
  where
    whiteSpaceParser :: OrgParser ()
    whiteSpaceParser = void $ some f
    f :: OrgParser ()
    f = do
      MP.notFollowedBy (string "\n\n")
      void $ char ' ' <|> char '\t' <|> char '\n'
    singleLineCommentParser :: OrgParser ()
    singleLineCommentParser = (L.skipLineComment "# ")

symbolNoDoubleNewline :: T.Text -> OrgParser T.Text
symbolNoDoubleNewline = L.symbol spaceConsumerNoDoubleNewline

lexemeNoDoubleNewline :: OrgParser a -> OrgParser a
lexemeNoDoubleNewline = L.lexeme spaceConsumerNoDoubleNewline

orgDocParser :: OrgParser OrgDoc
orgDocParser = do
  _ <- optional spaceConsumer
  meta <- docMetaParser
  cells <- many cellParser
  MP.eof
  pure
    $ OrgDoc
      { cells = cells,
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

docMetaEntryParser :: OrgParser (T.Text, T.Text)
docMetaEntryParser = do
  _ <- string "#+"
  key <- MP.takeWhile1P (Just "keyword letter") isKeywordLetter
  _ <- symbol ":"
  value <- MP.takeWhile1P (Just "value letter") (\c -> c /= '\n')
  pure (key, value)
  where
    isKeywordLetter c = or $ map ($ c) [isAsciiLower, (== '_')]

docMetaParser :: OrgParser (M.Map T.Text T.Text)
docMetaParser = do
  metaKVs <- many (lexeme docMetaEntryParser)
  pure $ M.fromList metaKVs

cellParser :: OrgParser Cell
cellParser = do
  lexeme headingParser

headingParser :: OrgParser Cell
headingParser = do
  stars <- MP.takeWhile1P (Just "heading star") (\c -> c == '*')
  _ <- char ' '
  body <- MP.some (lexemeNoNewline pTokenParser)
  _ <- char '\n'
  drawers <- drawersParser
  pure
    $ CHeading
      Heading
        { level = T.length stars,
          body = body,
          meta = drawers
        }

proseParser :: OrgParser Prose
proseParser = many (lexemeNoDoubleNewline pTokenParser)

pTokenParser :: OrgParser PToken
pTokenParser = do
  MP.choice
    [ linkParser',
      mathFragmentInlineParser',
      styledStringParser'
    ]
  where
    linkParser' = PTokenLink <$> linkParser
    mathFragmentInlineParser' = PTokenMathFragment <$> mathFragmentInlineParser
    styledStringParser' = PTokenStyledString <$> styledStringParser

styledStringParser :: OrgParser StyledString
styledStringParser = do
  bold <-
    MP.between
      (string "*")
      (string "*")
      ( MP.label "bolded text"
          $ MP.some
          $ MP.anySingleBut '*'
      )
  pure
    StyledString
      { body = "FOO",
        style = Plain
      }

boldParser :: OrgParser StyledString
boldParser = do
  MP.between
    (string "*")
    (string "*")
    ( MP.label "bolded text"
        $ MP.some
        $ MP.anySingleBut '*'
    )
  pure
    StyledString
      { body = "FOO",
        style = Plain
      }

linkParser :: OrgParser Link
linkParser = do
  MP.choice [linkExternalParser]

linkExternalParser :: OrgParser Link
linkExternalParser = do
  _ <- string "["
  target <-
    MP.between
      (string "[")
      (string "]")
      ( MP.label "external link target"
          $ MP.some
          $ MP.anySingleBut ']'
      )
  name <-
    MP.optional
      $ MP.between
        (string "[")
        (string "]")
        ( MP.label "external link name"
            $ MP.some
            $ MP.anySingleBut ']'
        )
  _ <- string "]"
  pure
    Link
      { typ = LinkExternal,
        target = T.pack target,
        name =
          [ StyledString
              { body = T.pack (fromMaybe target name),
                style = Plain
              }
          ]
      }

mathFragmentInlineParser :: OrgParser MathFragment
mathFragmentInlineParser = do
  _ <- string "\\("
  latex <- MP.someTill (MP.label "LaTeX math input" MP.anySingle) (string "\\)")
  pure
    MathFragment
      { meta = M.fromList [],
        body = T.pack latex
      }

drawersParser :: OrgParser (Map T.Text T.Text)
drawersParser = do
  kvs <- many (lexeme drawerParser)
  pure $ M.fromList kvs

drawerParser :: OrgParser (T.Text, T.Text)
drawerParser = do
  _ <- char ':'
  drawerName <- MP.takeWhile1P (Just "drawer name") isDrawerLetter
  _ <- char ':'
  _ <- char '\n'
  drawerContents <-
    MP.manyTill
      (MP.label "drawer contents" MP.anySingle)
      (string ":END:\n")
  pure (drawerName, T.pack drawerContents)
  where
    isDrawerLetter c = and $ map ($ c) [(/= '\n'), (/= ':'), (/= ' ')]
