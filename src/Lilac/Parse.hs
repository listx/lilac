module Lilac.Parse (parseOrgDoc) where

import Data.Graph
import Data.IntMap
import Data.Kind (Type)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type OrgDoc :: Type
data OrgDoc = OrgDoc
  { cells :: [Cell],
    code :: [Graph],
    codeBlocks :: IntMap Block,
    meta :: M.Map T.Text T.Text
  }

type Cell :: Type
data Cell
  = CHeading Heading
  | CProse Prose
  | CBlock Block
  | CListItem
      { level :: Int,
        ordered :: Bool,
        body :: Prose
      }
  | CMathFragment MathFragment
  | CFigure
      { meta :: M.Map T.Text T.Text,
        link :: Link
      }
  | CTable
      { meta :: M.Map T.Text T.Text,
        grid :: M.Map (Int, Int) Prose,
        gridSize :: (Int, Int)
      }

type Heading :: Type
data Heading = Heading
  { level :: Int,
    body :: Prose,
    meta :: M.Map T.Text T.Text
  }

type Prose :: Type
type Prose = [PToken]

type PToken :: Type
data PToken
  = PTokenStyledString StyledString
  | PTokenLink Link
  | PTokenMathFragment MathFragment

type StyledString :: Type
data StyledString = StyledString
  { body :: T.Text,
    style :: Style
  }

type Style :: Type
data Style = Plain | Bold | Italic | Code | Verbatim

type Link :: Type
data Link = Link
  { typ :: LinkType,
    target :: T.Text,
    name :: [StyledString]
  }

type LinkType :: Type
data LinkType
  = LinkNoweb
  | LinkHeading
  | LinkIdProp
  | LinkNameKeyword
  | LinkFootnote
  | LinkExternal

type Block :: Type
data Block = Block
  { meta :: M.Map T.Text T.Text,
    body :: [BToken]
  }

type BToken :: Type
data BToken
  = BTokenString T.Text
  | BTokenLink Link

type MathFragment :: Type
data MathFragment = MathFragment
  { meta :: M.Map T.Text T.Text,
    body :: T.Text
  }

type Megaparser :: Type -> Type
type Megaparser = Parsec Void T.Text

parseOrgDoc :: T.Text -> IO ()
parseOrgDoc _ = pure ()
