module Lilac.Types.Internal
  ( GPath (..),
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
    toJSONKey,
  )
import Data.Aeson.Types
  ( toJSONKeyText,
    withText,
  )
import Data.Text qualified as T
import GHC.Generics (Generic)
import Relude
  ( Eq,
    FilePath,
    Hashable,
    Ord,
    Show,
    Type,
    pure,
    ($),
    (.),
  )

type GPath :: Type
newtype GPath = GPath FilePath
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (ToJSON)
  deriving newtype (Hashable)

instance ToJSONKey GPath where
  toJSONKey = toJSONKeyText $ \(GPath p) -> T.pack p

instance FromJSON GPath where
  parseJSON = withText "GPath" $ \t -> do
    pure . GPath $ T.unpack t

instance FromJSONKey GPath where
  fromJSONKey = FromJSONKeyTextParser $ \t -> do
    pure . GPath $ T.unpack t
