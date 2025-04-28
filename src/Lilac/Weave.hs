module Lilac.Weave (weave, weaveToDisk) where

import Data.Aeson (encode, encodeFile)
import Data.ByteString.Lazy.Char8 qualified as BL
import Lilac.Parse (OrgDoc)
import Relude (FilePath, IO)

weave :: OrgDoc -> BL.ByteString
weave = encode

weaveToDisk :: FilePath -> OrgDoc -> IO ()
weaveToDisk = encodeFile
