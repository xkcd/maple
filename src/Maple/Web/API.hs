{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO remove this
module Maple.Web.API where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Linear
import Network.HTTP.Date
import Text.Read

import Servant.API

import Maple.AABB
import Maple.Web.Admin
import Maple.Web.Local
import Maple.Web.Session

instance Read p => FromHttpApiData (V2 p) where
  parseQueryParam txt =
    -- TODO Clean up this "parser"
    case Text.splitOn "," txt of
      [x,y] -> first Text.pack $  V2 <$> readEitherText x <*> readEitherText y
      _ -> Left $ "Unable to parse V2 point from: " <> txt
    where
      readEitherText = readEither . Text.unpack

instance Read p => FromHttpApiData (V3 p) where
  parseQueryParam txt =
    -- TODO Clean up this "parser"
    case Text.splitOn "," txt of
      [x,y,z] -> first Text.pack $ V3 <$> readEitherText x <*> readEitherText y <*> readEitherText z
      _ -> Left $ "Unable to parse V3 point from: " <> txt
    where
      readEitherText = readEither . Text.unpack

-- TODO remove this. it's bad
instance (ToJSON p) => ToJSON (V3 p) where
  toJSON p = toJSON $ Foldable.toList p
instance (ToJSON p) => ToJSON (V2 p) where
  toJSON p = toJSON $ Foldable.toList p

instance (FromJSON p) => FromJSON (V3 p) where
  parseJSON o = do
    xs <- parseJSON o
    case xs of
      [x,y,z] -> pure $ V3 x y z
      _       -> fail "Unable to parse V3 from list"

instance (FromJSON p) => FromJSON (V2 p) where
  parseJSON o = do
    xs <- parseJSON o
    case xs of
      [x,y] -> pure $ V2 x y
      _     -> fail "Unable to parse V2 from list"

data UserInfo i = UserInfo {
  userLoot  :: [i]
, userHints :: [Text.Text]
} deriving (Eq, Ord, Show)

instance ToJSON i => ToJSON (UserInfo i) where
  toJSON (UserInfo loot hints) =
    Aeson.object [
      "loot" Aeson..= loot
    , "hints" Aeson..= hints
    ]

instance FromJSON i => FromJSON (UserInfo i) where
  parseJSON =
    Aeson.withObject "UserInfo" $ \o ->
      UserInfo
        <$> o Aeson..: "loot"
        <*> o Aeson..: "hints"


data UserWithBins b i = UserWithBins {
  binImages :: Map b [i]
, lootPlaced :: Bool
, userData :: UserInfo i
} deriving (Eq, Ord, Show)

instance (Aeson.ToJSONKey b, ToJSON i) => ToJSON (UserWithBins b i) where
  toJSON (UserWithBins b p u) =
    Aeson.object [
      "bins" Aeson..= b
    , "placed" Aeson..= p      
    , "user" Aeson..= u
    ]

instance (Aeson.FromJSONKey b, FromJSON i, Ord b) => FromJSON (UserWithBins b i) where
  parseJSON =
    Aeson.withObject "UserWithBins" $ \o ->
      UserWithBins
        <$> o Aeson..: "bins"
        <*> o Aeson..: "placed"
        <*> o Aeson..: "user"

instance ToHttpApiData HTTPDate where
  toUrlPiece = Text.decodeUtf8 . formatHTTPDate

data MapleCacheControl = MapleCacheControl {
  mapleCacheControlMaxSeconds :: Word
}

newtype CacheControlHeader = CacheControlHeader {
  unCacheControlHeader :: Text.Text
}

instance ToHttpApiData CacheControlHeader where
  toUrlPiece = unCacheControlHeader

type (MapleAPI s l v p b i) =
          "view" :> QueryParam' '[Required] "minPoint" (V2 p) :> QueryParam' '[Required] "maxPoint" (V2 p) :> Get '[JSON] (Headers '[Header "Surrogate-Control" Text.Text, Header "Cache-Control" CacheControlHeader] (Map b (BoundingBox V2 p))) --
          -- View
          -- TODO
          --   give chromakode better JSON (get rid of the tuples)
          --   fix BoundingBox query parameter encoding
    :<|> "bin" :> Capture "id" b :> Get '[JSON] (Headers '[Header "Cache-Control" CacheControlHeader, Header "Expires" HTTPDate] [i]) -- 5 seconds
    :<|> "season" :> "pass" :> Post '[JSON] (Sessioned s i (UserInfo i))
    :<|> MapleLocal l :> "user" :> ReqBody '[JSON] (Sessioned s i ()) :> Post '[JSON] (Sessioned s i (UserInfo i))
    :<|> MapleLocal l :> "claim" :> ReqBody '[JSON] (Sessioned s i s) :> Post '[JSON] (Sessioned s i (UserInfo i))
    :<|> MapleLocal l :> "place" :> ReqBody '[JSON] (Sessioned s i i) :> Post '[JSON] (Headers '[Header "Cache-Control" CacheControlHeader] (Sessioned s i (UserWithBins b i)))
    :<|> "healthz" :> GetNoContent
    :<|> "static" :> Raw
    :<|> BasicAuth "admin" MapleAdmin :> "mod" :> "clear" :> ReqBody '[JSON] (BoundingBox v p) :> Post '[JSON] (Map b [i])
    :<|> BasicAuth "admin" MapleAdmin :> "mod" :> "remove" :> ReqBody '[JSON] i :> Post '[JSON] (Map b [i])
    :<|> BasicAuth "admin" MapleAdmin :> "mod" :> "loot" :> "all" :> Get '[JSON] [i]
    :<|> MapleLocal l :> BasicAuth "admin" MapleAdmin :> "mod" :> "loot" :> "place" :> ReqBody '[JSON] i :> Post '[JSON] (Map b [i])

mapleAPI :: Proxy (MapleAPI c l v p b i)
mapleAPI = Proxy
