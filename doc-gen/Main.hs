{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO remove this
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import Data.Proxy
import Linear
import Data.Text (Text)
import qualified Data.Time as Time
import Network.HTTP.Date

import Servant.API
import Servant.Docs
import Servant.Docs.Internal
import Web.Cookie

import Maple.AABB
import Maple.Loot
import Maple.Web.Admin
import Maple.Web.API
import Maple.Web.Local
import Maple.Web.Session

main :: IO ()
main = writeFile "docs.md" markdownMapleDocs

mapleDocsAPI :: Proxy (MapleAPI Int Int V3 Int Int (ImgLoot V3 Int))
mapleDocsAPI = mapleAPI

mapleDocs :: API
mapleDocs = docsWith (DocOptions 5) [] mempty mapleDocsAPI

markdownMapleDocs :: String
markdownMapleDocs = markdown mapleDocs

instance ToAuthInfo (BasicAuth "admin" MapleAdmin) where
  toAuthInfo _ =
    DocAuthentication "This route requires a user login" "Username and password"

{- Warning - orphan instances -}

instance ToSample (Map Int [ImgLoot V3 Int]) where
  toSamples _ =
    samples [Map.fromList [ (1,[ImgLoot (bound (V3 0 0 1) (V3 20 20 1)) "https://example.com/image.png"])]]

instance ToSample (Sessioned Int (ImgLoot V3 Int) (UserWithBins Int (ImgLoot V3 Int))) where
  toSamples _ = samples [Sessioned "{user session}" $ UserWithBins (Map.fromList [(1,[ImgLoot (bound (V3 0 0 1) (V3 20 20 1)) "https://example.com/image.png"])]) True u]
    where u = UserInfo [ImgLoot (bound (V3 0 0 1) (V3 20 20 1)) "https://example.com/image.png"] ["A hint to help you find an image"]

instance ToSample Text where
  toSamples _ = samples ["..."]

instance HasDocs api => HasDocs (MapleLocal s :> api) where
  docsFor _ = docsFor (Proxy :: Proxy api)

instance ToSample (Sessioned Int (ImgLoot V3 Int) (ImgLoot V3 Int)) where
  toSamples _ =
    samples [Sessioned "{user session}" $ ImgLoot (bound (V3 0 0 1) (V3 20 20 1)) "https://example.com/image.png"]

instance ToSample (Sessioned Int (ImgLoot V3 Int) (UserInfo (ImgLoot V3 Int))) where
  toSamples _ =
    samples [Sessioned "{user session}" $ UserInfo [ImgLoot (bound (V3 0 0 1) (V3 20 20 1)) "https://example.com/image.png"] ["A hint to help you find an image"]]

instance ToSample (Sessioned Int (ImgLoot V3 Int) ()) where
  toSamples _ =
    samples [Sessioned "{user session}" ()]

instance ToSample (Sessioned Int (ImgLoot V3 Int) Int) where
  toSamples _ =
    samples [Sessioned "{user session}" 10]

instance ToSample HTTPDate where
  toSamples _ =
    samples [utcToHTTPDate $ Time.UTCTime (Time.ModifiedJulianDay 20090) $ Time.secondsToDiffTime 0]

instance ToSample CacheControlHeader where
  toSamples _ =
    samples [CacheControlHeader "public, max-age=20"]

instance ToSample MapleCacheControl where
  toSamples _ =
    samples [MapleCacheControl 5, MapleCacheControl 60]

instance ToSample (BoundingBox V3 Int) where
  toSamples _ =
    samples [bound (V3 0 0 0) (V3 20 20 1)]

instance ToSample (UserInfo (ImgLoot V3 Int)) where
  toSamples _ = 
    samples [UserInfo [ImgLoot (bound (V3 0 0 1) (V3 20 20 1)) "https://example.com/image.png"] ["A hint to help you find an image"]]


instance ToSample (UserInfo (ImgLoot V2 Int)) where
  toSamples _ = 
    samples [UserInfo [ImgLoot (bound (V2 0 0) (V2 20 20)) "https://example.com/image.png"] ["A hint to help you find an image"]]

instance ToSample SetCookie where
  toSamples _ =
    samples [defaultSetCookie { setCookieName = "maple", setCookieValue = "..."}]

instance ToSample (V2 Int) where
  toSamples _ =
    samples [V2 0 0, V2 5 6, V2 12 12]

instance ToSample (V3 Int) where
  toSamples _ =
    samples [V3 0  0 0, V3 5 0 6, V3 12 12 1 ]


instance ToSample (BoundingBox V2 Int) where
  toSamples _ =
    samples $ [bound (V2 0 0) (V2 10 10)]

instance ToSample (Map Int (BoundingBox V2 Int)) where
  toSamples _ =
    singleSample $ Map.fromList $ Foldable.toList $ (,) <$> toSample Proxy <*> toSample Proxy

instance ToSample Int where
  toSamples _ = singleSample 5

instance ToSample (ImgLoot V2 Int) where
  toSamples _ = singleSample $ ImgLoot (bound (V2 0 0) (V2 10 10)) "test.png"

instance ToSample (ImgLoot V3 Int) where
  toSamples _ = singleSample $ ImgLoot (bound (V3 0 0 0) (V3 10 10 1)) "test.png"

instance ToSample p => ToParam (QueryParam' '[Required] "maxPoint" (V2 p)) where
  toParam _ =
    DocQueryParam "maxPoint"
                  ["5,10", "0,0"]
                  "Required maximum point"
                  Normal


instance ToSample p => ToParam (QueryParam' '[Required] "minPoint" (V2 p)) where
  toParam _ =
    DocQueryParam "minPoint"
                  ["0,5", "12,15"]
                  "Required minimum point"
                  Normal

instance ToParam (QueryParam' '[Required] "minPoint" (V3 Int)) where
  toParam _ =
    DocQueryParam "minPoint"
                  ["0,5,1", "12,15,1"]
                  "Required minimum point"
                  Normal

instance ToParam (QueryParam' '[Required] "maxPoint" (V3 Int)) where
  toParam _ =
    DocQueryParam "maxPoint"
                  ["0,0,1", "8,8,1"]
                  "Required maximum point"
                  Normal

instance ToSample b => ToCapture (Capture "id" b) where
  toCapture _ =
    DocCapture "id" "id for a bin"
