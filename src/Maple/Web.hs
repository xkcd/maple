{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Maple.Web where

import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Data.Aeson (ToJSON, FromJSON, ToJSONKey)
import Data.Serialize

import Maple.AABB
import Maple.Config
import Maple.Loot
import Maple.Map
import Maple.Session
import Maple.Web.Server

import Servant.API
import Servant.Server

import qualified Network.Wai.Middleware.Cors as Cors
import Network.HTTP.Types.Method
import Network.Wai

serveMapleAPI :: forall m c g l s i v p b e
              .  (Applicative v, Num p, FromJSON (v p), Read p, FromJSON p, ToJSON p, FromJSON s, ToJSON i, FromJSON i, ToJSONKey b, FromHttpApiData b, FromHttpApiData (v p), i ~ (I m))
              => (Show (v p), Show i, HasLootName i, Eq i, ToJSON s, HasBoundingBox i v p, BinStorage c g l v p b m, HasClientKey c, Serialize l, MonadThrow m, MonadIO m, HasLocalPoolSize c, HasMapleConfig c g l s i v p b e)
              => c -- ^ Config to run the server with
              -> (forall x. m x -> IO x) -- ^ Function run the internal m in
              -> Application
serveMapleAPI config runM =
  Cors.cors getCorsPolicy $ mapleApp config $ hoistMapleAPIServer (runExceptTHandler runM) $ mapleAPIServer config
  where
    getCorsPolicy request =
      if requestMethod request == methodGet
        then Just Cors.simpleCorsResourcePolicy
        else Just corsPolicy -- Only allow select origins
    corsPolicy =
      Cors.simpleCorsResourcePolicy {
        Cors.corsOrigins = Just (config ^. mcAllowedOrigins, True)
      }
-- | Run a servant hanlder in ExceptT ServerError m a
-- in order to run things underneath. Handler doesn't allow for this
runExceptTHandler :: forall m a. (forall x. m x -> IO x)
                  -> ExceptT ServerError m a
                  -> Handler a
runExceptTHandler run ref = Handler $ mapExceptT run ref
