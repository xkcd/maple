{-# LANGUAGE OverloadedStrings     #-}
module Maple.Web.Session where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Maple.Session

data Sessioned s i a = Sessioned {
  sessionData  :: Text
, sessionValue :: a
} deriving (Eq, Ord, Show)

instance Functor (Sessioned s i) where
  fmap f (Sessioned d v) = Sessioned d (f v)

instance ToJSON a => ToJSON (Sessioned s i a) where
  toJSON (Sessioned d v) =
    Aeson.object [
      "session" Aeson..= d
    , "value" Aeson..= v
    ]

instance FromJSON a => FromJSON (Sessioned s i a) where
  parseJSON =
    Aeson.withObject "Sessioned" $ \o ->
      Sessioned <$> (o Aeson..: "session") <*> (o Aeson..: "value")

makeSessioned :: (MonadIO m, ToJSON s, ToJSON i, MonadReader c m, MonadThrow m, HasClientKey c)
              => SessionData s i -> a -> m (Sessioned s i a)
makeSessioned session value = do
  encodedSession <- encodeSession session
  pure $ Sessioned (Text.decodeUtf8 encodedSession) value
