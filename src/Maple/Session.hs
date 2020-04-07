{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Maple.Session where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.Aeson as JS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as C
import           Data.Text (Text)
import           Data.UUID (UUID)
import qualified Data.UUID.V1 as V1
import           Data.Word
import qualified Web.ClientSession as CS

type Difficulty = Word16

type SessionGeneration = Word64

data SessionState
 = SessGeneration SessionGeneration
 | SessFraud
 deriving (Read, Show, Eq, Ord)

instance C.Serialize SessionState where
  put SessFraud = C.put (-1::Integer)
  put (SessGeneration g) = C.put (fromIntegral g::Integer)

  get = do
    (i::Integer) <- C.get
    if i < 0
      then pure $ SessFraud
      else pure $ SessGeneration (fromIntegral i)

getGen :: SessionState -> Maybe SessionGeneration
getGen (SessGeneration g) = Just g
getGen SessFraud          = Nothing

data SessionFraud = SessionFraud deriving (Show)

instance Exception SessionFraud

getGenM :: MonadThrow m => SessionState -> m SessionGeneration
getGenM SessFraud          = throwM SessionFraud
getGenM (SessGeneration g) = pure g

instance Semigroup SessionState where
  SessFraud <> _ = SessFraud
  _ <> SessFraud = SessFraud
  (SessGeneration g0) <> (SessGeneration g1) = SessGeneration $ max g0 g1

instance Monoid SessionState where
  mempty = SessGeneration 0

-- | Hash of the hint so we can go in both directions and not take too much space on the client.
--   but it is still stable to updates.
type HintId = Text

-- | This is the data we use for the client state.
--   It MUST BE HMACes on the client.
--   The generation is our protection against fuckery,
--   effectively checkpointing us to move forward when
--   loot is actually placed.
data SessionData s i
 = SessionData
   { _sdUUID         :: UUID
   , _sdGeneration   :: SessionGeneration
   , _sdKeyHistory   :: [s]
   , _sdCurrentLoot  :: [i]
   , _sdCurrentHints :: [HintId]
   , _sdRecentHints  :: [HintId]
   }
 deriving (Show, Eq, Ord)

makeLenses ''SessionData

emptySessionData :: MonadIO m => m (SessionData s i)
emptySessionData = liftIO $ do
  mu <- V1.nextUUID
  case mu of
    Nothing -> fail "Can't generate a V1 UUID?!"
    Just u -> pure $ SessionData u 0 [] [] [] []

addKey :: SessionData s i -> s -> SessionData s i
addKey sd s = sd { _sdKeyHistory = take 50 $ s : (_sdKeyHistory sd) }

addLoot :: SessionData s i -> i -> SessionData s i
addLoot sd i = sd { _sdCurrentLoot = take 50 $ i : (_sdCurrentLoot sd) }

instance (JS.ToJSON s, JS.ToJSON i) => JS.ToJSON (SessionData s i) where
  toJSON (SessionData u g h l c r) =
    JS.object
    [ "u" JS..= u
    , "g" JS..= g
    , "h" JS..= h
    , "l" JS..= l
    , "c" JS..= c --(map (encodeBase64 . T.pack . show) c)
    , "r" JS..= r
    ]

instance (JS.FromJSON s, JS.FromJSON i) => JS.FromJSON (SessionData s i) where
  parseJSON = JS.withObject "session" $ \v -> SessionData
    <$> v JS..: "u"
    <*> v JS..: "g"
    <*> v JS..: "h"
    <*> v JS..: "l"
    <*> v JS..: "c"
    <*> v JS..: "r"

class HasClientKey c where
  clientKey :: Lens' c CS.Key

instance HasClientKey CS.Key where
  clientKey = id

encodeSession :: (JS.ToJSON s, JS.ToJSON i, MonadReader c m, HasClientKey c, MonadIO m) => SessionData s i -> m ByteString
encodeSession s = do
  k <- view clientKey
  liftIO $ CS.encryptIO k (B64U.encodeBase64' $ BL.toStrict $ JS.encode s)

data SessionUnreadable = SessionUnreadable deriving (Show)

instance Exception SessionUnreadable

decodeSession :: (JS.FromJSON s, JS.FromJSON i, MonadReader c m, MonadThrow m, HasClientKey c) => ByteString -> m (SessionData s i)
decodeSession b = do
  k <- view clientKey
  case (JS.decode' . BL.fromStrict) =<< (either (const Nothing) Just . B64U.decodeBase64) =<< CS.decrypt k b of
    Nothing -> throwM SessionUnreadable
    Just s -> pure s
