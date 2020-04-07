{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Maple.Web.Server where

import Control.Lens (Lens', to, view, (%~), (&), (^.), (.~))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Aeson
import Data.Bifunctor
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LText
import qualified Data.Text.Lazy.Builder.Int as LText
import qualified Data.Time as Time
import Data.Tuple
import Linear
import Network.HTTP.Date
import Network.Mime
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import System.IO
import System.Random
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types
import qualified Web.ClientSession as CS

import Maple.AABB
import Maple.Config
import Maple.Loot
import Maple.Map
import Maple.Session
import Maple.Web.Admin
import Maple.Web.API
import Maple.Web.Local
import Maple.Web.Session


mapleApp :: forall config g l s i v p b e
         .  (FromJSON (v p), FromJSON (V2 p), Read p, ToJSON p, FromJSON s, ToJSON i, FromJSON i, ToJSONKey b, FromHttpApiData b, FromHttpApiData (v p))
         => HasMapleConfig config g l s i v p b e
         => config
         -> Server (MapleAPI s l v p b i)
         -> Application
mapleApp config =
  serveWithContext mapleAPI context
  where
    context :: Context '[BasicAuthCheck MapleAdmin, CS.Key, MakeMapleLocal l]
    context = (mapleAdminCheck $ config ^. mcCheckAdmin) :. (config ^. mcCSKey) :. (config ^. mcMkLocal) :. EmptyContext

-- | Hoist a maple API server into a different monad
-- useful for n ~ Handler
hoistMapleAPIServer :: forall s l i v p b m n
                    .  (Num p, Applicative v, FromJSON (v p), ToJSON p, FromJSON (V2 p), Read p, ToJSON i, FromJSON i, FromJSON s, ToJSONKey b, FromHttpApiData b, FromHttpApiData (v p))
                    => (forall x. m x -> n x)
                    -> ServerT (MapleAPI s l v p b i) m
                    -> ServerT (MapleAPI s l v p b i) n
hoistMapleAPIServer = hoistServerWithContext mapleAPI mapleContext
  where
    mapleContext :: Proxy '[BasicAuthCheck MapleAdmin, CS.Key, MakeMapleLocal l]
    mapleContext = Proxy

-- | Maple API server
-- TODO add a new session request
mapleAPIServer :: (Show (v p), Show i, HasLocalPoolSize c, Serialize l, MonadError ServerError m, MonadThrow m, MonadIO m, HasMapleConfig c g l s i v p b e, i ~ I m)
               => (HasLootName i, Num p, Applicative v, FromJSON s, FromJSON i, FromJSON (v p), ToJSON s, HasClientKey c, Eq (v p), Eq i, ToJSON i, HasBoundingBox i v p, BinStorage c g l v p b m)
               => c -> ServerT (MapleAPI s l v p b i) m
mapleAPIServer c =
       viewportBinsH
  :<|> binContentsH
  :<|> newSeasonPassH
  :<|> userInfoH
  :<|> claimLootH
  :<|> placeLootH
  :<|> healthH
  :<|> staticH (c ^. mcImageFolders)
  :<|> moderationClearH
  :<|> moderationRemoveH
  :<|> moderationListLootH (c ^. mcAdminLoot)
  :<|> moderationPlaceLootH

newSeasonPassH :: (MonadThrow m, MonadIO m, HasClientKey c, ToJSON s, ToJSON i, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
               => m (Sessioned s i (UserInfo i))
newSeasonPassH = guardOnPaused $ do
  newSession <- emptySessionData
  makeUserInfo newSession

-- | Get the bounding boxes and bins for a given viewport
-- TODO should this check viewport bounding box size?
-- TODO Maybe change the order in overlapableBins?
viewportBinsH :: (Num p, Applicative v, MonadIO m, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
              => V2 p -> V2 p -> m (Headers '[Header "Surrogate-Control" Text.Text, Header "Cache-Control" CacheControlHeader] (Map b (BoundingBox V2 p)))
viewportBinsH minPoint maxPoint = do
  withSurrogateHeader $ withCacheHeaders mcBinListCachePolicy $ do
    bs <- view (to (`overlapableBins` viewport))
    pure $ Map.fromList $ fmap swap bs
  where viewport = bound ((pure 0) & _xy .~ minPoint) ((pure 0) & _xy .~ maxPoint) 

binContentsH :: (MonadIO m, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
             => b -> m (Headers '[Header "Cache-Control" CacheControlHeader, Header "Expires" HTTPDate] [I m])
binContentsH bin =
  withCacheHeaders mcBinContentsCachePolicy $ withExpiresHeaders mcBinContentsCachePolicy $ map _dbmItem <$> tip bin

userInfoH :: (MonadThrow m, FromJSON i, FromJSON s, i ~ I m, BinStorage c g l v p b m, HasLocalPoolSize c)
          => (Serialize l, HasMapleConfig c g l s i v p b e, MonadIO m, HasClientKey c, ToJSON s, ToJSON i)
          => l -> Sessioned s i () -> m (Sessioned s i (UserInfo i))
userInfoH local sessioned = guardOnPaused $ do
  lootBoxes <- view mcLootBoxes
  (sessionInfo, _) <- first (currentHintsOnly lootBoxes) <$> getSessionData local sessioned
  newHints <- sessionHints lootBoxes local sessionInfo
  newSessionInfo <- if null newHints then pure sessionInfo else incSessionGen (sessionInfo & sdCurrentHints %~ mappend newHints)
  makeUserInfo newSessionInfo

claimLootH :: (HasLocalPoolSize c, Serialize l, MonadThrow m, FromJSON i, FromJSON s, MonadIO m, HasClientKey c, ToJSON s, ToJSON i, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
           => l -> Sessioned s i s -> m (Sessioned s i (UserInfo i))
claimLootH local sessioned = guardOnPaused $ do
  (currentSession, key) <- getSessionData local sessioned
  lootBoxes <- view mcLootBoxes
  let newSession = addKey currentSession key
  case dropLoot lootBoxes newSession of
    [] -> do
      -- Don't need to increment the session here
      -- a replay attack doesn't help them here
      makeUserInfo newSession
    lootResult -> do
      let completedHints = fmap fst lootResult
          newLoot = fmap snd lootResult
          addLootToSession s = foldr' (flip addLoot) s newLoot
          removeHintsFromSession s = s & sdCurrentHints %~ (filter (not . (`elem` completedHints)))
                                       & sdRecentHints %~ (Set.toList . Set.fromList . (completedHints++))
      ld <- readLocalDifficulty local
      incBy <- fmap (sum . (0:)) . liftIO .
               forM (mapMaybe (fmap _lbDifficulty . (`Map.lookup` lootBoxes)) $ completedHints) $ \hd -> do
        incEng <- randomRIO (0, ld + hd)
        pure $ if incEng < hd then 1 else 0
      when (incBy > 0) $ incLocalDifficulty local incBy
      lootSession <- incSessionGen $ removeHintsFromSession $ addLootToSession newSession
      makeUserInfo lootSession

-- | Sign and encode the session
-- returning the set cookie header for the session
-- and the UserInfo for the session
makeUserInfo :: (MonadIO m, HasClientKey c, ToJSON s, ToJSON i, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
             => SessionData s i -> m (Sessioned s i (UserInfo i))
makeUserInfo session = do
  lootBoxes <- view mcLootBoxes
  newSession <- Text.decodeUtf8 <$> encodeSession session
  pure $ Sessioned newSession $ UserInfo {
      userLoot = session ^. sdCurrentLoot
      -- Done this way to keep the order of hints
    , userHints = fmap (view lbHint) . catMaybes $ (flip Map.lookup lootBoxes) <$> (session ^. sdCurrentHints)
    }

-- TODO give back new session
placeLootH :: (HasMapleConfig c g l s i v p b e, MonadThrow m, MonadError ServerError m, HasClientKey c, BinStorage c g l v p b m, HasBoundingBox i v p)
           => (HasLootName i, Num p, HasLocalPoolSize c, Serialize l, ToJSON s, i ~ I m, ToJSON i, FromJSON i, FromJSON s, Eq i, MonadThrow m, MonadIO m)
           => l -> Sessioned s i i -> m (Headers '[Header "Cache-Control" CacheControlHeader] (Sessioned s i (UserWithBins b i)))
placeLootH local sessioned = withCacheHeaders mcBinContentsCachePolicy $ guardOnPaused $ do
  (session, loot) <- getSessionData local sessioned
  removedLootSession <- removeLoot session loot
  currentTime <- liftIO Time.getCurrentTime
  added <- binTX doesNotOverlap (DBMeta local currentTime loot)
  newSession <- incSessionGen $ if added then removedLootSession else session
  u <- makeUserInfo newSession
  let viewport = loot ^. boundingBox
  bs <- fmap snd <$> view (to (`overlapableBins` viewport))
  items <- mapM (\b -> (\is -> (b, fmap _dbmItem is)) <$> tip b) bs
  pure $ UserWithBins (Map.fromList items) added <$> u

healthH :: (Monad m, MonadThrow m, BinStorage c g l v p b m) => m NoContent
healthH = do
  checkDB
  pure NoContent

staticH :: [String] -> ServerT Raw m
staticH folders = serveDirectoryWith $ serveDirectoriesStatic folders

serveDirectoriesStatic :: [String] -> StaticSettings
serveDirectoriesStatic folders =
  StaticSettings {
      ssLookupFile = lookupFile
    , ssGetMimeType = return . defaultMimeLookup . fromPiece . fileName
    , ssIndices = []
    , ssListing = Nothing
    , ssMaxAge = MaxAgeSeconds 60 -- TODO make this a configuration option
    , ssMkRedirect = ssMkRedirect (defaultWebAppSettings "") -- This doesn't actually use the folder "" but it isn't exported anywhere else
    , ssRedirectToIndex = False
    , ssUseHash = True
    , ssAddTrailingSlash = False
    , ss404Handler = Nothing
  }
  where
    lookupFile pieces =
      case pieces of
        (p:rest) ->
          let p' =  Text.unpack $ fromPiece p
          in if p' `elem` folders
              then do ssLookupFile (defaultWebAppSettings p') rest
              else pure LRNotFound
        _ -> pure LRNotFound

-- | Remove all items that overlap with a bin
moderationClearH :: (MonadIO m, Show (v p), HasMapleConfig c g l s i v p b e, i ~ I m, BinStorage c g l v p b m, HasBoundingBox i v p)
                 => MapleAdmin -> BoundingBox v p ->  m (Map b [i])
moderationClearH admin viewport = do
  logAdminAction admin $ "clear " <> show viewport
  bins <- fmap snd <$> view (to (`overlapableBins` viewport))
  mapM_ (flip binFilter (not . overlaps viewport . view boundingBox)) bins
  items <- mapM (\b -> (\is -> (b, fmap _dbmItem is)) <$> tip b) bins
  pure $ Map.fromList items

moderationRemoveH :: (MonadIO m, Show i, Eq i, HasBoundingBox i v p, HasMapleConfig c g l s i v p b e, i ~ I m, BinStorage c g l v p b m)
                  => MapleAdmin -> i -> m (Map b [i])
moderationRemoveH admin i = do
  logAdminAction admin $ "remove " <> show i
  let imageBB = i ^. boundingBox
  bins <- fmap snd <$> view (to (`overlapableBins` imageBB))
  mapM_ (flip binFilter (not . ((==) i) . _dbmItem)) bins
  items <- mapM (\b -> (\is -> (b, fmap _dbmItem is)) <$> tip b) bins
  pure $ Map.fromList items

moderationListLootH :: (HasBoundingBox i v p, HasMapleConfig c g l s i v p b e, i ~ I m, BinStorage c g l v p b m)
                    => [i] -> MapleAdmin -> m [i]
moderationListLootH allLoot _ = pure allLoot

moderationPlaceLootH :: (MonadError ServerError m, HasClientKey c, BinStorage c g l v p b m, HasBoundingBox i v p, i ~ I m)
                      => (Show i, ToJSON i, Eq i, MonadThrow m, MonadIO m)
                      => l -> MapleAdmin -> i -> m (Map b [i])
moderationPlaceLootH local admin loot = do
  logAdminAction admin $ "place " <> show loot
  currentTime <- liftIO Time.getCurrentTime
  _ <- binTX (const $ pure True) (DBMeta local currentTime loot)
  let viewport = loot ^. boundingBox
  bs <- fmap snd <$> view (to (`overlapableBins` viewport))
  items <- mapM (\b -> (\is -> (b, fmap _dbmItem is)) <$> tip b) bs
  pure $ Map.fromList items

withCacheHeaders :: (MonadIO m, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
                 => AddHeader h CacheControlHeader orig res
                 => Lens' (MapleConfig g l s i v p b e) MapleCacheControl
                 -> m orig
                 -> m res
withCacheHeaders l act = do
  res <- act
  cacheControl <- view (mapleConfig . l)
  let
    secondsToExpire = mapleCacheControlMaxSeconds cacheControl
    cacheHeader = CacheControlHeader $ LText.toStrict $ LText.toLazyText $ "public, max-age=" <> LText.decimal secondsToExpire
  pure $ addHeader cacheHeader res

withExpiresHeaders :: (MonadIO m, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
                   => (AddHeader h HTTPDate orig res)
                   => Lens' (MapleConfig g l s i v p b e) MapleCacheControl
                   -> m orig
                   -> m res
withExpiresHeaders l act = do
  res <- act
  cacheControl <- view (mapleConfig . l)
  currentTime <- liftIO $ Time.getCurrentTime
  let
    secondsToExpire = mapleCacheControlMaxSeconds cacheControl
    timeToExpire = Time.addUTCTime (Time.secondsToNominalDiffTime (fromIntegral secondsToExpire)) currentTime
    expiresDate = utcToHTTPDate timeToExpire
  pure $ addHeader expiresDate res

withSurrogateHeader :: (Monad m, AddHeader h Text.Text orig b) => m orig -> m b
withSurrogateHeader act = do
  res <- act
  pure $ addHeader (Text.pack "max-age=3600") res

data MaplePausedException = MaplePausedException deriving (Show)

instance Exception MaplePausedException

guardOnPaused :: (MonadThrow m, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m) => m a -> m a
guardOnPaused act = do
  paused <- view $ mapleConfig . mcPaused
  when paused $ throwM MaplePausedException
  act

-- Verify the data in a session, throwing an error if validation fails
getSessionData :: (HasLocalPoolSize c, Serialize l, MonadIO m, FromJSON s, FromJSON i, MonadThrow m, HasClientKey c, HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m)
               => l -> Sessioned s i a -> m (SessionData s i, a)
getSessionData local unverifified = do
  session <- if Text.null sessionText
    then do
      lootBoxes <- view mcLootBoxes
      sessionInfo <- emptySessionData
      newHints <- sessionHints lootBoxes local sessionInfo
      pure $ sessionInfo & sdCurrentHints %~ mappend newHints
    else decodeSession $ Text.encodeUtf8 sessionText
  pure (session, sessionValue unverifified)
  where
    sessionText = sessionData unverifified

logAdminAction :: MonadIO m => MapleAdmin -> String -> m ()
logAdminAction admin str =
  liftIO $ hPutStrLn stderr $ (Text.unpack $ mapleAdminUsername admin) <> ": " <> str
