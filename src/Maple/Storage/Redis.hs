{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Maple.Storage.Redis where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Time
import qualified Data.Aeson as JS
import qualified Data.Bytes.Get as S
import qualified Data.Bytes.Put as S
import qualified Data.Bytes.Serial as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import           Data.Kind
import           Data.Maybe
import qualified Data.Serialize as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as V1
import           Data.Word
import qualified Database.Redis as Redis
import           Linear
import           Maple.Config
import           Maple.Map
import           Maple.Session
import           Safe
import           System.Random

data RedisEnv
 = RedisBinEnv
   { _rePool       :: Redis.Connection
   }

makeClassy ''RedisEnv

instance HasRedisEnv (MapleConfig g l s i v p b RedisEnv) where
  redisEnv = mcExtra

newtype RedisStoreT f g l s i (v::Type->Type) p b m a = RS { unRedisStore :: ReaderT (f RedisEnv) m a }
  deriving (Functor, Applicative, Monad, MonadReader (f RedisEnv), MonadError e, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail)
  deriving (MonadTrans)  via ReaderT (f RedisEnv)

runRedisStore' :: (HasMapleConfig (f RedisEnv) g l s i v p b RedisEnv, HasRedisEnv (f RedisEnv), HasBinMap (f RedisEnv) g v p b, MonadIO m) => (f RedisEnv) -> RedisStoreT f g l s i v p b m a -> m a
runRedisStore' ref (RS m) = runReaderT m ref

startRedisStore :: (Functor f, HasRedisEnv (f RedisEnv), HasBinMap (f e) g v p b, HasBinMap (f RedisEnv) g v p b, MonadIO m) => Redis.ConnectInfo -> f e -> m (f RedisEnv)
startRedisStore cinf c = do
  rp <- liftIO $Redis.checkedConnect cinf
  pure $ fmap (const $ RedisBinEnv rp) c

runRedisStore :: (Functor f, HasRedisEnv (f RedisEnv), HasBinMap (f e) g v p b, HasBinMap (f RedisEnv) g v p b, MonadIO m) => Redis.ConnectInfo -> f e -> RedisStoreT f g l s i v p b m a -> m a
runRedisStore cinf c (RS m) = do
  env <- startRedisStore cinf c
  m `runReaderT` env 

sessKey :: UUID -> BS.ByteString
sessKey u = mconcat [TE.encodeUtf8 $ T.pack "sess_", UUID.toASCIIBytes u]

collapseEithers :: Either e0 (Either e1 a) -> Maybe a
collapseEithers (Right (Right a)) = Just a
collapseEithers _ = Nothing

either2maybe :: Either e a -> Maybe a
either2maybe (Left _) = Nothing
either2maybe (Right a) = Just a

readSess :: (Redis.RedisCtx m f, Functor f) => UUID -> m (f (Maybe SessionState))
readSess u = (fmap $ join . fmap (either2maybe . C.decode)) <$> Redis.get (sessKey u)

localDiffKey :: Show l => l -> BS.ByteString
localDiffKey l = mconcat [TE.encodeUtf8 $ T.pack "locdif_", TE.encodeUtf8 $ T.pack $ show l]

localLastKey :: Show l => l -> BS.ByteString
localLastKey l = mconcat [TE.encodeUtf8 $ T.pack "loclast_", TE.encodeUtf8 $ T.pack $ show l]

readLocalDiff :: (Show l, Redis.RedisCtx m (Either Redis.Reply)) => l -> m Difficulty
readLocalDiff l = (fromRight 0 . fmap (fromMaybe 0 . join . fmap (readMay . T.unpack . TE.decodeUtf8))) <$> Redis.get (localDiffKey l)

binKey :: Word64 -> BS.ByteString
binKey b = mconcat [TE.encodeUtf8 $ T.pack "bin_", TE.encodeUtf8 $ T.pack $ show b]

readBin :: forall i . (JS.FromJSON i) => Word64 -> Redis.Redis (Either SomeException [(UUID, i)])
readBin b = do
  (bimap (const $ toException RedisError) (mapMaybe parseBin)) <$> Redis.hgetall (binKey b)
  where
    parseBin :: (BS.ByteString, BS.ByteString) -> Maybe (UUID, i)
    parseBin (bsu, bsi) = do
      u <- UUID.fromASCIIBytes bsu
      i <- JS.decodeStrict' bsi
      pure (u, i)

newtype SetWatch f g l s i (v::Type->Type) p b a
  = SWT { unSWT :: ReaderT (f RedisEnv) Redis.Redis a }
  deriving (Functor, Applicative, Monad, MonadReader (f RedisEnv), MonadFail)

instance (HasPos i v p, HasRedisEnv (f RedisEnv), HasBinMap (f RedisEnv) g v p Word64, HasBinMap g g v p Word64, Ord p, Eq (v p), Traversable v, Additive v, JS.FromJSON i, Show l, JS.FromJSON l)  =>  BinReader (f RedisEnv) g l v p Word64 (SetWatch f g l s i v p Word64) where
  type I (SetWatch f g l s i v p Word64) = i

  tip b = do
    void $ SWT $ lift $ Redis.watch [binKey b]
    r <- SWT $ lift $ readBin b
    case r of
      Left _ -> pure []
      Right t -> pure $ map snd t

  -- | Not correct, but we shouldn't be using this.
  readSession _ = pure $ 0

  readLocalDifficulty _ = pure $ 0

instance (HasPos i v p, HasRedisEnv (f RedisEnv), HasBinMap (f RedisEnv) g v p Word64, HasBinMap g g v p Word64, Ord p, Eq (v p), Traversable v, Additive v, MonadThrow m, MonadIO m, JS.FromJSON i, Show l, JS.FromJSON l) => BinReader (f RedisEnv) g l v p Word64 (RedisStoreT f g l s i v p Word64 m) where
  type I (RedisStoreT f g l s i v p Word64 m) = i

  tip b = do
    p <- view rePool
    r <- liftIO $ Redis.runRedis p $ readBin b
    case r of
      Left e -> throwM e
      Right t -> pure $ map snd t

  readSession u = do
    p <- view rePool
    mss <- liftIO $ Redis.runRedis p $ readSess u
    case mss of
      Left _ -> throwM RedisError
      Right Nothing  -> throwM SessionFraud
      Right (Just SessFraud) -> throwM SessionFraud
      Right (Just (SessGeneration g)) -> pure g

  readLocalDifficulty l = do
    p <- view rePool
    liftIO $ Redis.runRedis p $ readLocalDiff l

data RedisError = RedisError deriving (Show)

instance Exception RedisError

instance Functor Redis.TxResult where
  fmap f (Redis.TxSuccess a) = Redis.TxSuccess (f a)
  fmap _ Redis.TxAborted = Redis.TxAborted
  fmap _ (Redis.TxError e) = Redis.TxError e

instance (HasPos i v p, HasRedisEnv (f RedisEnv), HasBinMap (f RedisEnv) g v p Word64, HasBinMap g g v p Word64, Ord p, Eq (v p), Traversable v, Additive v, MonadThrow m, MonadFail m, MonadIO m, JS.FromJSON i, JS.ToJSON i, Show l, JS.FromJSON l, JS.ToJSON l) => BinStorage (f RedisEnv) g l v p Word64 (RedisStoreT f g l s i v p Word64 m) where
  checkDB = do
    p <- view rePool
    ep <- liftIO $ Redis.runRedis p $ Redis.ping
    case ep of
      Left _ -> throwM RedisError
      Right Redis.Ok -> pure ()
      Right Redis.Pong -> pure ()
      Right (Redis.Status _) -> throwM RedisError

  binFilter b f = do
    p <- view rePool
    ebm <- liftIO $ Redis.runRedis p $ readBin b
    case ebm of
      Left _ -> throwM RedisError
      Right bm -> do
        void $ liftIO $ Redis.runRedis p $ Redis.hdel (binKey b) $ (`mapMaybe` bm) $ \(u, i) -> do
          if f i
            then Nothing
            else Just $ UUID.toASCIIBytes u

  binTX chk i = do
    env <- view id
    mb <- view $ (binMap.to (`homeBin` i))
    p <- view rePool
    Just u <- liftIO $ V1.nextUUID
    case mb of
      Nothing -> pure False
      Just b -> runTx 0
        where
          runTx tries = do
            txr <- liftIO $ Redis.runRedis p $ do
              cont <- (unSWT $ (chk i::SetWatch f g l s i v p Word64 Bool)) `runReaderT` env
              case cont of
                False -> pure Nothing
                True  -> fmap Just $ Redis.multiExec $ Redis.hset (binKey b) (UUID.toASCIIBytes u) (BL.toStrict $ JS.encode i)
            case txr of
              Nothing                  -> pure False
              Just (Redis.TxSuccess _) -> pure True
              Just Redis.TxAborted | tries <= 5 -> do
                                  liftIO $ randomRIO (0, ((1.4::Double)^(tries::Word))/10) >>= delay
                                  runTx (tries+1)
              Just Redis.TxAborted              -> pure False 
              Just (Redis.TxError _)   -> throwM RedisError

  setSession u mo n = do
    p <- view rePool
    let sk = sessKey u
    mf <- liftIO $ Redis.runRedis p $ do
      emo <- Redis.getset sk (C.encode $ SessGeneration n)
      case fmap (fmap C.decode) $ emo of
        Left _ -> pure $ Nothing
        -- It didn't exist, so we have to trust it
        Right Nothing -> pure $ Just False
        -- We couldn't decode so we screwed up, trust it.
        Right (Just (Left _)) -> pure $ Just False
        -- Everything was right
        Right (Just (Right (SessGeneration g))) | (Just g) == mo -> pure $ Just False
        -- They're from the future, so we lost a write?
        Right (Just (Right (SessGeneration g))) | (Just g) < mo  -> pure $ Just False
        -- g must be larger, or we had no expectation, but it was in the DB, so thats bad.
        Right (Just (Right (SessGeneration _))) -> do
          void $ Redis.set sk (C.encode SessFraud)
          pure $ Just True
        -- Fraud stays fraud.
        Right (Just (Right SessFraud)) -> do
          void $ Redis.set sk (C.encode SessFraud)
          pure $ Just True
    case mf of
      Nothing -> throwM RedisError
      Just True -> throwM SessionFraud
      Just False -> pure ()

  incLocalDifficulty l d = do
    p <- view rePool
    void . liftIO $ Redis.runRedis p $ Redis.incrby (localDiffKey l) (fromIntegral d)

  tryPlace l nd = do
    now <- liftIO $ getCurrentTime
    p <- view rePool
    transact p (localLastKey l) now (0::Word64)
    where
      transact _ _ _ cnt | cnt >= 10 = throwM RedisError
      transact p lk now cnt = do
        es <- liftIO $ Redis.runRedis p $ do
            void $ Redis.watch [lk]
            molt <- (join . collapseEithers . fmap (sequenceA . fmap (S.runGetS S.deserialize))) <$> Redis.get lk
            fmap (const molt) <$>  Redis.multiExec (Redis.set lk (S.runPutS $ S.serialize now))
        case es of
          Redis.TxSuccess molt ->
            pure $ case molt of
                     Nothing -> Nothing
                     Just lt | (nd `addUTCTime` lt) < now -> Nothing
                     Just _ -> Just $ nd `addUTCTime` now
          Redis.TxAborted -> transact p lk now (cnt+1)
          Redis.TxError _ -> throwM RedisError
