{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Maple.Storage.Ref where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.Fail
import           Control.Monad.Loops
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Time
import           Data.UUID
import           Linear
import           Maple.Config
import           Maple.Map
import           Maple.Session

data RefBinEnv r l b i
 = RefBinEnv
   { _rbeLock    :: r Bool
   , _rbeBins    :: Map b (r [DBMeta l i])
   , _rbeSess    :: r (Map UUID SessionState)
   , _rbeLocDif  :: r (Map l Difficulty)
   , _rbeLocLast :: r (Map l UTCTime)
   }

makeClassy ''RefBinEnv

instance HasRefBinEnv (MapleConfig g l s i v p b (RefBinEnv r l b i)) r l b i where
  refBinEnv = mcExtra

newtype RefBinStoreT r f g l s i (v::Type->Type) p b m a = RBS { unRefBinStore :: ReaderT (f (RefBinEnv r l b i)) m a }
  deriving (Functor, Applicative, Monad, MonadReader (f (RefBinEnv r l b i)), MonadError e, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail)
  deriving (MonadTrans)  via ReaderT (f (RefBinEnv r l b i))

runRefBinStore' :: (HasRefBinEnv (f (RefBinEnv r l b i)) r l b i, HasBinMap (f (RefBinEnv r l b i)) g v p b, MonadRef r m) => (f (RefBinEnv r l b i)) -> RefBinStoreT r f g l s i v p b m a -> m a
runRefBinStore' ref (RBS m) = runReaderT m ref

startBinStore :: (Functor f, HasRefBinEnv (f (RefBinEnv r l b i)) r l b i, HasBinMap (f e) g v p b, HasBinMap (f (RefBinEnv r l b i)) g v p b, Ord l, MonadRef r m) => f e -> m (f (RefBinEnv r l b i))
startBinStore c = do
  l <- newRef True
  br <- forM (c^.allBins) $ \i -> (i,) <$> newRef []
  s <- newRef mempty
  d <- newRef mempty
  t <- newRef mempty
  pure $ fmap (const $ RefBinEnv l (Map.fromList br) s d t) c

runRefBinStore :: (Functor f, HasRefBinEnv (f (RefBinEnv r l b i)) r l b i, HasBinMap (f e) g v p b, HasBinMap (f (RefBinEnv r l b i)) g v p b, Ord l, MonadRef r m) => f e -> RefBinStoreT r f g l s i v p b m a -> m a
runRefBinStore c (RBS m) = do
  env <- startBinStore c
  m `runReaderT` env 

instance (HasPos i v p, HasRefBinEnv (f (RefBinEnv r l b i)) r l b i, HasBinMap (f (RefBinEnv r l b i)) g v p b, HasBinMap g g v p b, Ord p, Eq (v p), Traversable v, Additive v, Ord l, MonadAtomicRef r m, MonadThrow m) => BinReader (f (RefBinEnv r l b i)) g l v p b (RefBinStoreT r f g l s i v p b m) where
  type I (RefBinStoreT r f g l s i v p b m) = i

  tip b = do
    view (rbeBins.at b) >>= maybe (pure []) (lift . readRef)

  readSession u = view rbeSess >>= (fmap (Map.lookup u) . lift . readRef) >>= maybe (throwM SessionFraud) getGenM

  readLocalDifficulty l = view rbeLocDif >>= (fmap (Map.findWithDefault 0 l) . lift . readRef)

instance (HasPos i v p, HasRefBinEnv (f (RefBinEnv r l b i)) r l b i, HasBinMap (f (RefBinEnv r l b i)) g v p b, HasBinMap g g v p b, Ord p, Eq (v p), Traversable v, Additive v, Ord l, MonadMask m, MonadAtomicRef r m, MonadThrow m, MonadIO m) => BinStorage (f (RefBinEnv r l b i)) g l v p b (RefBinStoreT r f g l s i v p b m) where
  checkDB = pure () -- always up

  binFilter b f =
    view (rbeBins.at b) >>= maybe (pure ()) (\r -> lift $ atomicModifyRef' r ((,()) . filter f))

  binTX chk i = do
    mb <- view $ (binMap.to (`homeBin` i))
    l  <- view rbeLock
    case mb of
      Nothing -> pure False
      Just b -> do
        mbr <- view (rbeBins.at b)
        case mbr of
          Nothing -> pure False
          Just br -> do
            bracket_ (pure ()  `untilM_` (lift $ atomicModifyRef' l (\case { True -> (False, True); False -> (False, False)})))
                     (lift $ writeRef l True) $ do
                       g <- chk i
                       case g of
                         False -> pure False
                         True -> lift $ atomicModifyRef' br (\bc -> (i:bc, True))

  setSession u mo n = do
    sm <- view rbeSess
    f <- lift $ atomicModifyRef' sm (\os ->
                                let mov = Map.lookup u os
                                in case mov of
                                     -- If we don't have the data, we're forced into blind acceptance.
                                     Nothing -> (Map.insert u (SessGeneration n) os , False)
                                     -- Everything matches.
                                     Just (SessGeneration g) | (Just g) == mo -> (Map.insert u (SessGeneration n) os , False)
                                     -- If they're from the future, we missed a write.
                                     Just (SessGeneration g) | (Just g) <  mo -> (Map.insert u (SessGeneration n) os , False)
                                     --  By process of elimination, their session is older and is thus a replay.
                                     Just (SessGeneration _) -> (Map.insert u SessFraud os , True)
                                     -- Fraud stays fraud
                                     Just SessFraud -> (os, True)
                             )
    case f of
      True -> throwM SessionFraud
      False -> pure ()

  incLocalDifficulty l d = do
    view rbeLocDif >>= lift . (`atomicModifyRef'` (\lm -> (Map.insertWith (+) l d lm, ())))

  tryPlace l nd = do
    now <- liftIO $ getCurrentTime
    r <- view rbeLocLast
    lift . atomicModifyRef' r $ \lm ->
      case Map.lookup l lm of
        Nothing -> (Map.insert l now lm, Nothing)
        Just lt | (nd `addUTCTime` lt) < now -> (Map.insert l now lm, Nothing)
        Just _ -> (Map.insert l now lm, Just $ nd `addUTCTime` now)
