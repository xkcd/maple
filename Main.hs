{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens
import           Control.Monad.State
import           Data.Time
import qualified Data.Vector as V
import           Data.Word
import           Linear
import           Linear.Affine
import           Maple.AABB
import           Maple.Config
import           Maple.Loot
import           Maple.Map
import           Maple.Storage.Ref
import           Maple.Web
import           Maple.Web.Admin
import           Maple.Web.API
import           Maple.Web.Local
import           Network.Wai.Handler.Warp
import           System.Random
import qualified Web.ClientSession as CS

main :: IO ()
main = do
  (_, key) <- CS.randomKey
  let safeFilePaths = ["loot"]
  (mbb, l, adminLoot) <- loadLoot filePath3D [atExactly] safeFilePaths "test_loot.csv"
  -- adminCheck <- authCheckFromFile "admins.txt"
  let
    config = MC
          { _mcCSKey     = key
          , _mcGrigMap   = mkMap { _igmMaxBB = mbb }
          , _mcLootBoxes = l
          , _mcMkLocal   = ipv4Local
          , _mcPoolSize  = 20
          , _mcBinListCachePolicy = MapleCacheControl 60 -- 60 second cache policy
          , _mcBinContentsCachePolicy = MapleCacheControl 5 -- 5 second cache policy
          , _mcImageFolders = safeFilePaths
          , _mcCheckAdmin = nullAdminCheck
          , _mcAdminLoot = adminLoot
          , _mcPaused    = False
          , _mcAllowedOrigins = []
          , _mcExtra     = ()
          }
  let (AABB (V2 minMapX minMapY) (V2 maxMapX maxMapY)) =
        config^.mcGrigMap.igmBBs.to (bounding . concatMap (corners . box2d . fst) . concatMap V.toList . V.toList)
  let randomPosInBounds = fmap P . liftIO $ V3 <$> randomRIO (minMapX, maxMapX) <*> randomRIO (minMapY, maxMapY) <*> randomRIO (0, 10)
  let randomLoot = liftIO $ fmap (\i -> adminLoot!!i) $ randomRIO (0, length adminLoot-1)
  cfg <- startBinStore config
  runRefBinStore' cfg $ do
    forM_ [1..(10000::Word64)] $ \_ -> do
      tp <- randomPosInBounds
      lt <- randomLoot
      now <- liftIO $ getCurrentTime
      void $ binTX doesNotOverlap (DBMeta 0 now (lt & pos .~ tp))
  putStrLn "Serving"
  run 8000 $ serveMapleAPI cfg (runRefBinStore' cfg)

mkMap :: IGridMap2 V3 Int Word
mkMap = fst $
  (`runState` 0) $ buildMap (\b -> state (\c -> ((b, c), c+1)))
                            (AABB (V3 0 0 0) (V3 0 0 0))
                            (P (V2 0 0))
                            (V2 500 500)
                            80 20
