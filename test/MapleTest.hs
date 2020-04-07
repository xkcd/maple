{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           AABBTest
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Loops
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Time
import qualified Data.Aeson as JS
import           Data.Foldable
import           Data.IORef
import           Data.Kind
import           Data.List (sort, intercalate, isInfixOf)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import qualified Data.UUID.V1 as V1
import qualified Data.Vector as V
import           Data.Word
import qualified Database.Redis as Redis
import           GHC.IO.Handle (hPutStr, hClose, hGetLine)
import           Linear
import           Linear.Affine
import           Main.Utf8 (withUtf8)
import           Maple.AABB
import           Maple.Config
import           Maple.Loot
import           Maple.Map
import           Maple.Session
import           Maple.Web.Session
import           Maple.Storage.Redis
import           Maple.Storage.Ref
import           Maple.TextHintParser
import           Maple.Web
import           Maple.Web.Admin
import           Maple.Web.API
import           Maple.Web.Local
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Test
import           ParserTests
import           System.Process
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients
import           Test.Tasty.Ingredients.Basic
import           Test.Tasty.QuickCheck
import qualified Test.Tasty.Runners.Reporter as Reporter
import           Web.ClientSession

type MapleTestM' c g l s i v p b e m =
  (HasMapleConfig c g l s i v p b e, BinStorage c g l v p b m, HasClientKey c, HasLocalPoolSize c, MonadThrow m, MonadFail m, MonadIO m, I m ~ ImgLoot V3 Int)

type MapleTestM c m e = MapleTestM' c (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64 e m

type MapleTestConfig = MapleConfig (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64

data SimpleConfig g l s i (v::Type->Type) p b e
 = SC
   { _scGridMap :: g
   , _scMkLocal :: MakeMapleLocal l
   , _scExtra   :: e
   }

makeLenses ''SimpleConfig

instance Functor (SimpleConfig g l s i v p b) where
  fmap f d = d { _scExtra = f (_scExtra d) }

instance HasBinMap g g v p b => HasBinMap (SimpleConfig g l s i v p b e) g v p b where
  binMap = scGridMap

instance HasRefBinEnv (SimpleConfig g l s i v p b (RefBinEnv r l b i)) r l b i where
  refBinEnv = scExtra

instance HasRedisEnv (SimpleConfig g l s i v p b RedisEnv) where
  redisEnv = scExtra

instance HasMapleConfig (SimpleConfig g l s i v p b e) g l s i v p b e where
  mcMkLocal = scMkLocal
  mapleConfig = error "Simple config mapleConfig not implemented"

makeTestMapleConfig :: Key -> MapleConfig (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64 ()
makeTestMapleConfig k =
  MC {
      _mcCSKey = k
    , _mcGrigMap = mkMap
    , _mcLootBoxes = Map.empty
    , _mcMkLocal = ipv4Local
    , _mcPoolSize = 1
    , _mcBinListCachePolicy = MapleCacheControl 60 -- 60 second cache policy
    , _mcBinContentsCachePolicy = MapleCacheControl 5 -- 5 second cache policy 
    , _mcImageFolders = []
    , _mcAdminLoot = []
    , _mcCheckAdmin = MapleAdminCheck (const Nothing)
    , _mcPaused = False
    , _mcAllowedOrigins = []
    , _mcExtra = ()
    }

runRefMapleTest :: Key -> RefBinStoreT IORef MapleTestConfig (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64 IO a -> IO a
runRefMapleTest k = runRefBinStore (makeTestMapleConfig k)

rR :: RefBinStoreT IORef (SimpleConfig (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64) (IGridMap2 V3 Int Word64) Word32 [Text] (ImgLoot V3 Int) V3 Int Word64 IO a -> IO a
rR = runRefBinStore rRConfig

rRConfig :: SimpleConfig (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64 ()
rRConfig = SC mkMap ipv4Local ()

rD :: Key -> IO Redis.ConnectInfo -> RedisStoreT MapleTestConfig (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64 IO a -> IO a
rD key mkRedisConn act = do
  connInfo <- mkRedisConn
  runRedisStore connInfo (makeTestMapleConfig key) $ act

redisConfig :: String -> String
redisConfig pass = intercalate "\n"
  [ "protected-mode yes"
  , "port 0"
  , "daemonize no"
  , "supervised no"
  , "loglevel notice"
  , "always-show-logo no"
  , "databases 128"
  , "logfile \"\""
  , "dbfilename \"\""
  , "# This just exists to make sure we use auth correctly in the tests."
  , "requirepass "++pass
  , "# This is so we error if tests go off the rails"
  , "maxmemory 512MiB"
  , "maxmemory-policy noeviction"
  , ""
  , "unixsocket redis.sock"
  , "unixsocketperm 700"
  ]

mkPassword :: IO String
mkPassword = forM [1..20::Word] $ const $ randomRIO ('a', 'z')

-- | Generates Redis ConnectionInfos for sequential DBs
redisConnectParams :: String -> IO Integer -> IO Redis.ConnectInfo
redisConnectParams pass dbNumGen = do
  dbNum <- dbNumGen
  pure $ Redis.defaultConnectInfo
    { Redis.connectPort = Redis.UnixSocket "redis.sock"
    , Redis.connectAuth = Just $ TE.encodeUtf8 $ T.pack pass
    , Redis.connectDatabase = dbNum
    }

withRedis :: (IO Redis.ConnectInfo -> IO a) -> IO a
withRedis act = do
  pass <- mkPassword
  withCreateProcess ((proc "redis-server" ["-"]) {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,close_fds=True}) $
    \(Just redis_stdin) (Just redis_stdout) Nothing _ -> do
      hPutStr redis_stdin (redisConfig pass) >> hClose redis_stdin
      -- Make sure the server is online before we go further.
      -- Slight worry that we stop reading here if it keeps writing.
      void $ iterateUntil ("Server initialized" `isInfixOf`) (hGetLine redis_stdout)
      r <- newIORef 0
      act $ redisConnectParams pass (atomicModifyIORef' r (\v -> (v+1, v)))

main :: IO ()
main = withUtf8 $ do
  (_, key) <- randomKey
  withRedis $ \mkRedisConn ->
    defaultMainWithIngredients [consoleTestReporter `composeReporters` Reporter.ingredient, listingTests] $
    testGroup "Maple Tests"
    [ aabbTests
    , gridTests
    , lootTests
    , localPoolTests
    , sessTests
    , testGroup "storage backends"
      [ testGroup "in memory" $ storageTests (runRefMapleTest key)
      , testGroup "redis" $ storageTests $ rD key mkRedisConn
      ]
    , testHintParser
    ]
 where
   storageTests :: MapleTestM c m e => (forall a . m a -> IO a) -> [TestTree]
   storageTests rS =
     [ storageSessionTests rS
     , storageLocalTests rS
     , binTests rS
     ]

sessTests :: TestTree
sessTests = testGroup "session"
  [ testCase "To/FromJSON" $ do
      (_, l, is) <- loadLoot filePath3D [atExactly] ["loot"] "test_loot.csv"
      Just u <- V1.nextUUID
      let esess = SessionData u 0 ([]::[Text]) ([]::[ImgLoot V3 Int]) [] []
      (Just esess) @?= (JS.decode $ JS.encode esess)
      let sess = SessionData u 0 ["a"::Text] [head is] (Map.keys l) []
      (Just sess) @?= (JS.decode $ JS.encode sess)
  ]

lootTests :: TestTree
lootTests =
  testGroup "Loot" [
      testCase "Image loot translation base case" $ do
        i <- readImgLoot filePath3D "loot/1/A.png"
        isTranslatedLoot i i @?= True
    , testProperty "Image loot translation by arbitrary vector" $ \(v1 :: V3 Int) (v2 :: V3 Int) v ->
        let
          i = ImgLoot { _ilAABB = (bound v1 v2), _ilImg = "image" }
          i' = i & boundingBox %~ (.+^ v)
        in
          isTranslatedLoot i i' === True
    , testCase "readImgLoot" $ do
        i <- readImgLoot filePath3D "loot/1/A.png"
        i @?= (ImgLoot (AABB (V3 0 0 1) (V3 30 30 1)) "loot/1/A.png")
    , testCase "loadLoot" $ do
        l <- Map.elems . (view _2) <$> loadLoot filePath3D [atExactly] ["loot"] "test_loot.csv"
        (length l) @?= 7
        (((head l)^.ldTumbler) `runReaderT` ["d"]) @?= Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/D.png"})
        (((head l)^.ldTumbler) `runReaderT` ["a"]) @?= Nothing
    , testCase "load loot combines" $ do
        l <- Map.elems . (view _2) <$> loadLoot filePath3D [atExactly] ["loot"] "test_loot_combine.csv"
        (length l) @?= 1
        (((head l)^.ldTumbler) `runReaderT` ["a"]) @?= Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/A.png"})
        (((head l)^.ldTumbler) `runReaderT` ["A"]) @?= Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/A.png"})
    , testCase "group load" $ do
      grps <- loadGroups ["loot"] "test_groups.csv"
      l <- loadGrouped parseTextTumbler grps "test_grouped.csv"
      (Map.keys l) @?= ["XFJzAoTujqP6kQPyQAvu6g==","sMMaT9IYcP-HBOuNqz01iw=="]
      (((l Map.! "XFJzAoTujqP6kQPyQAvu6g==")^.ldTumbler) `runReaderT` ["A"]) @?=
        Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 2, _maxCorner = V3 30 30 2}, _ilImg = "loot/1/A.png"})
      (((l Map.! "XFJzAoTujqP6kQPyQAvu6g==")^.ldTumbler) `runReaderT` ["A","C"]) @?=
        Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/A.png"})
      (((l Map.! "sMMaT9IYcP-HBOuNqz01iw==")^.ldTumbler) `runReaderT` ["e","t"]) @?=
        Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/E.png"})
  , testCase "from2X2D" $ do
      (from2X2D $ ImgLoot {_ilAABB = AABB {_minCorner = V3 (0::Int) 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/E.png"}) @?=
        (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 15 15 1}, _ilImg = "loot/1/E.png"})
      (from2X2D $ ImgLoot {_ilAABB = AABB {_minCorner = V3 (0::Int) 0 2, _maxCorner = V3 30 30 2}, _ilImg = "loot/1/E.png"}) @?=
        (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 2, _maxCorner = V3 15 15 2}, _ilImg = "loot/1/E.png"})
  , testCase "readImgLoot" $ do
      i <- readImgLoot filePath3D "loot/1/A.png"
      i @?= (ImgLoot (AABB (V3 0 0 1) (V3 30 30 1)) "loot/1/A.png")
  , testCase "loadLoot" $ do
      l <- Map.elems . (view _2) <$> loadLoot filePath3D [atExactly] ["loot"] "test_loot.csv"
      (length l) @?= 7
      (((head l)^.ldTumbler) `runReaderT` ["d"]) @?= Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/D.png"})
      (((head l)^.ldTumbler) `runReaderT` ["a"]) @?= Nothing
  , testCase "load loot combines" $ do
      l <- Map.elems . (view _2) <$> loadLoot filePath3D [atExactly] ["loot"] "test_loot_combine.csv"
      (length l) @?= 1
      (((head l)^.ldTumbler) `runReaderT` ["a"]) @?= Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/A.png"})
      (((head l)^.ldTumbler) `runReaderT` ["A"]) @?= Just (ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/A.png"})
  ]

gridTests :: TestTree
gridTests = testGroup "Grid"
  [ testCase "To/From JSON" $ do
      let m = mkMap
      (Just m) @?= (JS.decode $ JS.encode m)
  , testCase "buildMap sane" $ do
      let (gm::IGridMap2 V3 Int Word64) = fst $ (`runState` 0) $ buildMap (\b -> state (\c -> ((b, c), c+1)))
                                          (AABB (V3 0 0 0) (V3 0 0 0))
                                          (P (V2 0 0))
                                          (V2 100 100)
                                          2 2
      gm @?= (IGMap (AABB (V3 0 0 0) (V3 0 0 0))
               (V.fromList [V.fromList [(AABB (V2 0 0) (V2 100 100), 0), (AABB (V2 0 100) (V2 100 200), 1)]
                           ,V.fromList [(AABB (V2 100 0) (V2 200 100), 2), (AABB (V2 100 100) (V2 200 200), 3)]
                           ]))
  , testCase "extends same as buildMap" $ do
      let (bm::IGridMap2 V3 Int Word64) = fst $ (`runState` 0) $ buildMap (\b -> state (\c -> ((b, c), c+1)))
                                          (AABB (V3 0 0 0) (V3 0 0 0))
                                          (P (V2 0 0))
                                          (V2 100 100)
                                          2 2
      let em = fst $ (`runState` 0) $ igridmap2ExtendX incStateGen =<< igridmap2ExtendY incStateGen =<< singletonIGridMap2 incStateGen (AABB (V3 0 0 0) (V3 0 0 0)) (AABB (V2 0 0) (V2 100 100))
      em @?= bm
  , testCase "extends further same as buildMap" $ do
      let (bm::IGridMap2 V3 Int Word64) = fst $ (`runState` 0) $ buildMap (\b -> state (\c -> ((b, c), c+1)))
                                          (AABB (V3 0 0 0) (V3 0 0 0))
                                          (P (V2 0 0))
                                          (V2 100 100)
                                          4 4
      let em = fst $ (`runState` 0) $
            igridmap2ExtendX incStateGen =<< igridmap2ExtendX incStateGen =<<
            igridmap2ExtendX incStateGen =<<
            igridmap2ExtendY incStateGen =<< igridmap2ExtendY incStateGen =<<
            igridmap2ExtendY incStateGen =<<
            singletonIGridMap2 incStateGen (AABB (V3 0 0 0) (V3 0 0 0)) (AABB (V2 0 0) (V2 100 100))
      em @?= bm
  , testCase "Every tile in grid is its self" $ do
      let (bm::IGridMap2 V3 Int Word64) = fst $ (`runState` 0) $ buildMap (\b -> state (\c -> ((b, c), c+1)))
                                          (AABB (V3 0 0 0) (V3 0 0 0))
                                          (P (V2 0 0))
                                          (V2 100 100)
                                          4 4
      mapM_ (\(bb@(AABB (V2 lx ly) (V2 hx hy)), b) -> [(bb, b)] @=? (overlapableBins bm (AABB (V3 lx ly 0) (V3 hx hy 0)))) $
           concatMap toList $ toList $ _igmBBs bm
  , testCase "Every tile in grid is its self even when extending negative" $ do
      let (bm::IGridMap2 V3 Int Word64) = fst $ (`runState` 0) $
            igridmap2ExtendXNeg incStateGen =<< igridmap2ExtendXNeg incStateGen =<<
            igridmap2ExtendXNeg incStateGen =<<
            igridmap2ExtendYNeg incStateGen =<< igridmap2ExtendYNeg incStateGen =<<
            igridmap2ExtendYNeg incStateGen =<<
            singletonIGridMap2 incStateGen (AABB (V3 0 0 0) (V3 0 0 0)) (AABB (V2 0 0) (V2 100 100))
      mapM_ (\(bb@(AABB (V2 lx ly) (V2 hx hy)), b) -> [(bb, b)] @=? (overlapableBins bm (AABB (V3 lx ly 0) (V3 hx hy 0)))) $
           concatMap toList $ toList $ _igmBBs bm
      let (gm::IGridMap2 V3 Int Word64) = IGMap
            { _igmMaxBB = AABB {_minCorner = V3 0 0 0, _maxCorner = V3 0 0 0}
            , _igmBBs = V.fromList $ map V.fromList $
              [[(AABB {_minCorner = V2 (-300) (-300), _maxCorner = V2 (-200) (-200)},12)
               ,(AABB {_minCorner = V2 (-300) (-200), _maxCorner = V2 (-200) (-100)},13)
               ,(AABB {_minCorner = V2 (-300) (-100), _maxCorner = V2 (-200) 0},14)
               ,(AABB {_minCorner = V2 (-300) 0, _maxCorner = V2 (-200) 100},15)
               ]
              ,[(AABB {_minCorner = V2 (-200) (-300), _maxCorner = V2 (-100) (-200)},8)
               ,(AABB {_minCorner = V2 (-200) (-200), _maxCorner = V2 (-100) (-100)},9)
               ,(AABB {_minCorner = V2 (-200) (-100), _maxCorner = V2 (-100) 0},10)
               ,(AABB {_minCorner = V2 (-200) 0, _maxCorner = V2 (-100) 100},11)
               ]
              ,[(AABB {_minCorner = V2 (-100) (-300), _maxCorner = V2 0 (-200)},4)
               ,(AABB {_minCorner = V2 (-100) (-200), _maxCorner = V2 0 (-100)},5)
               ,(AABB {_minCorner = V2 (-100) (-100), _maxCorner = V2 0 0},6)
               ,(AABB {_minCorner = V2 (-100) 0, _maxCorner = V2 0 100},7)
               ]
              ,[(AABB {_minCorner = V2 0 (-300), _maxCorner = V2 100 (-200)},3)
               ,(AABB {_minCorner = V2 0 (-200), _maxCorner = V2 100 (-100)},2)
               ,(AABB {_minCorner = V2 0 (-100), _maxCorner = V2 100 0},1)
               ,(AABB {_minCorner = V2 0 0, _maxCorner = V2 100 100},0)
               ]
              ]
            }
      bm @?= gm

  ]
  where
    incStateGen = state (\c -> (c, c+1))

runWithPoolSize :: LocalPoolSize -> RefBinStoreT IORef MapleTestConfig (IGridMap2 V3 Int Word64) Word32 Text (ImgLoot V3 Int) V3 Int Word64 IO a -> IO a
runWithPoolSize lps act = do
  (_, key) <- randomKey
  runRefBinStore ((makeTestMapleConfig key) & mcPoolSize .~ lps) act

localPoolTests :: TestTree
localPoolTests = testGroup "local pool"
  [ testCase "localPool" $ do
      let loc = 0
      l <- (view _2) <$> loadLoot filePath3D [atExactly] ["loot"] "test_loot.csv"
      p0 <- runWithPoolSize (LPS 2) $ localPool l loc
      p0 @?= ["NVNvA-6YGTPxeECfsMdkpg==","dQoQZy_yxSuYDhkZkUXeuw=="]
      p1 <- runWithPoolSize (LPS 3) $ localPool l loc
      p1 @?= ["NVNvA-6YGTPxeECfsMdkpg==","dQoQZy_yxSuYDhkZkUXeuw==","ZOpPFeF8CTNsupUSVtiGsQ=="]
  , testCase "dropLoot" $ do
      l <- (view _2) <$> loadLoot filePath3D [atExactly] ["loot"] "test_loot.csv"
      let sess = SessionData undefined 0 ["a"] [] (Map.keys l) []
      (dropLoot l sess) @?= [("dQoQZy_yxSuYDhkZkUXeuw==", ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/A.png"})]
      (dropLoot l (sess {_sdKeyHistory=["b"]})) @?= [("ZOpPFeF8CTNsupUSVtiGsQ==",ImgLoot {_ilAABB = AABB {_minCorner = V3 0 0 1, _maxCorner = V3 30 30 1}, _ilImg = "loot/1/B.png"})]
      (dropLoot l (sess {_sdKeyHistory=["z"]})) @?= []
  ]

storageLocalTests :: MapleTestM c m e => (forall a . m a -> IO a) -> TestTree
storageLocalTests rS = testGroup "Local Storage"
  [ testCase "can write difficulty" $ rS $ do
      let l = 0
      r <- incLocalDifficulty l 0
      liftIO $ r @?= ()
  , testCase "can read difficulty" $ rS $ do
      let l = 0
      r <- readLocalDifficulty l
      liftIO $ r @?= 0
  , testCase "can manage difficulty" $ rS $ do
      let l = 0
      incLocalDifficulty l 1
      r0 <- readLocalDifficulty l
      liftIO $ r0 @?= 1
      incLocalDifficulty l 2
      r1 <- readLocalDifficulty l
      liftIO $ r1 @?= 3
  , testCase "Starts able to place" $ rS $ do
      let l = 0
      r <- tryPlace l 0.1
      liftIO $ (isNothing r) @?= True
  , testCase "Starts can't place for a bit" $ rS $ do
      let l = 0
      r0 <- tryPlace l 0.1
      liftIO $ (isNothing r0) @?= True
      r1 <- tryPlace l 0.1
      liftIO $ (isNothing r1) @?= False
  , testCase "gets to place again" $ rS $ do
      let l = 0
      r0 <- tryPlace l 0.1
      liftIO $ (isNothing r0) @?= True
      r1 <- tryPlace l 0.1
      liftIO $ (isNothing r1) @?= False
      delay (0.1::Double)
      r2 <- tryPlace l 0.1
      liftIO $ (isNothing r2) @?= True
  ]

storageSessionTests :: MapleTestM c m e => (forall a . m a -> IO a) -> TestTree
storageSessionTests rS = testGroup "Session Storage"
  [ testCase "can write" $ rS $ do
      Just u <- liftIO $ V1.nextUUID
      r <- setSession u Nothing 0
      liftIO $ r @?= ()
  , testCase "can read" $ rS $ do
      Just u <- liftIO $ V1.nextUUID
      setSession u Nothing 0
      r0 <- readSession u
      liftIO $ r0 @?= 0
      setSession u (Just 0) 1
      r1 <- readSession u
      liftIO $ r1 @?= 1
  , testCase "Ok if client gen newer" $ rS $ do
      Just u <- liftIO $ V1.nextUUID
      setSession u Nothing 0
      r0 <- readSession u
      liftIO $ r0 @?= 0
      setSession u (Just 4) 5
      r1 <- readSession u
      liftIO $ r1 @?= 5
  , testCase "Throws on not new" $ (`shouldThrow` (\SessionFraud -> True)) $ rS $ do
      Just u <- liftIO $ V1.nextUUID
      setSession u Nothing 0
      setSession u Nothing 0
  , testCase "Throws on replay" $ (`shouldThrow` (\SessionFraud -> True)) $ rS $ do
      Just u <- liftIO $ V1.nextUUID
      setSession u Nothing 5
      setSession u (Just 4) 5
  ]

-- Small functions to help match the functions from hspec
shouldNotBe :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
shouldNotBe a b =
  when (a == b) $
    assertFailure $ show a <> " should not be equal to " <> show b

shouldThrow :: (HasCallStack, Exception e) => Assertion -> (e -> Bool) -> Assertion
shouldThrow a s = do
  caught <- catch (const False <$> a) (pure . s)
  unless caught $
    assertFailure "Did not throw the correct exception"

binTests :: MapleTestM c m e => (forall a . m a -> IO a) -> TestTree
binTests rS = testGroup "Bins"
  [ testCase "overlap bins" $ rS $ do
      a <- readImgLoot filePath3D "loot/1/A.png"
      bs <- fmap snd <$> view (to (`overlapableBins` a))
      q <- (box2d . expandBy (a^.boundingBox)) <$> view (binMap.igmMaxBB)
      t <- (map snd . filter (overlaps q . fst) . concatMap toList) <$> view (binMap.igmBBs)
      liftIO $ t `shouldNotBe` []
      liftIO $ bs @?= t
  , testCase "insert image" $ rS $ do
      a <- readImgLoot filePath3D "loot/1/A.png"
      now <- liftIO $ getCurrentTime
      ra <- binTX (const (pure True)) (DBMeta 0 now a)
      bs <- view (binMap.allBins)
      t <- mconcat <$> forM bs tip
      liftIO $ ra @?= True
      liftIO $ t @?= [DBMeta 0 now a]
  , testCase "reinsert image" $ rS $ do
      a <- readImgLoot filePath3D "loot/1/A.png"
      now <- liftIO $ getCurrentTime
      bs <- view (binMap.allBins)
      p0 <- doesNotOverlap a
      liftIO $ p0 @?= True
      ra0 <- binTX doesNotOverlap (DBMeta 0 now a)
      t0 <- mconcat <$> forM bs tip
      liftIO $ t0 @?= [DBMeta 0 now a]
      liftIO $ ra0 @?= True
      p1 <- doesNotOverlap a
      liftIO $ p1 @?= False
      ra1 <- binTX doesNotOverlap (DBMeta 0 now a)
      t1 <- mconcat <$> forM bs tip
      liftIO $ t1 @?= [DBMeta 0 now a]
      liftIO $ ra1 @?= False
  , testCase "many images" $ rS $ do
      a <- readImgLoot filePath3D "loot/1/A.png"
      now <- liftIO $ getCurrentTime
      ra <- binTX (const (pure True)) (DBMeta 0 now a)
      b <- readImgLoot filePath3D "loot/1/B.png"
      rb <- binTX (const (pure True)) (DBMeta 0 now  b)
      c <- readImgLoot filePath3D "loot/1/C.png"
      rc <- binTX (const (pure True)) (DBMeta 0 now  c)
      bs <- view (binMap.allBins)
      t <- mconcat <$> forM bs tip
      liftIO $ ra @?= True
      liftIO $ rb @?= True
      liftIO $ rc @?= True
      liftIO $ (sort t) @?= (sort [DBMeta 0 now a, DBMeta 0 now b, DBMeta 0 now c])
  , testCase "remove image" $ rS $ do
      a <- readImgLoot filePath3D "loot/1/A.png"
      now <- liftIO $ getCurrentTime
      ra <- binTX (const (pure True)) (DBMeta 0 now a)
      b <- readImgLoot filePath3D "loot/1/B.png"
      rb <- binTX (const (pure True)) (DBMeta 0 now b)
      c <- readImgLoot filePath3D "loot/1/C.png"
      rc <- binTX (const (pure True)) (DBMeta 0 now c)
      bs <- view (binMap.allBins)
      t <- mconcat <$> forM bs tip
      liftIO $ ra @?= True
      liftIO $ rb @?= True
      liftIO $ rc @?= True
      liftIO $ (sort t) @?= (sort [DBMeta 0 now a, DBMeta 0 now b, DBMeta 0 now c])
      Just bn <- view $ (binMap.to (`homeBin` b))
      binFilter bn (/= (DBMeta 0 now  b))
      td <- mconcat <$> forM bs tip
      liftIO $ (sort td) @?= (sort [DBMeta 0 now a, DBMeta 0 now c])
  , testCase "api insert image" $ rS $ do
      refEnv <- ask
      let app = serveMapleAPI refEnv rS
      (a :: ImgLoot V3 Int) <- readImgLoot filePath3D "loot/1/A.png"
      (s :: SessionData Int (ImgLoot V3 Int)) <- emptySessionData
      sessioned <- makeSessioned (s & sdCurrentLoot .~ [a]) a

      liftIO $ flip runSession app $ do
        let
          insertRequest = defaultRequest {
            requestMethod = methodPost
            , requestHeaders = [(hContentType, "application/json")]
            }
        res <- srequest $ SRequest (setPath insertRequest "place") $ JS.encode sessioned
        assertStatus 200 res
  , testCase "api reinsert image" $ rS $ do
    refEnv <- ask
    let app = serveMapleAPI refEnv rS
    (a :: ImgLoot V3 Int) <- readImgLoot filePath3D "loot/1/A.png"
    (s :: SessionData Int (ImgLoot V3 Int)) <- emptySessionData
    sessioned <- makeSessioned (s & sdCurrentLoot .~ [a]) a

    liftIO $ (`shouldThrow` (const True :: NoSuchLoot -> Bool)) $ flip runSession app $ do
      let
        insertRequest = defaultRequest
                          { requestMethod = methodPost
                          , requestHeaders = [(hContentType, "application/json")]
                          }
      res1 <- srequest $ SRequest (setPath insertRequest "place") $ JS.encode sessioned
      assertStatus 200 res1
      let
        mResponse :: Maybe (Sessioned Int (ImgLoot V3 Int) (UserWithBins Int (ImgLoot V3 Int)))
        mResponse = JS.decode (simpleBody res1)
        sessData = maybe (error "Session data not found") sessionData mResponse
      res2 <- srequest $ SRequest (setPath insertRequest "place") $ JS.encode $ Sessioned sessData a
      assertStatus 403 res2 -- TODO this is bad445
  , testCase "user has info" $ rS $ do
      refEnv <- ask
      let app = serveMapleAPI refEnv rS
      (a :: ImgLoot V3 Int) <- readImgLoot filePath3D "loot/1/A.png"
      (s :: SessionData Int (ImgLoot V3 Int)) <- emptySessionData
      sessioned <- makeSessioned (s & sdCurrentLoot .~ [a]) ()

      liftIO $ flip runSession app $ do
        let
          insertRequest = defaultRequest
                          { requestMethod = methodPost
                          , requestHeaders = [(hContentType, "application/json")]
                          }
        res <- srequest $ SRequest (setPath insertRequest "user") $ JS.encode sessioned
        assertStatus 200 res
        let
          respUserInfo :: Maybe (Sessioned Int (ImgLoot V3 Int) (UserInfo (ImgLoot V3 Int)))
          respUserInfo = JS.decode $ simpleBody res
        liftIO $ (sessionValue <$> respUserInfo) @?= (Just $ UserInfo [a] [])
    ]

mkMap :: IGridMap2 V3 Int Word64
mkMap = fst $
  (`runState` 0) $ buildMap (\b -> state (\c -> ((b, c), c+1)))
                            (AABB (V3 0 0 0) (V3 0 0 0))
                            (P (V2 0 0))
                            (V2 100 100)
                            10 10
