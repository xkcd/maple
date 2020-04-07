{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Maple.Loot where

import           Codec.Picture
import           Control.Applicative
import           Control.Exception (PatternMatchFail(..))
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Crypto.Classes
import           Crypto.Skein
import qualified Data.Aeson as JS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import           Data.Foldable
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Serialize as C
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.UUID (UUID)
--import qualified Data.UUID.V5 as V5
import           Data.Word
import           Linear
import           Linear.Affine
import           Maple.AABB
import           Maple.Map
import           Maple.Session
import           System.FilePath.Posix
import           System.Random

class HasUUID h where
  hasUUID :: Getter h UUID

type Hint = Text

type Tumbler s i = forall l . (MonadReader [s] l, Alternative l) => l i

data LootBox s i
 = LootBox
   { _lbHint       :: Hint
   , _lbDifficulty :: Difficulty
   , _ldTumbler    :: ReaderT [s] Maybe i
   }

makeLenses ''LootBox

-- A collection of potental loot boxes
-- Difficulty allows us to give a good sampling, this might be color?
-- Hint is what we show the person, if we choose to name the loot riddle.
-- s -> Maybe i is a function that takes our state and tells us what loot we got or that we haven't gotten it yet.
type LootBoxes s i = Map HintId (LootBox s i)

hintId :: LootBox s i -> HintId
hintId (LootBox {_lbHint=h}) = B64U.encodeBase64 $ C.encode (hash' (TE.encodeUtf8 h)::Skein_256_128)

dropLoot :: forall s i . LootBoxes s i -> SessionData s i -> [(HintId, i)]
dropLoot lm s =
  mapMaybe checkTumbler (s^.sdCurrentHints)
  where
    checkTumbler :: HintId -> Maybe (HintId, i)
    checkTumbler hid = do
      lb <- Map.lookup hid lm
      fmap (hid,) $ (lb^.ldTumbler) `runReaderT` (s^.sdKeyHistory)

newtype LocalPoolSize = LPS Word deriving (Read, Show, Eq, Ord, C.Serialize, Enum, Bounded, Num, Real, Integral) via Word

makeClassy ''LocalPoolSize

localPool :: (BinReader c g l v p b m, MonadReader c m, HasLocalPoolSize c, C.Serialize l) => LootBoxes s i -> l -> m [HintId]
localPool lm lcl = do
  cnt <- view localPoolSize
  let lclStr = C.encode lcl
  ld <- readLocalDifficulty lcl
  pure $ take (fromIntegral cnt) $ map snd $ sort $ map (\k -> (hash (BL.fromChunks [lclStr, TE.encodeUtf8 k])::Skein_256_128, k)) $
    Map.keys $ Map.filter (\h -> (h^.lbDifficulty) >= ld) lm

sessionHints :: (BinReader c g l v p b m, MonadIO m, MonadReader c m, HasLocalPoolSize c, C.Serialize l)
             => LootBoxes s i -> l -> SessionData s i -> m [HintId]
sessionHints lm lcl sess = do
  case length $ sess^.sdCurrentHints of
    l | l >= 5 -> pure []
    l -> do
      let inSess = Set.fromList $ (sess^.sdCurrentHints) ++ (sess^.sdRecentHints)
      lp <- localPool lm lcl
      liftIO $ (take (5-l) . map snd . sort) <$> mapM (\v -> (randomIO::IO Word64) >>= \r -> pure (r, v))
                                                      (filter (not . (`Set.member` inSess)) $ lp) 

currentHintsOnly :: LootBoxes s i -> SessionData s i -> SessionData s i
currentHintsOnly lm sess =
  sess & sdCurrentHints .~ (sess^.sdCurrentHints.to (filter (`Map.member` lm)))

data ImgLoot v p = ImgLoot {_ilAABB :: (BoundingBox v p), _ilImg :: Text} deriving (Eq, Ord, Show)

class HasLootName a where
  lootName :: a -> Text

instance HasLootName (ImgLoot v p) where
  lootName = _ilImg

-- | Checks if two ImgLoots are the same dimensions and have the same text
isTranslatedLoot :: (HasPos i v p, HasLootName i, HasBoundingBox i v p, Eq (v p)) => i -> i -> Bool
isTranslatedLoot i1 i2 =
  (lootName i1) == (lootName i2) &&
    (i1 ^. boundingBox) == ((i2 & pos .~ (i1 ^. pos)) ^. boundingBox)

data NoSuchLoot = NoSuchLoot deriving (Show)

instance Exception NoSuchLoot

extract :: (HasPos i v p, HasLootName i, HasBoundingBox i v p, Eq (v p)) => i -> [i] -> Maybe (i, [i])
extract t l' = go [] l'
  where
    go _ [] = Nothing
    go p (h:r) | isTranslatedLoot h t = Just (h, (reverse p)++r)
    go p (h:r) = go (h:p) r

-- | Removes loot from the session and updates the generation in storage.
removeLoot :: (HasPos i v p, HasLootName i, HasBoundingBox i v p, MonadThrow m, BinStorage c g l v p b m) => SessionData s i -> i -> m (SessionData s i)
removeLoot sd i = do
  case extract i (sd^.sdCurrentLoot) of
    Nothing -> throwM NoSuchLoot
    Just (_, rl) -> pure (sd { _sdCurrentLoot = rl })

{-
instance (JS.ToJSON (v p)) => HasUUID (ImgLoot v p) where
  hasUUID =
    to (\(ImgLoot aabb img) ->
          V5.generateNamed (read "91ff06d6-b0fd-41d7-b343-e8d59641acb4") $
          BL.unpack $ JS.encode (aabb, img))
-}

instance (JS.ToJSON (v p)) => JS.ToJSON (ImgLoot v p) where
  toJSON (ImgLoot b img) =
    JS.object [
      "aabb" JS..= b
    , "img" JS..= img
    ]

instance (JS.FromJSON (v p)) => JS.FromJSON (ImgLoot v p) where
  parseJSON =
    JS.withObject "ImgLoot" $ \o ->
      ImgLoot <$> o JS..: "aabb" <*> o JS..: "img"

makeLenses ''ImgLoot

instance HasBoundingBox (ImgLoot v p) v p where
  boundingBox = ilAABB

instance (Affine v, Num p) => HasPos (ImgLoot v p) v p

-- | Given a function that can create the correct BoundingBox from the file's name and a 2D image size.
--   produce an image loot.
readImgLoot :: (MonadThrow m, MonadIO m) => (FilePath -> V2 Int -> BoundingBox v p) -> FilePath -> m (ImgLoot v p)
readImgLoot dimify fn = do
  ei' <- liftIO $ readImage fn
  case ei' of
    Left e -> throwM $ PatternMatchFail e
    Right i' -> do
      let i = convertRGBA8 i'
      pure $ ImgLoot (dimify fn (V2 (imageWidth i) (imageHeight i))) (T.pack $ fn)

data LootCSV
 = LootCSV { _lcsvDifficulty :: Difficulty, _lcsvHint :: Hint, _lcsvKey :: Text, _lcsvPath :: FilePath }
 deriving (Read, Show, Eq, Ord)

csvDifficulty, csvHint, csvKey, csvImage :: ByteString
csvDifficulty = TE.encodeUtf8 $ T.pack "difficulty"
csvHint = TE.encodeUtf8 $ T.pack "hint"
csvKey = TE.encodeUtf8 $ T.pack "key"
csvImage = TE.encodeUtf8 $ T.pack "image"

instance CSV.FromNamedRecord LootCSV where
  parseNamedRecord m = LootCSV <$> m CSV..: csvDifficulty <*> m CSV..: csvHint <*> m CSV..: csvKey <*> m CSV..: csvImage

instance CSV.ToNamedRecord LootCSV where
  toNamedRecord (LootCSV difficulty hint key img) =
    CSV.namedRecord
    [ csvDifficulty CSV..= difficulty
    , csvHint CSV..= hint
    , csvKey CSV..= key
    , csvImage CSV..= img
    ]

instance CSV.DefaultOrdered LootCSV where
  headerOrder _ = CSV.header [csvDifficulty, csvHint, csvKey, csvImage]

-- | Assume the depth is in the second component and that the target dimensionality is 3D,
--  produce the bounding box.
filePath3D :: FilePath -> V2 Int -> BoundingBox V3 Int
filePath3D fn dm =
  let z = read $ init $ splitPath fn!!1
  in bound (V3 0 0 z) (V3 (dm^._x.to fromIntegral) (dm^._y.to fromIntegral) z)

atExactly :: i -> Text -> Tumbler Text i
atExactly i k = do
  sl <- ask
  case listToMaybe sl of
    Nothing -> empty
    Just s ->
      case T.uncons k of
        Just ('@', knotch) | knotch == s -> pure i
        _ -> empty

-- | Reads a CSV, converting it into loot.
loadLoot :: (Traversable v, Applicative v, Additive v, Ord p, Num p, MonadIO m)
         => (FilePath -> V2 Int -> BoundingBox v p)
         -- ^ The function to convert the filenames and image size into their bounding boxes.
         -> (forall l . (MonadReader [s] l, Alternative l) => [ImgLoot v p -> Text -> l i])
         -- ^ Convert the key field to an actual key.
         -> [FilePath] -- ^ The directories its safe to load images from.
         -> FilePath -- ^ The CSV to load.
         -> m (BoundingBox v p, LootBoxes s i, [ImgLoot v p])
loadLoot dimify keyHoles safeDirs csvFile = liftIO $ do
  csvData <- BL.readFile csvFile
  case CSV.decodeByName csvData of
    Left e -> fail e
    Right (_, v) -> do
      loadedLoot <- forM (toList v) $ \(LootCSV difficulty hint key imgFl) -> do
        let nimgFl = normalise imgFl
        when ((not $ isRelative nimgFl) || (not $ (head $ splitDirectories nimgFl) `elem` safeDirs)) $
          fail $ "bad image file location! "<>nimgFl
        imgLoot <- readImgLoot dimify nimgFl
        pure $ (imgLoot, (imgLoot^.boundingBox.to corners
                ,LootBox hint difficulty $ foldr (<|>) empty $ map (\keyHole -> keyHole imgLoot key) keyHoles))
      let mbl = Map.fromListWith buildTumber . map (\ll->(hintId $ snd ll,ll)) $ snd <$> loadedLoot
      pure $ (bounding $ foldr (\(cs, _) acs -> cs<>acs) [] mbl, fmap snd mbl, fst <$> loadedLoot)
  where
    buildTumber :: ([v p], LootBox s i) -> ([v p], LootBox s i) -> ([v p], LootBox s i)
    buildTumber (bb0, lb0) (bb1, lb1) = (bb0<>bb1 ,lb0 { _ldTumbler = (_ldTumbler lb0) <|> (_ldTumbler lb1) })

data GroupCSV
 = GroupCSV { _gcsvGroup :: Text, _gcsvDepth :: Int, _gcsvImage :: FilePath }
 deriving (Read, Show, Eq, Ord)

csvGroup, csvDepth :: ByteString
csvGroup = TE.encodeUtf8 $ T.pack "group"
csvDepth = TE.encodeUtf8 $ T.pack "depth"

instance CSV.FromNamedRecord GroupCSV where
  parseNamedRecord m = GroupCSV <$> m CSV..: csvGroup <*> m CSV..: csvDepth <*> m CSV..: csvImage

instance CSV.ToNamedRecord GroupCSV where
  toNamedRecord (GroupCSV grp dpth img) =
    CSV.namedRecord
    [ csvGroup CSV..= grp
    , csvDepth CSV..= dpth
    , csvImage CSV..= img
    ]

instance CSV.DefaultOrdered GroupCSV where
  headerOrder _ = CSV.header [csvGroup, csvDepth, csvImage]

maxBB :: (Foldable t, Traversable v, Applicative v, Additive v, Ord p, Num p) => t (ImgLoot v p) -> BoundingBox v p
maxBB = bounding . concatMap (\l -> l^.boundingBox.to corners)

loadGroups :: MonadIO m
           => [FilePath] -- ^ The directories its safe to load images from.
           -> FilePath -- ^ The CSV to load.
           -> m (Map Text [ImgLoot V3 Int])
loadGroups safeDirs csvFile = liftIO $ do
  csvData <- BL.readFile csvFile
  case CSV.decodeByName csvData of
    Left e -> fail e
    Right (_, v) -> do
      fmap (Map.fromListWith (++) . map (fmap pure)) .
        forM (toList v) $ \(GroupCSV grp dpth fp) -> do
          let nimgFl = normalise fp
          when ((not $ isRelative nimgFl) || (not $ (head $ splitDirectories nimgFl) `elem` safeDirs)) $
            fail $ "bad image file location! "<>nimgFl
          (grp,) <$> readImgLoot (\_ (V2 x y) -> AABB (V3 0 0 dpth) (V3 x y dpth)) nimgFl

-- | Reads group encoded CSV, converting it into loot.
loadGrouped :: (MonadFail m, MonadIO m)
            => ([i] -> Text -> Either String ([s] -> Maybe i))
            -- ^ Convert the key field to an actual key.
            -> (Map Text [i]) -- ^ The loot groups to use.
            -> FilePath -- ^ The CSV to load.
            -> m (LootBoxes s i)
loadGrouped keyHoles grpMap csvFile = liftIO $ do
  csvData <- BL.readFile csvFile
  case CSV.decodeByName csvData of
    Left e -> fail e
    Right (_, v) -> do
      loadedLoot <- forM (toList v) $ \(LootCSV difficulty hint key imgGrp) -> do
        case Map.lookup (T.pack imgGrp) grpMap of
          Nothing -> fail $ "Could not find group "<>imgGrp
          Just imgsInGrp ->
            case keyHoles imgsInGrp key of
              Left e -> fail $ mconcat
                        [ "Got ", e, "when trying to parse: ", T.unpack key]
              Right tblr -> pure $ LootBox hint difficulty (lift . tblr =<< ask)
      let mbl = Map.fromListWith (\lb0 lb1 -> lb0 { _ldTumbler = (_ldTumbler lb0) <|> (_ldTumbler lb1) }) .
                map (\ll->(hintId ll,ll)) $ loadedLoot
      pure $ mbl
