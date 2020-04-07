{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Maple.Map where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Aeson as JS
import           Data.Foldable
import           Data.Kind
import           Data.List
import           Data.Proxy
import           Data.Time
import           Data.UUID (UUID)
import qualified Data.Vector as V
import           Linear
import           Linear.Affine
import           Maple.AABB
import           Maple.Session

class HasPos i v p | i -> v, i -> p where
  pos :: Lens' i (Point v p)
  default pos :: (Num p, Affine v, HasBoundingBox i v p) => Lens' i (Point v p)
  pos = lens (\s -> s^.boundingBox.minCorner.from _Point)
             (\s b ->
                let off = s^.boundingBox.maxCorner .-. s^.boundingBox.minCorner
                in (s & boundingBox.minCorner .~ (b^.lensP)) & boundingBox.maxCorner .~ ((b^.lensP) .+^ off))

instance (Affine v, Num p) => HasPos (BoundingBox v p) v p

pos2d :: forall i v p . (R2 v, HasPos i v p) => Proxy v -> Lens' i (Point V2 p)
pos2d _ =
  lens (\vn -> P (V2 (vn^.(pos::Lens' i (Point v p))._x) (vn^.(pos::Lens' i (Point v p))._y)))
       (\s b -> s & (pos::Lens' i (Point v p))._x .~ (b^._x) & (pos::Lens' i (Point v p))._y .~ (b ^. _y))

class (R2 v, Ord b) => HasBinMap c g (v::Type->Type) p b | c -> g, c -> v, c -> p, c -> b where

  binMap :: Getter c g

  -- | Useful if we need to reproject everything into a new universe.
  allBins :: Getter c [b]
  default allBins :: HasBinMap g g v p b => Getter c [b]
  allBins = binMap.allBins

  -- | What is the largest BB allowed in the Grid that we have to check we could overlap.
  largestBB :: Getter c (BoundingBox v p)
  default largestBB :: HasBinMap g g v p b => Getter c (BoundingBox v p)
  largestBB = binMap.largestBB

  -- | The bin this point belongs in.
  homeBin :: HasPos i v p => c -> i -> Maybe b
  default homeBin :: (HasBinMap g g v p b, HasPos i v p) => c -> i -> Maybe b
  homeBin g i = g^.binMap.to (`homeBin` i)

  -- | The list of bins that could possibly have something that overlaps this.
  overlapableBins :: (HasBoundingBox i v p) => c -> i -> [(BoundingBox V2 p, b)]
  default overlapableBins :: (R2 v, HasBoundingBox i v p, HasBinMap g g v p b)
                          => c -> i -> [(BoundingBox V2 p, b)]
  overlapableBins g b = g^.binMap.to (`overlapableBins` b)

data IGridMap2 v p b
 = IGMap
   { _igmMaxBB :: BoundingBox v p
   -- | Sorted by the min corner in X and then Y.
   , _igmBBs   :: V.Vector (V.Vector (BoundingBox V2 p, b))
   }
   deriving (Eq, Ord, Show)

makeLenses ''IGridMap2

instance (JS.ToJSON p, JS.ToJSON (V2 p), JS.ToJSON (v p), JS.ToJSON b) => JS.ToJSON (IGridMap2 v p b) where
  toJSON (IGMap maxBB vBBs) = JS.object [ "maxBB" JS..= maxBB, "tiles" JS..= (map V.toList $ V.toList vBBs)]

instance (JS.ToJSON p, JS.FromJSON (V2 p), JS.FromJSON (v p), JS.FromJSON b) => JS.FromJSON (IGridMap2 v p b) where
  parseJSON = JS.withObject "IGridMap2" $ \v -> IGMap
    <$> v JS..: "maxBB"
    <*> (V.fromList <$> (v JS..: "tiles" >>= mapM (fmap V.fromList . JS.parseJSON)))

-- | Creates a  IGridMap with a single tile in it.
singletonIGridMap2 :: Monad m => m b -> BoundingBox v p -> BoundingBox V2 p -> m (IGridMap2 v p b)
singletonIGridMap2 gID maxBB baseBB = (IGMap maxBB) <$> ((V.singleton . V.singleton . (baseBB,)) <$> gID)

-- | Adds another row of the tile in the x direction
igridmap2ExtendX :: (Monad m, HasPos (BoundingBox V2 p) V2 p, Num p) => m b -> IGridMap2 v p b -> m (IGridMap2 v p b)
igridmap2ExtendX gID (IGMap maxBB v) = do
  fy <- V.lastM v
  b  <- V.headM fy
  let xstride = b^._1.maxCorner._x - b^._1.minCorner._x
  ny <- V.forM fy $ \(lbb, _) -> (lbb & pos._x +~ xstride,) <$> gID
  pure $ IGMap maxBB (v `V.snoc` ny)

-- | Adds another row of the tile in the negative x direction
igridmap2ExtendXNeg :: (Monad m, HasPos (BoundingBox V2 p) V2 p, Num p) => m b -> IGridMap2 v p b -> m (IGridMap2 v p b)
igridmap2ExtendXNeg gID (IGMap maxBB v) = do
  hy <- V.headM v
  b  <- V.headM hy
  let xstride = b^._1.maxCorner._x - b^._1.minCorner._x
  ny <- V.forM hy $ \(lbb, _) -> (lbb & pos._x -~ xstride,) <$> gID
  pure $ IGMap maxBB (ny `V.cons` v)

-- | Adds another row of the tile in the y direction
igridmap2ExtendY :: (Monad m, HasPos (BoundingBox V2 p) V2 p, Num p) => m b -> IGridMap2 v p b -> m (IGridMap2 v p b)
igridmap2ExtendY gID (IGMap maxBB v) = do
  b <- V.headM v >>= V.headM
  let ystride = b^._1.maxCorner._y - b^._1.minCorner._y
  nv <- V.forM v $ \yv -> do
    (ly, _) <- V.lastM yv
    ny <- (ly & pos._y +~ ystride,) <$> gID
    pure $ yv `V.snoc` ny
  pure $ IGMap maxBB nv

-- | Adds another row of the tile in the y direction
igridmap2ExtendYNeg :: (Monad m, HasPos (BoundingBox V2 p) V2 p, Num p) => m b -> IGridMap2 v p b -> m (IGridMap2 v p b)
igridmap2ExtendYNeg gID (IGMap maxBB v) = do
  b <- V.headM v >>= V.headM
  let ystride = b^._1.maxCorner._y - b^._1.minCorner._y
  nv <- V.forM v $ \yv -> do
    (ly, _) <- V.headM yv
    ny <- (ly & pos._y -~ ystride,) <$> gID
    pure $ ny `V.cons` yv
  pure $ IGMap maxBB nv

-- | Given the largest BoundingBox that can be placed in the map,
--   a function to define bins for generated bounding boxes,
--   an origin point, a stride vector, and how many bins in both
--   x and y to generate to build a IGridMap2.
buildMap :: (Monad m, R2 v, Integral p) => (BoundingBox V2 p -> m (BoundingBox V2 p, b)) -> BoundingBox v p -> Point V2 p -> V2 p -> p -> p -> m (IGridMap2 v p b)
buildMap g l orig step xc yc = do
  let b = AABB (orig^.lensP) ((orig^.lensP) .+^ step)
  (fmap $ IGMap l) $ do
    V.generateM (fromIntegral xc) $ \xm' -> do
      V.generateM (fromIntegral yc) $ \ym' -> do
        let j = V2 ((step^._x) * fromIntegral xm') ((step^._y) * fromIntegral ym')
        g $ b & minCorner._xy .~ ((b ^.minCorner._xy) .+^ j)
              & maxCorner._xy .~ ((b ^.maxCorner._xy) .+^ j)

binsUnique :: (HasBinMap c (IGridMap2 v p b) v p b, Additive v, Ord p, Eq p) => c -> Bool
binsUnique c =
  let binSet = c^.binMap.igmBBs.to (concatMap toList . toList)
      bids   = map snd binSet
  in (nub bids == bids) && and [bid0 == bid1 || not (b0 `overlaps'` b1) | (b0, bid0) <- binSet, (b1, bid1) <- binSet]

updateMap :: (Num b, Ord b, Ord p, Integral p, Eq (v p), Traversable v, Additive v, R2 v) => IGridMap2 v p b -> (forall m . Monad m => m b -> IGridMap2 v p b -> m (IGridMap2 v p b)) -> (IGridMap2 v p b)
updateMap ogm act =
  if binsUnique ngm
  then ngm
  else error "BAD UPDATE OF MAP"
  where
    high = ogm^.binMap.igmBBs.to (maximum . map snd . concatMap toList . toList)
    ngm = fst $ (`runState` (high+1)) $ act (state (\c -> (c, c+1))) ogm

instance forall v p b. (R2 v, Additive v, Traversable v, Ord b, Ord p, Integral p, Eq (v p)) => HasBinMap (IGridMap2 v p b) (IGridMap2 v p b) v p b where

  binMap = id

  largestBB = igmMaxBB

  allBins = binMap.igmBBs. to (map snd . mconcat . fmap toList . toList)

  homeBin (IGMap {_igmBBs=ab}) i = do
    xv <- findX2 (i^.pos2d (Proxy::Proxy v)) ab
    b <- V.headM xv
    let stride = b^._1.maxCorner._y - b^._1.minCorner._y
    fmap snd $ xv V.!? (fromIntegral $ (i^.(pos::HasPos i v p => Lens' i (Point v p))._Point._y - b^._1.minCorner._y) `div` stride)

  overlapableBins (IGMap {_igmMaxBB=lbb, _igmBBs=ab}) bb =
    let q = (bb^.boundingBox.to box2d) `expandBy` (lbb^.to box2d)
    in filter (overlaps q . fst) $ mconcat $ fmap toList $ toList $ ab

-- | Finds the vector the BoundingBox for that X will be found in for 2 dimensions of bounding boxes.
findX2 :: (Integral p) => Point V2 p -> V.Vector (V.Vector (BoundingBox V2 p, b)) -> Maybe (V.Vector (BoundingBox V2 p, b))
findX2 pnt v = do
  i <- V.headM v >>= V.headM
  let stride = i^._1.maxCorner._x - i^._1.minCorner._x
  v V.!? (fromIntegral $ (pnt^._x - i^._1.minCorner._x) `div` stride)

data DBMeta l i
 = DBMeta
 { _dbmLocal :: l
 , _dbmTime :: UTCTime
 , _dbmItem :: i
 }
 deriving (Read, Show, Eq, Ord, Functor)

makeLenses ''DBMeta

instance HasPos i v p => HasPos (DBMeta l i) v p where
  pos = dbmItem.pos

instance HasBoundingBox i v p => HasBoundingBox (DBMeta l i) v p where
  boundingBox = dbmItem.boundingBox

instance (JS.ToJSON l, JS.ToJSON i) => JS.ToJSON (DBMeta l i) where
  toJSON (DBMeta l t i) = JS.object ["l" JS..= l, "t" JS..= t, "i" JS..= i]

instance (JS.FromJSON l, JS.FromJSON i) => JS.FromJSON (DBMeta l i) where
  parseJSON = JS.withObject "dbmeta" $ \v -> DBMeta
    <$> v JS..: "l"
    <*> v JS..: "t"
    <*> v JS..: "i"

class (MonadReader c m, HasBinMap c g v p b, HasBinMap g g v p b, HasPos (I m) v p, Ord p, Eq (v p), Traversable v, Additive v) => BinReader c g l v p b m | m -> c, m -> l where
  -- | The type of the items stored.
  type I m :: Type

  -- | Gets the bin contense
  tip :: b -> m [DBMeta l (I m)]

  -- | Reads the state of the session, starting at generation 0 if not yet set.
  readSession :: UUID -> m SessionGeneration

  -- | Gets the current difficulty for a locality, or 0 if not set.
  readLocalDifficulty :: l -> m Difficulty

instance (BinReader c g l v p b m) => BinReader c g l v p b (ExceptT e m) where
  type I (ExceptT e m) = I m
  tip = lift . tip
  readSession = lift . readSession
  readLocalDifficulty = lift . readLocalDifficulty

doesNotOverlap :: (BinReader c g l v p b tx, R2 v, HasBoundingBox (I tx) v p
                  ,HasBoundingBox i v p, Additive v, Traversable v, Ord p, Eq (v p))
               => i -> tx Bool
doesNotOverlap i = do
  bs <- view (to (`overlapableBins` i))
  fmap (not . or) . forM (snd <$> bs) $ \b -> (any (\c -> overlaps' (i^.boundingBox) (c^.boundingBox))) <$> tip b

class (BinReader c g l v p b m) => BinStorage c g l v p b m where
  -- | takes a function that tells us if we should commit
  binTX :: (forall tx . (BinReader c g l v p b tx, I m ~ I tx) => DBMeta l (I tx) -> tx Bool)
        -> DBMeta l (I m) -> m Bool
  -- | Given a function that says if it should be in the set, makes the bin contain only the ones that should.
  binFilter :: b -> (DBMeta l (I m) -> Bool) -> m ()
  -- | Stores session state, throws is session generation provided does not match.
  setSession :: UUID -> Maybe SessionGeneration -> SessionGeneration -> m ()
  -- | Verifies DB connetion. Throws an exception if it fails
  checkDB :: m ()
  -- | Increase locality difficulty by value.
  incLocalDifficulty :: l -> Difficulty -> m ()
  -- | Returns Just the time they can next place and updates the last palce time,
  --   or Nothing if they can't play yet.
  tryPlace :: l -> NominalDiffTime -> m (Maybe UTCTime)
 
instance (BinStorage c g l v p b m) => BinStorage c g l v p b (ExceptT e m) where
  binTX f i = lift $ binTX f i
  binFilter b f = lift $ binFilter b f
  setSession u mo n = lift $ setSession u mo n
  checkDB = lift checkDB
  incLocalDifficulty l d = lift $ incLocalDifficulty l d
  tryPlace l nd = lift $ tryPlace l nd

incSessionGen :: BinStorage c g l v p b m => SessionData s i -> m (SessionData s i)
incSessionGen sd = do
  let ng = 1 + sd^.sdGeneration
  setSession (sd^.sdUUID) (Just $ sd^.sdGeneration) ng
  pure (sd { _sdGeneration = ng })
