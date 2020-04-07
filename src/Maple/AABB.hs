{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Maple.AABB where

import Control.Lens hiding ((.=))
import Data.Aeson
import Linear
import Linear.Affine

slice2d :: (R2 v) => Lens' (v p) (V2 p) 
slice2d = lens (\vn -> V2 (vn^._x) (vn^._y)) (\s b -> s & _x .~ (b^._x) & _y .~ (b ^. _y))

-- | An Axis Aligned Bounding Box
data BoundingBox v p = AABB { _minCorner, _maxCorner :: v p } deriving (Show, Eq, Ord)

makeClassy ''BoundingBox

box2d :: R2 v => BoundingBox v p -> BoundingBox V2 p
box2d bb = AABB (bb^.minCorner._xy) (bb^.maxCorner._xy)

from2X2D :: (R2 v, HasBoundingBox i v p, Affine v, Integral p) => i -> i
from2X2D bb =
  bb & boundingBox.maxCorner._xy .~ nMax
  where
    minC = bb^.boundingBox.minCorner._xy
    maxC = bb^.boundingBox.maxCorner._xy
    diag = maxC .-. minC
    nMax = minC .+^ (fmap (`div` 2) diag)

instance (Additive v, Ord p) => Semigroup (BoundingBox v p) where
  (AABB aminC amaxC) <> (AABB bminC bmaxC) = AABB (liftU2 min aminC bminC) (liftU2 max amaxC bmaxC)

instance (Applicative v, Additive v, Ord p, Num p) => Monoid (BoundingBox v p) where
  mempty = AABB (pure 0) (pure 0)

instance (Additive v) => Affine (BoundingBox v) where
  type Diff (BoundingBox v) = v

  bba .-. bbb = bbb^.minCorner ^-^ bba^.minCorner

  (AABB minC maxC) .+^ v = AABB (minC ^+^ v) (maxC ^+^ v)

instance ToJSON (v p) => ToJSON (BoundingBox v p) where
  toJSON (AABB minC maxC) =
    object [
        "min" .= minC
      , "max" .= maxC
      ]

instance FromJSON (v p) => FromJSON (BoundingBox v p) where
  parseJSON =
    withObject "BoundingBox" $ \o ->
      AABB <$> o .: "min" <*> o .: "max"
  

corners :: (Additive v, Traversable v) => BoundingBox v p -> [v p]
corners (AABB minC maxC) = sequenceA $ liftI2 (\a b -> [a, b]) minC maxC

-- | Constructs an AABB from any two corners.a
bound :: (Additive v, Ord p) => v p -> v p -> BoundingBox v p
bound c0 c1 = AABB (liftU2 min c0 c1) (liftU2 max c0 c1)

bounding :: (Foldable t, Applicative v, Additive v, Ord p, Num p) => t (v p) -> BoundingBox v p
bounding = foldr (\pnt (AABB minC maxC) -> AABB (liftU2 min minC pnt) (liftU2 max maxC pnt)) mempty

liftI3 :: Additive f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftI3 f a b c = liftI2 (\(a', b') c' -> f a' b' c') (liftI2 (,) a b) c

touches :: (Additive v, Traversable v, Ord p, Eq (v p)) => BoundingBox v p -> v p -> Bool
touches bb@(AABB minC maxC) pnt =
     -- Its between the inclusive min edges and exclusive max edges.
     and (liftI3 (\l h p -> l <= p && p < h) minC maxC pnt)
     -- The min is the inclusive corner. Every corner is some AABB's min.
  && (pnt==minC || not (pnt `elem` (corners bb)))

liftI4 :: Additive f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftI4 f a b c d = liftI2 (\(a', b') (c', d') -> f a' b' c' d') (liftI2 (,) a b) (liftI2 (,) c d)

-- | Check if two bounding boxes overlap in any way
overlaps :: (Additive v, Traversable v, Ord p, Eq (v p)) => BoundingBox v p -> BoundingBox v p -> Bool
overlaps (AABB aminC amaxC) (AABB bminC bmaxC) =
  and $ liftI4 overlap aminC amaxC bminC bmaxC
  where
    overlap aminP amaxP bminP bmaxP =
      containsPoint aminP amaxP bminP
      || containsPoint bminP bmaxP aminP
    containsPoint x1 x2 y =
      ((x1 <= y) && (y < x2)) || (x1 == y)

overlaps' :: (Additive v, Traversable v, Ord p, Eq (v p), HasBoundingBox b v p) => b -> b -> Bool
overlaps' bba bbb = overlaps (bba^.boundingBox) (bbb^.boundingBox)

-- | Takes on BB, the first one, and expands it to be large enough to cover any area that could
--   include the second bounding when said is directly adjacent to the first.
expandBy :: (Additive v, Num p) => BoundingBox v p -> BoundingBox v p -> BoundingBox v p
expandBy (AABB cminC cmaxC) (AABB nminC nmaxC) =
  let d = nmaxC ^-^ nminC
  in AABB (cminC ^-^ d) (cmaxC ^+^ d)
