{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module AABBTest where

import Control.Monad.State
import Data.Foldable
import Linear
import Test.QuickCheck

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Maple.AABB

instance (Foldable v, Applicative v, Additive v, Ord p, Arbitrary (v p), Eq (v p)) => Arbitrary (BoundingBox v p) where
  arbitrary = do
    (p1 :: v p) <- arbitrary
    let
      validP2 :: v p -> Bool
      validP2 = all (not . (`elem` toList p1))
    (p2 :: v p) <- arbitrary `suchThat` validP2
    pure $ bound p1 p2

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

-- Close to the definition in HSpec
infix 1 `shouldSatisfy`
shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
shouldSatisfy x f =
  assertBool ("predicate failed on: " <> show x) (f x)

aabbTests :: TestTree
aabbTests =
  testGroup "AABB" [
      overlapping2dBoxes
    , testCase "Trivial points in a box" $ do
        let
          box :: BoundingBox V2 Int
          box = bound (V2 0 0) (V2 2 2)
          pointsIn = [(V2 0 0), (V2 1 1), (V2 1 0), (V2 0 1)]
        forM_ pointsIn $ \p ->
          (box, p) `shouldSatisfy` (uncurry touches)
    , testCase "Trivial 2d 2x2 box around the origin contains the origin" $ do
        let
          box :: BoundingBox V2 Int
          box = bound (V2 1 1) (V2 (-1) (-1))
        box `shouldSatisfy` (`touches` (V2 0 0))
    , testProperty "Min corner is contained in a box" $ \box ->
        touches (box :: BoundingBox V2 Int) $ _minCorner box
    , testProperty "Max corner is not contained in a box" $ \box ->
        not $ touches (box :: BoundingBox V2 Int) $ _maxCorner box
    , testProperty "Overlaps is symmetric" $ \(b1 :: BoundingBox V3 Int) b2 ->
        overlaps b1 b2 ==> overlaps b2 b1
    , testCase "3D overlapping volumes" $ do
      let
        c1, c2, c3 :: BoundingBox V3 Int
        c1 = bound (V3 0 0 0) (V3 5 5 5)
        c2 = bound (V3 0 0 0) (V3 10 10 10)
        c3 = bound (V3 (-1) (-1) (-1)) (V3 1 1 1)
      (c1,c2) `shouldSatisfy` uncurry overlaps
      (c1,c3) `shouldSatisfy` uncurry overlaps
    , testCase "3D non-overlapping volumes" $ do
      let
        c1, c2, c3 :: BoundingBox V3 Int
        c1 = bound (V3 0 0 0) (V3 5 5 5)
        c2 = bound (V3 5 5 5) (V3 6 6 6)
        c3 = bound (V3 (-1) (-1) (-1)) (V3 0 0 0)
      (c1,c2) `shouldSatisfy` not . uncurry overlaps
      (c1,c3) `shouldSatisfy` not . uncurry overlaps
  ]

-- | Just some simple 2d cases
overlapping2dBoxes :: TestTree
overlapping2dBoxes =
  testGroup "Overlapping 2d boxes" [notTouching2d, noCornersIntersecting, adjacent2d, bottomLeftIn2d
                                   , bottomRightIn2d, topLeftIn2d, topRightIn2d]

-- | 4x4 2D Bounding box centered around the origin
bba :: BoundingBox V2 Int
bba = bound (V2 (-2) (-2)) (V2 2 2)

{-|
   bba
  +---+
+-------+
| |   | | bbb
+-------+
  +---+
-}
noCornersIntersecting :: TestTree
noCornersIntersecting =
  testCase "Two boxes overlap with no corners" $
    assertBool "overlaps bba bbb" $ overlaps bba bbb
  where
    bbb = bound (V2 (-4) (-1)) (V2 4 1)


{-|
 bba    bbb
+---+  +---+
|   |  |   |
|   |  |   |
|   |  |   |
+---+  +---+
-}
notTouching2d :: TestTree
notTouching2d =
    testCase "Two separate boxes don't overlap" $
      assertBool "" $ not $ overlaps bba bbb
  where
    bbb = bound (V2 6 0) (V2 10 4)

{-|
 bba bbb
+---+---+
|   |   |
|   |   |
|   |   |
+---+---+
-}
adjacent2d :: TestTree
adjacent2d = do
    testCase "Adjacent boxes don't overlap" $
      (bba, bbb) `shouldSatisfy` not . (uncurry overlaps)
  where
    bbb = bound (V2 2 (-2)) (V2 6 2)

{-|
   bbb
+-------+
|  bba  |
| +---+ |
| |   | |
| |   | |
| |   | |
| +---+ |
+-------+
-}
fullContained2d :: TestTree
fullContained2d =
    testCase "bba is fully c" $
      assertBool "overlaps bba bbb" $ overlaps bba bbb
  where
    bbb = bound (V2 6 0) (V2 10 4)
{-|
  bbb
+---+
|   |
| +---+
| | | |
+---+ |bba
  |   |
  +---+
-}
bottomRightIn2d :: TestTree
bottomRightIn2d =
  testCase "Bottom right corner of bbb is in bba" $
    assertBool "overlaps bba bbb" $ overlaps bba bbb
  where
    bbb = bound (V2 (-4) (0)) (V2 0 4)

{-|
      +---+
      |   |
    +---+ |bbb
    | | | |
bba | +---+
    |   |
    +---+
-}
bottomLeftIn2d :: TestTree
bottomLeftIn2d =
  testCase "Bottom left corner of bbb is in bba" $
    assertBool "overlaps bba bbb" $ overlaps bba bbb
  where
    bbb = bound (V2 (0) (0)) (V2 4 4)

{-|
    +---+
    |   |
bba | +---+
    | | | |
    +---+ | bbb
      |   |
      +---+
-}
topLeftIn2d :: TestTree
topLeftIn2d =
  testCase "Top left corner of bbb is in bba" $
    assertBool "overlaps bba bbb" $ overlaps bba bbb
  where
    bbb = bound (V2 (0) (-4)) (V2 4 0)

{-|
  +---+
  |   |
+---+ |bba
| | | |
| +---+
|   |
+---+
  bbb
-}
topRightIn2d :: TestTree
topRightIn2d =
  testCase "Top right corner of bbb is in bba" $
    assertBool "overlaps bba bbb" $ overlaps bba bbb
  where
    bbb = bound (V2 (-4) (-4)) (V2 0 0)
