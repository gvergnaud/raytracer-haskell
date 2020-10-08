{-# LANGUAGE NamedFieldPuns #-}

module AABB where

import Ray
import Vec3

data AABB = AABB
  { minVec :: Vec3,
    maxVec :: Vec3
  }

order :: Ord b => (b, b) -> (b, b)
order (x, y) = if x > y then (y, x) else (x, y)

interval :: Float -> Float -> Float -> Float -> (Float, Float)
interval origin direction x1 x2 =
  let t1 = (origin - x1) / direction
      t2 = (origin - x2) / direction
   in order (t1, t2)

overlap :: (Float, Float) -> (Float, Float) -> Maybe (Float, Float)
overlap (min1, max1) (min2, max2) =
  if max1 > min2 && min1 < max2
    then Just $ (max min1 min2, min max1 max2)
    else Nothing

hitAABB :: Ray -> Float -> Float -> AABB -> Bool
hitAABB ray@(Ray {origin, direction}) tMin tMax (AABB (Vec3 x y z) (Vec3 x' y' z')) =
  let xsInterval = interval (getX origin) (getX direction) x x'
      ysInterval = interval (getY origin) (getY direction) y y'
      zsInterval = interval (getZ origin) (getZ direction) z z'
      hitInterval = overlap (tMin, tMax) =<< overlap xsInterval =<< overlap ysInterval zsInterval
   in case hitInterval of
        Just interval -> True
        Nothing -> False

suroundingBox :: Maybe AABB -> Maybe AABB -> Maybe AABB
suroundingBox aabb1 aabb2 =
  case (aabb1, aabb2) of
    (Just (AABB min max), Just (AABB min' max')) ->
      Just $ AABB (vmin min min') (vmax max max')
    _ -> Nothing