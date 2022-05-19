module AABB where

import Ray (Ray (..))
import Vec3 (Vec3 (..), vmax, vmin)

data AABB = AABB
  { minVec :: Vec3,
    maxVec :: Vec3
  }
  deriving (Show)

order :: Ord b => (b, b) -> (b, b)
order (x, y) = if x < y then (x, y) else (y, x)

interval :: Float -> Float -> Float -> Float -> (Float, Float)
interval origin direction x1 x2 =
  let t1 = (origin - x1) / direction
      t2 = (origin - x2) / direction
   in order (t1, t2)

overlap :: (Float, Float) -> (Float, Float) -> Maybe (Float, Float)
overlap (min1, max1) (min2, max2) =
  let oMin = max min1 min2
      oMax = min max1 max2
   in if oMin < oMax
        then Just $ (oMin, oMax)
        else Nothing

hitAABB :: Ray -> AABB -> Bool
hitAABB ray (AABB minVec maxVec) =
  let xsInterval = interval ray.origin.x ray.direction.x minVec.x maxVec.x
      ysInterval = interval ray.origin.y ray.direction.y minVec.y maxVec.y
      zsInterval = interval ray.origin.z ray.direction.z minVec.z maxVec.z
      hitInterval = overlap xsInterval =<< overlap ysInterval zsInterval
   in case hitInterval of
        Just interval -> True
        Nothing -> False

suroundingBox :: AABB -> AABB -> AABB
suroundingBox (AABB minVec maxVec) (AABB minVec' maxVec') =
  AABB
    { minVec = (minVec `vmin` minVec'),
      maxVec = (maxVec `vmax` maxVec')
    }