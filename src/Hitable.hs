module Hitable where

import AABB
import Material
import Ray
import Vec3

data HitRecord = HitRecord
  { t :: Float,
    u :: Float,
    v :: Float,
    point :: Vec3,
    normal :: Vec3,
    material :: Material
  }

class Hitable a where
  hit :: Ray -> (Float, Float) -> a -> Maybe HitRecord
  boundingBox :: (Float, Float) -> a -> AABB

data SomeHitable :: * where
  SomeHitable :: Hitable a => a -> SomeHitable

instance Hitable SomeHitable where
  boundingBox range (SomeHitable x) = boundingBox range x
  hit ray range (SomeHitable x) = hit ray range x

instance Hitable a => Hitable [a] where
  boundingBox range (head : tail) =
    foldl suroundingBox (boundingBox range head) . map (boundingBox range) $ tail

  hit ray range@(tMin, tMax) list =
    foldl pickClosestHit Nothing list
    where
      pickClosestHit acc item =
        case acc of
          Nothing -> hit ray range item
          Just hitRecord ->
            case hit ray (tMin, hitRecord.t) item of
              Just closest -> Just closest
              Nothing -> Just hitRecord
