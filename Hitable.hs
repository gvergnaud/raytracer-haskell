module Hitable where

import Material
import Ray
import Vec3

data HitRecord = HitRecord
  { t :: Float,
    point :: Vec3,
    normal :: Vec3,
    material :: Material
  }

class Hitable a where
  hit :: Ray -> Float -> Float -> a -> Maybe HitRecord

instance Hitable a => Hitable [a] where
  hit ray tMin tMax list =
    foldl pickClosestHit Nothing list
    where
      pickClosestHit acc item =
        case acc of
          Nothing -> hit ray tMin tMax item
          Just hitRecord ->
            case hit ray tMin (t hitRecord) item of
              Just closest -> Just closest
              Nothing -> Just hitRecord
