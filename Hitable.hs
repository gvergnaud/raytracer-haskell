module Hitable where

import AABB
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
  boundingBox :: Float -> Float -> a -> Maybe AABB

instance Hitable a => Hitable [a] where
  boundingBox tMin tMax list =
    foldl suroundingBox Nothing . map (boundingBox tMin tMax) $ list

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
