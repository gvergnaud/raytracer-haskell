module Ray where

import GHC.Records (HasField (..))
import Vec3 (Vec3, vec3)

data Ray = Ray
  { origin :: Vec3,
    direction :: Vec3
  }

instance HasField "pointAtParameter" Ray (Float -> Vec3) where
  getField = flip pointAtParameter

pointAtParameter :: Float -> Ray -> Vec3
pointAtParameter t ray =
  ray.origin + vec3 t * ray.direction
