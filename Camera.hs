module Camera where

import Ray
import Vec3

data Camera = Camera
  { origin :: Vec3,
    lowerLeftCorner :: Vec3,
    horizontal :: Vec3,
    vertical :: Vec3
  }

getRay :: Float -> Float -> Camera -> Ray
getRay u v (Camera origin lowerLeftCorner horizontal vertical) =
  Ray origin direction
  where
    direction = lowerLeftCorner + (vec3 u * horizontal) + (vec3 v * vertical)