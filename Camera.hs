{-# LANGUAGE NamedFieldPuns #-}

module Camera where

import Ray
import Vec3

data Camera = Camera
  { origin :: Vec3,
    lowerLeftCorner :: Vec3,
    horizontal :: Vec3,
    vertical :: Vec3
  }

newCamera :: Float -> Float -> Camera
newCamera fov aspect =
  let theta = fov * pi / 180
      halfHeight = tan $ theta / 2
      halfWidth = aspect * halfHeight
   in Camera
        { Camera.origin = vec3 0,
          lowerLeftCorner = Vec3 (- halfWidth) (- halfHeight) (-1),
          horizontal = Vec3 (2 * halfWidth) 0 0,
          vertical = Vec3 0 (2 * halfHeight) 0
        }

getRay :: Float -> Float -> Camera -> Ray
getRay u v (Camera {Camera.origin, lowerLeftCorner, horizontal, vertical}) =
  Ray origin direction
  where
    direction = lowerLeftCorner + (vec3 u * horizontal) + (vec3 v * vertical) - origin