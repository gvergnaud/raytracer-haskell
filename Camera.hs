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

newCamera :: Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Camera
newCamera lookFrom lookAt vup fov aspect =
  let theta = fov * pi / 180
      halfHeight = tan $ theta / 2
      halfWidth = aspect * halfHeight
      w = unitVector $ lookFrom - lookAt
      u = unitVector $ vup `cross` w
      v = w `cross` u
   in Camera
        { Camera.origin = lookFrom,
          lowerLeftCorner = lookFrom - vec3 halfWidth * u - vec3 halfHeight * v - w,
          horizontal = vec3 (2 * halfWidth) * u,
          vertical = vec3 (2 * halfHeight) * v
        }

getRay :: Float -> Float -> Camera -> Ray
getRay u v (Camera {Camera.origin, lowerLeftCorner, horizontal, vertical}) =
  Ray origin direction
  where
    direction = lowerLeftCorner + (vec3 u * horizontal) + (vec3 v * vertical) - origin