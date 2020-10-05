{-# LANGUAGE NamedFieldPuns #-}

module Camera where

import Random (getRandomInUnitDisk)
import Ray
import Vec3

data Camera = Camera
  { origin :: Vec3,
    lowerLeftCorner :: Vec3,
    horizontal :: Vec3,
    vertical :: Vec3,
    lensRadius :: Float,
    cameraU :: Vec3,
    cameraV :: Vec3,
    cameraW :: Vec3
  }

newCamera :: Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Float -> Float -> Camera
newCamera lookFrom lookAt vup fov aspect aperture focusDistance =
  let lensRadius = aperture / 2
      theta = fov * pi / 180
      halfHeight = tan $ theta / 2
      halfWidth = aspect * halfHeight
      w = unitVector $ lookFrom - lookAt
      u = unitVector $ vup `cross` w
      v = w `cross` u
   in Camera
        { Camera.origin = lookFrom,
          lowerLeftCorner = lookFrom - vec3 (halfWidth * focusDistance) * u - vec3 (halfHeight * focusDistance) * v - vec3 focusDistance * w,
          horizontal = vec3 (2 * (halfWidth * focusDistance)) * u,
          vertical = vec3 (2 * (halfHeight * focusDistance)) * v,
          lensRadius,
          cameraU = u,
          cameraV = v,
          cameraW = w
        }

getRay :: Float -> Float -> Camera -> IO Ray
getRay s t (Camera {Camera.origin, lowerLeftCorner, horizontal, vertical, lensRadius, cameraU = u, cameraV = v}) = do
  rand <- getRandomInUnitDisk
  let randDisc = vec3 lensRadius * rand
      offset = u * vec3 (getX randDisc) + v * vec3 (getY randDisc)
      rayOrigin = origin + offset
      direction = lowerLeftCorner + (vec3 s * horizontal) + (vec3 t * vertical) - origin - offset
  return $ Ray rayOrigin direction
