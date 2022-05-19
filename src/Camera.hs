module Camera where

import GHC.Records (HasField (getField))
import Random (getRandomInUnitDisk)
import Ray
import Vec3

data Camera = Camera
  { origin :: Vec3,
    lowerLeftCorner :: Vec3,
    horizontal :: Vec3,
    vertical :: Vec3,
    lensRadius :: Float,
    u :: Vec3,
    v :: Vec3,
    w :: Vec3
  }

newCamera :: Vec3 -> Vec3 -> Vec3 -> Float -> Float -> Float -> Float -> Camera
newCamera lookFrom lookAt vup fov aspect aperture focusDistance =
  let lensRadius = aperture / 2
      theta = fov * pi / 180
      halfHeight = tan $ theta / 2
      halfWidth = aspect * halfHeight
      w = (lookFrom - lookAt).normalize
      u = (vup `cross` w).normalize
      v = w `cross` u
   in Camera
        { Camera.origin = lookFrom,
          lowerLeftCorner = lookFrom - vec3 (halfWidth * focusDistance) * u - vec3 (halfHeight * focusDistance) * v - vec3 focusDistance * w,
          horizontal = vec3 (2 * (halfWidth * focusDistance)) * u,
          vertical = vec3 (2 * (halfHeight * focusDistance)) * v,
          lensRadius,
          u,
          v,
          w
        }

getRay :: Float -> Float -> Camera -> IO Ray
getRay s t (Camera {origin, lowerLeftCorner, horizontal, vertical, lensRadius, u, v}) = do
  rand <- getRandomInUnitDisk
  let randDisc = vec3 lensRadius * rand
      offset = u * vec3 randDisc.x + v * vec3 randDisc.y
      rayOrigin = origin + offset
      direction = lowerLeftCorner + (vec3 s * horizontal) + (vec3 t * vertical) - origin - offset
  return $ Ray rayOrigin direction

instance HasField "getRay" Camera (Float -> Float -> IO Ray) where
  getField cam s t = getRay s t cam