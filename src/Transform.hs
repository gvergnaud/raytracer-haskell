module Transform (flipNormal, translate, rotateY) where

import AABB (AABB (..))
import GHC.Float ()
import Hitable (HitRecord (normal, point), Hitable (..))
import Ray (Ray (Ray, direction, origin))
import Vec3 (Vec3 (..), vec3, vmax, vmin)

flipNormal :: Hitable a => a -> FlipNormal a
flipNormal = FlipNormal

translate :: Hitable a => Vec3 -> a -> Translate a
translate vec hitable = Translate vec hitable

rotateY :: Hitable a => Float -> a -> Rotate a
rotateY angle hitable =
  let radians = (pi / 180) * angle
      sinTheta = sin radians
      cosTheta = cos radians
      bbox = boundingBox (0, 1) hitable
      ijk = [(i, j, k) | i <- [0, 1], j <- [0, 1], k <- [0, 1]]
      folder :: (Vec3, Vec3) -> (Float, Float, Float) -> (Vec3, Vec3)
      folder (accMin, accMax) (i, j, k) =
        let x = i * bbox.maxVec.x + (1 - i) * bbox.minVec.x
            y = j * bbox.maxVec.y + (1 - j) * bbox.minVec.y
            z = k * bbox.maxVec.z + (1 - k) * bbox.minVec.z
            newX = cosTheta * x + sinTheta * z
            newZ = (-sinTheta) * x + cosTheta * z
            testerVec = Vec3 newX y newZ
         in (accMin `vmin` testerVec, accMax `vmax` testerVec)
      (aabbMin, aabbMax) = foldl folder (vec3 1000000000000, vec3 (-1000000000000)) ijk
   in RotateY
        { aabb = AABB aabbMin aabbMax,
          sinTheta,
          cosTheta,
          hitable
        }

newtype FlipNormal a = FlipNormal a

instance Hitable a => Hitable (FlipNormal a) where
  boundingBox range (FlipNormal hitable) =
    boundingBox range hitable

  hit ray range (FlipNormal hitable) = do
    hitRecord <- hit ray range hitable
    pure (hitRecord {normal = -hitRecord.normal})

data Translate a = Translate Vec3 a

instance Hitable a => Hitable (Translate a) where
  boundingBox range (Translate offset hitable) =
    let (AABB minVec maxVec) = boundingBox range hitable
     in AABB (minVec + offset) (maxVec + offset)

  hit ray@(Ray {origin, direction}) range (Translate offset hitable) = do
    let translatedRay = Ray (origin - offset) direction
    hitRecord <- hit translatedRay range hitable
    pure (hitRecord {point = hitRecord.point + offset})

data Rotate a = RotateY
  { hitable :: a,
    aabb :: AABB,
    sinTheta :: Float,
    cosTheta :: Float
  }

instance Hitable a => Hitable (Rotate a) where
  boundingBox range (RotateY {aabb}) =
    aabb
  hit ray@(Ray {origin, direction}) range (RotateY {hitable, cosTheta, sinTheta, aabb}) = do
    -- The trick is to first rotate the ray around the Y axis
    -- And then rotate the hit point and the normal in the opposite direction
    -- In 2d, a rotation matrix is:
    --    ((cosTheta, -sinTheta),
    --     (sinTheta, cosTheta))
    let rotateY' vec =
          Vec3
            { x = (cosTheta * vec.x - sinTheta * vec.z),
              y = vec.y,
              z = (sinTheta * vec.x + cosTheta * vec.z)
            }
        invRotateY' vec =
          Vec3
            { x = (cosTheta * vec.x + sinTheta * vec.z),
              y = vec.y,
              z = (-sinTheta * vec.x + cosTheta * vec.z)
            }
        rotatedRay =
          Ray
            { origin = (rotateY' origin),
              direction = (rotateY' direction)
            }

    hitRecord <- hit rotatedRay range hitable

    pure
      hitRecord
        { point = invRotateY' hitRecord.point,
          normal = invRotateY' hitRecord.normal
        }
