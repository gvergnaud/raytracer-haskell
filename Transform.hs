{-# LANGUAGE NamedFieldPuns #-}

module Transform (flipNormal, translate, rotateY) where

import AABB
import GHC.Float
import Hitable
import Ray
import Vec3

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
        let x = i * (getX . maxVec) bbox + (1 - i) * (getX . minVec) bbox
            y = j * (getY . maxVec) bbox + (1 - j) * (getY . minVec) bbox
            z = k * (getZ . maxVec) bbox + (1 - k) * (getZ . minVec) bbox
            newX = cosTheta * x + sinTheta * z
            newZ = (- sinTheta) * x + cosTheta * z
            testerVec = Vec3 newX y newZ
         in (vmin accMin testerVec, vmax accMax testerVec)
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
    (HitRecord {t, u, v, point, normal, material}) <- hit ray range hitable
    return $ HitRecord {normal = - normal, t, u, v, point, material}

data Translate a = Translate Vec3 a

instance Hitable a => Hitable (Translate a) where
  boundingBox range (Translate offset hitable) =
    let (AABB minVec maxVec) = boundingBox range hitable
     in AABB (minVec + offset) (maxVec + offset)

  hit ray@(Ray {origin, direction}) range (Translate offset hitable) = do
    let translatedRay = Ray (origin - offset) direction
    record@(HitRecord {point}) <- hit translatedRay range hitable
    Just $
      HitRecord
        { point = point + offset,
          u = u record,
          v = v record,
          t = t record,
          material = material record,
          normal = normal record
        }

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
            (cosTheta * (getX vec) - sinTheta * (getZ vec))
            (getY vec)
            (sinTheta * (getX vec) + cosTheta * (getZ vec))
        invRotateY' vec =
          Vec3
            (cosTheta * (getX vec) + sinTheta * (getZ vec))
            (getY vec)
            (- sinTheta * (getX vec) + cosTheta * (getZ vec))

        rotatedRay = Ray (rotateY' origin) (rotateY' direction)

    (HitRecord {point, normal, t, u, v, material}) <- hit rotatedRay range hitable

    return $
      HitRecord
        { point = invRotateY' point,
          normal = invRotateY' normal,
          t,
          u,
          v,
          material
        }
