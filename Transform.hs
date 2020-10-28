{-# LANGUAGE NamedFieldPuns #-}

module Transform where

import AABB
import Hitable
import Ray
import Vec3

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