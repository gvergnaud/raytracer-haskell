{-# LANGUAGE NamedFieldPuns #-}

module Sphere where

import AABB
import Hitable
import Material
import Ray
import Vec3

data Sphere = Sphere
  { sphereCenter :: Vec3,
    sphereRadius :: Float,
    sphereMaterial :: Material
  }
  deriving (Show)

isBetween :: Float -> Float -> Float -> Bool
isBetween min max value = value > min && value < max

instance Hitable Sphere where
  boundingBox (tMin, tMax) (Sphere center radius material) =
    AABB (center - vec3 radius) (center + vec3 radius)

  hit ray@(Ray origin direction) (tMin, tMax) sphere@(Sphere center radius material) =
    {-
    The equation of a sphere `s` is :
      ((x - s.center) • (x - s.center)) = s.radius ** 2
        where the unknown `x` is a Vec3

    to know if the ray crosses the sphere we can interpolate
    the equation of a a 3D line `l`:
      l.origin + t * l.direction

    which gives us:
      ((l.origin + t * l.direction - s.center) • (l.origin + t * l.direction - s.center)) = s.radius ** 2

    which simplifies to:
      t**2 * (l.direction • l.direction)
        + 2*t*(l.direction • (l.origin - s.center))
        + (l.origin - s.center) • (l.origin - s.center)
        - s.radius ** 2
        = 0

      with
        oc = l.origin - s.center
        a = (l.direction • l.direction)
        b = (oc • l.direction)
        c = (oc • oc) - radius ** 2

      we can simplify this to:
        t**2 * a + 2*t * b + c = 0

      a quadratic function!

      using algebra, we know that the
      curve can cross the line 0, 1 or 2 times. To get this number
      the formula is:
       n = b ** 2 - a * c

      the only unknown is `t` so we ca simplify this to:
        t1 = ((- b + sqrt (b ** 2 - a * c)) / a)
        t2 = ((- b - sqrt (b ** 2 - a * c)) / a)
    -}
    let oc = origin - center
        a = (direction • direction)
        b = (oc • direction)
        c = (oc • oc) - radius ** 2
        discriminant = b ** 2 - a * c
     in if discriminant > 0
          then
            let getRecord t =
                  if isBetween tMin tMax t
                    then
                      let point = pointAtParameter t ray
                          normal = (point - center) / vec3 radius
                       in Just $ HitRecord {t, point, normal, material}
                    else Nothing
                t1 = ((- b - sqrt (b ** 2 - a * c)) / a)
                t2 = ((- b + sqrt (b ** 2 - a * c)) / a)
             in case (getRecord t1, getRecord t2) of
                  (Just record, _) -> Just record
                  (_, Just record) -> Just record
                  _ -> Nothing
          else Nothing