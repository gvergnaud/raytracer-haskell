module Sphere where

import AABB (AABB (AABB))
import Control.Monad (guard)
import Hitable
  ( HitRecord (..),
    Hitable (..),
  )
import Material (Material)
import Math (isBetween)
import Ray (Ray (..))
import Vec3 (Vec3 (..), dot, vec3)

data Sphere = Sphere
  { center :: Vec3,
    radius :: Float,
    material :: Material
  }
  deriving (Show)

getSphereUV :: Vec3 -> (Float, Float)
getSphereUV point@(Vec3 x y z) =
  let phi = atan2 z x
      theta = asin y
      u = 1 - (phi + pi) / (2 * pi)
      v = (theta + pi / 2) / pi
   in (u, v)

instance Hitable Sphere where
  boundingBox (tMin, tMax) (Sphere center radius material) =
    AABB (center - vec3 radius) (center + vec3 radius)

  hit ray (tMin, tMax) sphere = do
    {-
    The equation of a sphere `s` is :
      ((x - s.center) `dot` (x - s.center)) = s.radius ** 2
        where the unknown `x` is a Vec3

    to know if the ray crosses the sphere we can interpolate
    the equation of a a 3D line `l`:
      l.origin + t * l.direction

    which gives us:
      ((l.origin + t * l.direction - s.center) `dot` (l.origin + t * l.direction - s.center)) = s.radius ** 2

    which simplifies to:
      t**2 * (l.direction `dot` l.direction)
        + 2*t*(l.direction `dot` (l.origin - s.center))
        + (l.origin - s.center) `dot` (l.origin - s.center)
        - s.radius ** 2
        = 0

      with
        oc = l.origin - s.center
        a = (l.direction `dot` l.direction)
        b = (oc `dot` l.direction)
        c = (oc `dot` oc) - radius ** 2

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
    let oc = ray.origin - sphere.center
        a = (ray.direction `dot` ray.direction)
        b = (oc `dot` ray.direction)
        c = (oc `dot` oc) - sphere.radius ** 2
        discriminant = b ** 2 - a * c

    guard (discriminant > 0)

    let getRecord t = do
          guard (isBetween tMin tMax t)
          let point = ray.pointAtParameter t
              normal = (point - sphere.center) / vec3 sphere.radius
              (u, v) = getSphereUV point
           in Just HitRecord {t, u, v, point, normal, material = sphere.material}
        t1 = ((-b - sqrt (b ** 2 - a * c)) / a)
        t2 = ((-b + sqrt (b ** 2 - a * c)) / a)

    case (getRecord t1, getRecord t2) of
      (Just record1, Just record2) -> if (t1 < t2) then Just record1 else Just record2
      (Just record, _) -> Just record
      (_, Just record) -> Just record
      _ -> Nothing