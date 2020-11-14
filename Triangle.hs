{-# LANGUAGE NamedFieldPuns #-}

module Triangle where

import AABB
import Control.Monad (guard)
import Hitable
import Material
import Ray
import Vec3

data Triangle
  = XYTriangle (Float, Float) (Float, Float) (Float, Float) Float Material

instance Hitable Triangle where
  boundingBox _ (XYTriangle (x0, y0) (x1, y1) (x2, y2) k material) =
    AABB
      (Vec3 (x0 `min` x1 `min` x2) (y0 `min` y1 `min` y2) (k - 0.001))
      (Vec3 (x0 `max` x1 `max` x2) (y0 `max` y1 `max` y2) (k + 0.001))

  hit ray range (XYTriangle p1 p2 p3 k material) = do
    {-
     - trouver le point P d'intersection avec le plan:
        origin.z + t * direction.z = k
         <=> t = (k - origin.z) / direction.z
    -}
    let t = (k - (getZ . origin) ray) / (getZ . direction) ray
        x = (getX . origin) ray + t * (getX . direction) ray
        y = (getX . origin) ray + t * (getX . direction) ray
        z = k
        intersectionPoint = Vec3 x y z
    -------------
    --- Solution 1
    -------------
    {-
     - déterminer si le point P est dans le triange:
        - si l'angle entre un côté du triange et
          la droite qui passe par P et inférieur à l'angle theta du triangle
        - et si la même operation est vrai pour un autre côté du triangle, alors le point est dans le triangle
    -}
    -- theta = Nothing -- TODO
    -----------------
    -- Solution 2
    -----------------
    -- from https://mathworld.wolfram.com/TriangleInterior.html
    let vecFromTuple :: (Float, Float) -> Vec3
        vecFromTuple (x, y) = Vec3 x y k
        det :: Vec3 -> Vec3 -> Float
        det (Vec3 ux uy _) (Vec3 vx vy _) = ux * vy - uy * vx
        v = intersectionPoint
        v0 = vecFromTuple p1
        v1 = (vecFromTuple p2 - v0) -- vector from v0 to the first end of the side
        v2 = (vecFromTuple p3 - v0) -- vector from v0 to the second end of the side
        a = (det v v2 - det v0 v2) / det v1 v2
        b = (det v v1 - det v0 v1) / det v1 v2

    guard (a > 0)
    guard (b > 0)
    guard (a + b < 1)

    Just $
      HitRecord
        { t,
          u = a,
          v = b,
          point = intersectionPoint,
          normal = Vec3 0 0 1,
          material
        }