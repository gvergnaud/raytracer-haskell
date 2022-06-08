module Rectangle where

import AABB (AABB (AABB))
import Control.Monad (guard)
import Hitable (HitRecord (..), Hitable (..), SomeHitable (..))
import Material (Material)
import Math (isBetween)
import Ray (Ray (..))
import Transform (flipNormal)
import Vec3 (Vec3 (..))

data Rectangle
  = XYRectangle
      { xRange :: (Float, Float),
        yRange :: (Float, Float),
        k :: Float,
        material :: Material
      }
  | XZRectangle
      { xRange :: (Float, Float),
        zRange :: (Float, Float),
        k :: Float,
        material :: Material
      }
  | YZRectangle
      { yRange :: (Float, Float),
        zRange :: (Float, Float),
        k :: Float,
        material :: Material
      }

hitRectangle ::
  Ray ->
  -- t range
  (Float, Float) ->
  -- a, b, k
  ((Float, Float), (Float, Float), Float) ->
  -- getA, getB, getK
  (Vec3 -> Float, Vec3 -> Float, Vec3 -> Float) ->
  Material ->
  -- normal
  Vec3 ->
  Maybe HitRecord
hitRectangle
  ray@(Ray {origin, direction})
  (t0, t1)
  ((a0, a1), (b0, b1), k)
  (getA, getB, getK)
  material
  normal = do
    -- p(t) = origin + t * direction
    -- k = origin.z + t * direction.z
    -- t = (k - origin.z) / direction.z
    -- touchX = origin.x + t * direction.x
    -- touchY = origin.y + t * direction.y
    -- touch = Vec3 touchX touchY k
    let t = (k - (getK origin)) / (getK direction)

    guard $ isBetween t0 t1 t

    let a = (getA origin) + (t * getA direction)
        b = (getB origin) + (t * getB direction)

    guard $ isBetween a0 a1 a && isBetween b0 b1 b

    pure
      HitRecord
        { u = (a - a0) / (a1 - a0),
          v = (b - b0) / (b1 - b0),
          t,
          material,
          point = ray.pointAtParameter t,
          normal
        }

instance Hitable Rectangle where
  -- boundingBox :: (Float, Float) -> a -> AABB
  boundingBox _ (XYRectangle {xRange = (x0, x1), yRange = (y0, y1), k}) =
    AABB (Vec3 x0 y0 (k - 0.001)) (Vec3 x1 y1 (k + 0.001))
  boundingBox _ (XZRectangle {xRange = (x0, x1), zRange = (z0, z1), k}) =
    AABB (Vec3 x0 (k - 0.001) z0) (Vec3 x1 (k + 0.001) z1)
  boundingBox _ (YZRectangle {yRange = (y0, y1), zRange = (z0, z1), k}) =
    AABB (Vec3 (k - 0.001) y0 z0) (Vec3 (k + 0.001) y1 z1)

  -- hit :: Ray -> (Float, Float) ->  Rectangle -> Maybe HitRecord
  hit ray range (XYRectangle xRange yRange z material) =
    hitRectangle ray range (xRange, yRange, z) ((.x), (.y), (.z)) material (Vec3 0 0 1)
  hit ray range (XZRectangle xRange zRange y material) =
    hitRectangle ray range (xRange, zRange, y) ((.x), (.z), (.y)) material (Vec3 0 1 0)
  hit ray range (YZRectangle yRange zRange x material) =
    hitRectangle ray range (yRange, zRange, x) ((.y), (.z), (.x)) material (Vec3 1 0 0)

data Box = Box {pmin :: Vec3, pmax :: Vec3, hitable :: SomeHitable}

createBox :: Vec3 -> Vec3 -> Material -> Box
createBox pmin pmax material =
  let xRange = (pmin.x, pmax.x)
      yRange = (pmin.y, pmax.y)
      zRange = (pmin.z, pmax.z)
      hitable =
        SomeHitable
          [ SomeHitable (XYRectangle xRange yRange pmax.z material),
            SomeHitable (flipNormal (XYRectangle xRange yRange pmin.z material)),
            SomeHitable (XZRectangle xRange zRange pmax.y material),
            SomeHitable (flipNormal (XZRectangle xRange zRange pmin.y material)),
            SomeHitable (YZRectangle yRange zRange pmax.x material),
            SomeHitable (flipNormal (YZRectangle yRange zRange pmin.x material))
          ]
   in Box {pmin, pmax, hitable}

instance Hitable Box where
  boundingBox range (Box {hitable}) = boundingBox range hitable
  hit ray range (Box {hitable}) = hit ray range hitable