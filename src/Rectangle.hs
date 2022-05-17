module Rectangle where

import AABB
import Control.Monad (guard)
import Hitable
import Material
import Math
import Ray (Ray (..), pointAtParameter)
import Transform
import Vec3

data Rectangle
  = XYRectangle
      { xRange :: (Float, Float),
        yRange :: (Float, Float),
        k :: Float,
        xyRectMaterial :: Material
      }
  | XZRectangle
      { xRange :: (Float, Float),
        zRange :: (Float, Float),
        k :: Float,
        xzRectMaterial :: Material
      }
  | YZRectangle
      { yRange :: (Float, Float),
        zRange :: (Float, Float),
        k :: Float,
        yzRectMaterial :: Material
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

    return $
      HitRecord
        { u = (a - a0) / (a1 - a0),
          v = (b - b0) / (b1 - b0),
          t,
          material,
          point = pointAtParameter t ray,
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

data Box = Box {pmin :: Vec3, pmax :: Vec3, hitable :: [SomeHitable]}

createBox :: Vec3 -> Vec3 -> Material -> Box
createBox pmin@(Vec3 xmin ymin zmin) pmax@(Vec3 xmax ymax zmax) material =
  let xRange = (xmin, xmax)
      yRange = (ymin, ymax)
      zRange = (zmin, zmax)
      hitable =
        [ SomeHitable (XYRectangle xRange yRange zmax material),
          SomeHitable (flipNormal (XYRectangle xRange yRange zmin material)),
          SomeHitable (XZRectangle xRange zRange ymax material),
          SomeHitable (flipNormal (XZRectangle xRange zRange ymin material)),
          SomeHitable (YZRectangle yRange zRange xmax material),
          SomeHitable (flipNormal (YZRectangle yRange zRange xmin material))
        ]
   in Box {pmin, pmax, hitable}

instance Hitable Box where
  boundingBox range (Box {hitable}) = boundingBox range hitable
  hit ray range (Box {hitable}) = hit ray range hitable