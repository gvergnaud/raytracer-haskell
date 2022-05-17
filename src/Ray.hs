module Ray where

import Vec3

data Ray = Ray
  { origin :: Vec3,
    direction :: Vec3
  }

pointAtParameter :: Float -> Ray -> Vec3
pointAtParameter t ray =
  ray.origin + vec3 t * ray.direction
