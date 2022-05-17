module Texture where

import Noise
import Vec3

data Texture
  = ConstantTexture Vec3
  | CheckedTexture Texture Texture
  | NoiseTexture Float
  deriving (Show)

textureValue :: Float -> Float -> Vec3 -> Texture -> Vec3
textureValue _ _ _ (ConstantTexture color) =
  color
textureValue u v p@(Vec3 x y z) (CheckedTexture even odd) =
  let sines = sin (10 * x) * sin (10 * y) * sin (10 * z)
   in if (sines < 0)
        then textureValue u v p even
        else textureValue u v p odd
textureValue u v p (NoiseTexture scale) =
  vec3 (noise3D (vec3 scale * p))
