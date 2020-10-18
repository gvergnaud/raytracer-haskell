module Texture where

import Vec3

data Texture
  = ConstantTexture Vec3
  | CheckedTexture Texture Texture
  deriving (Show)

textureValue :: Float -> Float -> Vec3 -> Texture -> Vec3
textureValue _ _ _ (ConstantTexture color) =
  color
textureValue u v p@(Vec3 x y z) (CheckedTexture even odd) =
  let sines = sin (10 * x) * sin (10 * y) * sin (10 * z)
   in if (sines < 0)
        then textureValue u v p even
        else textureValue u v p odd
