module Vec3 where

import GHC.Records (HasField (..))

data Vec3 = Vec3
  { x :: Float,
    y :: Float,
    z :: Float
  }
  deriving (Show, Eq)

vec3 :: Float -> Vec3
vec3 f = Vec3 f f f

instance Num Vec3 where
  (Vec3 a b c) + (Vec3 d e f) = Vec3 (a + d) (b + e) (c + f)
  (Vec3 a b c) - (Vec3 d e f) = Vec3 (a - d) (b - e) (c - f)
  (Vec3 a b c) * (Vec3 d e f) = Vec3 (a * d) (b * e) (c * f)
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional Vec3 where
  (Vec3 a b c) / (Vec3 d e f) = Vec3 (a / d) (b / e) (c / f)
  fromRational a = (Vec3 (fromRational a) (fromRational a) (fromRational a))

instance Floating Vec3 where
  pi = vec3 pi
  exp vec = vec.map exp
  log vec = vec.map log
  sin vec = vec.map sin
  cos vec = vec.map cos
  asin vec = vec.map asin
  acos vec = vec.map acos
  atan vec = vec.map atan
  sinh vec = vec.map sinh
  cosh vec = vec.map cosh
  asinh vec = vec.map asinh
  acosh vec = vec.map acosh
  atanh vec = vec.map atanh

instance HasField "map" Vec3 ((Float -> Float) -> Vec3) where
  getField vec f = Vec3 (f vec.x) (f vec.y) (f vec.z)

instance HasField "normalize" Vec3 Vec3 where
  getField v = v / (vec3 $ vecLength v)

vmin :: Vec3 -> Vec3 -> Vec3
vmin (Vec3 x y z) (Vec3 x' y' z') =
  Vec3 (min x x') (min y y') (min z z')

vmax :: Vec3 -> Vec3 -> Vec3
vmax (Vec3 x y z) (Vec3 x' y' z') =
  Vec3 (max x x') (max y y') (max z z')

squaredLength :: Vec3 -> Float
squaredLength (Vec3 x y z) = (x ** 2) + (y ** 2) + (z ** 2)

vecLength :: Vec3 -> Float
vecLength = sqrt . squaredLength

-- dot product
dot :: Vec3 -> Vec3 -> Float
Vec3 a b c `dot` Vec3 x y z =
  a * x + b * y + c * z

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 x y z) =
  Vec3
    (b * z - c * y)
    (c * x - a * z)
    (a * y - b * x)

-- linear interpolation
lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp start end t = (vec3 (1 - t) * start) + (vec3 t * end)
