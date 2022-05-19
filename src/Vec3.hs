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
  vec1 + vec2 = Vec3 (vec1.x + vec2.x) (vec1.y + vec2.y) (vec1.z + vec2.z)
  vec1 - vec2 = Vec3 (vec1.x - vec2.x) (vec1.y - vec2.y) (vec1.z - vec2.z)
  vec1 * vec2 = Vec3 (vec1.x * vec2.x) (vec1.y * vec2.y) (vec1.z * vec2.z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional Vec3 where
  vec1 / vec2 = Vec3 (vec1.x / vec2.x) (vec1.y / vec2.y) (vec1.z / vec2.z)
  fromRational a = (Vec3 (fromRational a) (fromRational a) (fromRational a))

instance Floating Vec3 where
  pi = vec3 pi
  exp = Vec3.map exp
  log = Vec3.map log
  sin = Vec3.map sin
  cos = Vec3.map cos
  asin = Vec3.map asin
  acos = Vec3.map acos
  atan = Vec3.map atan
  sinh = Vec3.map sinh
  cosh = Vec3.map cosh
  asinh = Vec3.map asinh
  acosh = Vec3.map acosh
  atanh = Vec3.map atanh

instance HasField "map" Vec3 ((Float -> Float) -> Vec3) where
  getField = flip Vec3.map

map :: (Float -> Float) -> Vec3 -> Vec3
map f vec = Vec3 (f vec.x) (f vec.y) (f vec.z)

instance HasField "normalize" Vec3 Vec3 where
  getField v = v / (vec3 $ vecLength v)

instance HasField "vecLength" Vec3 Float where
  getField = vecLength

instance HasField "squaredLength" Vec3 Float where
  getField = squaredLength

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
Vec3 x y z `dot` Vec3 x' y' z' =
  x * x' + y * y' + z * z'

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x y z) (Vec3 x' y' z') =
  Vec3
    (y * z' - z * y')
    (z * x' - x * z')
    (x * y' - y * x')

-- linear interpolation
lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp start end t = (vec3 (1 - t) * start) + (vec3 t * end)
