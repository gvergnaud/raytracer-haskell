module Vec3 where

data Vec3 = Vec3
  { getX :: Float,
    getY :: Float,
    getZ :: Float
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
  exp = vmap exp
  log = vmap log
  sin = vmap sin
  cos = vmap cos
  asin = vmap asin
  acos = vmap acos
  atan = vmap atan
  sinh = vmap sinh
  cosh = vmap cosh
  asinh = vmap asinh
  acosh = vmap acosh
  atanh = vmap atanh

vmap :: (Float -> Float) -> Vec3 -> Vec3
vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

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
(•) :: Vec3 -> Vec3 -> Float
(•) (Vec3 a b c) (Vec3 x y z) =
  a * x + b * y + c * z

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 x y z) =
  Vec3
    (b * z - c * y)
    (c * x - a * z)
    (a * y - b * x)

unitVector :: Vec3 -> Vec3
unitVector v = v / (vec3 $ vecLength v)

-- linear interpolation
lerp :: Vec3 -> Vec3 -> Float -> Vec3
lerp start end t = (vec3 (1 - t) * start) + (vec3 t * end)
