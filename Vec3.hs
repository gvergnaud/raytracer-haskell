module Vec3 where

data Vec3 = Vec3 Float Float Float deriving (Show, Eq)

vec3 :: Float -> Vec3
vec3 f = Vec3 f f f

getX :: Vec3 -> Float
getX (Vec3 x _ _) = x

getY :: Vec3 -> Float
getY (Vec3 _ y _) = y

getZ :: Vec3 -> Float
getZ (Vec3 _ _ z) = z

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

map :: (Float -> Float) -> Vec3 -> Vec3
map f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

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