{-# LANGUAGE NamedFieldPuns #-}

module Material where

import Random
import Ray
import System.Random (randomIO)
import Texture
import Vec3

data Material
  = Lambertian Texture
  | Metal {albedo :: Texture, fuzz :: Float}
  | Dielectric {refractiveIdx :: Float}
  | DiffuseLight Texture
  deriving (Show)

data ScatterRecord = ScatterRecord
  { scattered :: Ray,
    attenuation :: Vec3
  }

reflect :: Vec3 -> Vec3 -> Vec3
reflect vec normal =
  vec - vec3 (2 * (vec • normal)) * normal

refract :: Vec3 -> Vec3 -> Float -> Maybe Vec3
refract vec normal refractiveIndicesRatio =
  let uv = unitVector vec
      dt = uv • normal
      discriminant = 1 - refractiveIndicesRatio ** 2 * (1 - dt ** 2)
   in if discriminant > 0
        then
          Just $
            vec3 refractiveIndicesRatio
              * (uv - normal * vec3 dt) - normal
              * (vec3 . sqrt) discriminant
        else Nothing

schlickReflectionProbability :: Float -> Float -> Float
schlickReflectionProbability cosine refractiveIdx =
  r0 + (1 - r0) * (1 - cosine) ** 5
  where
    r0 = ((1 - refractiveIdx) / (1 + refractiveIdx)) ** 2

scatter :: Ray -> Vec3 -> Vec3 -> Material -> IO (Maybe ScatterRecord)
scatter ray point normal (Lambertian tex) = do
  rand <- getRandomVecInUnitSphere
  let target = point + normal + rand
      scattered = Ray point (target - point)
      attenuation = textureValue 0 0 point tex
  return . Just $ ScatterRecord {scattered, attenuation}
--
scatter ray point normal (Metal {albedo, fuzz}) = do
  rand <- getRandomVecInUnitSphere
  let reflected = reflect (unitVector . direction $ ray) (normal)
      scattered =
        Ray
          { origin = point,
            direction = reflected + vec3 fuzz * rand
          }
      attenuation = textureValue 0 0 point albedo
  return $
    if (direction scattered) • normal > 0
      then Just $ ScatterRecord {scattered, attenuation}
      else Nothing
--
scatter (Ray {direction}) point normal (Dielectric {refractiveIdx}) = do
  rand <- randomIO :: IO Float
  let (outwardNormal, refRatio, cosine) =
        if (direction • normal > 0)
          then
            ( - normal,
              refractiveIdx,
              refractiveIdx * (direction • normal) / vecLength direction
            )
          else
            ( normal,
              1 / refractiveIdx,
              - (direction • normal) / vecLength direction
            )
      reflectionProbability = schlickReflectionProbability cosine refractiveIdx
  return . Just $
    case refract direction outwardNormal refRatio of
      Just refracted
        | rand > reflectionProbability ->
          ScatterRecord
            { scattered = Ray point refracted,
              attenuation = Vec3 1 1 1
            }
      _ ->
        ScatterRecord
          { scattered = Ray point (reflect direction normal),
            attenuation = Vec3 1 1 1
          }
--
scatter ray point normal (DiffuseLight _) =
  return Nothing

emit :: Float -> Float -> Vec3 -> Material -> Vec3
emit u v point (DiffuseLight texture) =
  textureValue u v point texture
emit u v point _ =
  vec3 0
