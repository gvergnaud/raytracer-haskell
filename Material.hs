{-# LANGUAGE NamedFieldPuns #-}

module Material where

import Random
import Ray
import System.Random (randomIO)
import Vec3

data Material
  = Lambertian {albedo :: Vec3}
  | Metal {albedo :: Vec3, fuzz :: Float}
  | Dielectric {refractiveIdx :: Float}

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
scatter ray point normal material = do
  case material of
    (Lambertian {albedo}) -> do
      rand <- getRandomVecInUnitSphere
      let target = point + normal + rand
          scattered = Ray point (target - point)
          attenuation = albedo
      return . Just $ ScatterRecord {scattered, attenuation}
    (Metal {albedo, fuzz}) -> do
      rand <- getRandomVecInUnitSphere
      let reflected = reflect (unitVector . direction $ ray) (normal)
          scattered =
            Ray
              { origin = point,
                direction = reflected + vec3 fuzz * rand
              }
          attenuation = albedo
      return $
        if (direction scattered) • normal > 0
          then Just $ ScatterRecord {scattered, attenuation}
          else Nothing
    (Dielectric {refractiveIdx}) -> do
      rand <- randomIO :: IO Float

      let (outwardNormal, refRatio, cosine) =
            if (direction ray • normal > 0)
              then
                ( - normal,
                  refractiveIdx,
                  refractiveIdx * (direction ray • normal) / (vecLength . direction) ray
                )
              else
                ( normal,
                  1 / refractiveIdx,
                  - (direction ray • normal) / (vecLength . direction) ray
                )
          reflectionProbability = schlickReflectionProbability cosine refractiveIdx

      return . Just $
        case refract (direction ray) outwardNormal refRatio of
          Just refracted
            | rand > reflectionProbability ->
              ScatterRecord
                { scattered = Ray point refracted,
                  attenuation = Vec3 1 1 1
                }
          _ ->
            ScatterRecord
              { scattered = Ray point (reflect (direction ray) normal),
                attenuation = Vec3 1 1 1
              }
