module Worlds where

import Camera
import Material
import Random
import Sphere
import System.Random
import Vec3

blueSky :: Vec3
blueSky = Vec3 0.5 0.7 1

rgb x y z = vmap (/ 255) (Vec3 x y z)

white :: Vec3
white = vec3 1

sphereColors :: [Vec3]
sphereColors =
  [ -- Hot
    rgb 232 120 12,
    rgb 255 163 13,
    rgb 255 88 0,
    rgb 232 56 12,
    rgb 255 26 13,
    -- Cold
    rgb 103 191 226,
    rgb 69 122 191,
    rgb 78 139 191,
    rgb 242 242 242,
    rgb 143 203 217
  ]

flattenListOfMaybes :: [Maybe a] -> [a]
flattenListOfMaybes xs =
  foldr flatten [] xs
  where
    flatten (Just v) acc = v : acc
    flatten Nothing acc = acc

tutoWorld :: IO [Sphere]
tutoWorld = do
  lambColor <- getRandomItem sphereColors
  metalColor <- getRandomItem sphereColors
  let spheres =
        [ Sphere (Vec3 0 (-1000) 0) 1000 (Lambertian (Vec3 0.5 0.5 0.5)),
          Sphere (Vec3 0 1 0) 1 (Dielectric 1.5),
          Sphere (Vec3 (-3) 1 0) 1 (Lambertian lambColor),
          Sphere (Vec3 3 1 0) 1 (Metal metalColor 0)
        ]

  fmap ((spheres ++) . flattenListOfMaybes) . sequence $ do
    a <- [(-11), (-9) .. 10]
    b <- [(-11), (-9) .. 10]
    return $ do
      randX <- randomIO :: IO Float
      randZ <- randomIO :: IO Float
      let center = Vec3 (a + 0.9 * randX) 0.2 (b + 0.9 * randZ)
      if vecLength (center - (Vec3 4 0.2 0)) <= 0.0
        then return Nothing
        else do
          randMat <- randomIO :: IO Float
          case randMat of
            _
              | randMat < 0.8 -> do
                rgb <- getRandomItem sphereColors
                return . Just $ Sphere center 0.2 (Lambertian rgb)
            _
              | randMat < 0.95 -> do
                randVec <- getRandomItem sphereColors
                metalness <- randomIO :: IO Float
                return . Just $ Sphere center 0.2 (Metal (vec3 0.5 * (vec3 1 + randVec)) (0.5 * metalness))
            _ -> do
              return . Just $ Sphere center 0.2 (Dielectric 1.5)

tutoCamera :: Float -> Float -> Camera
tutoCamera nx ny =
  let lookFrom = (Vec3 8 1.7 5)
      lookAt = (Vec3 0 0.5 (-1))
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
   in newCamera lookFrom lookAt vup 30 (nx / ny) 0.02 focusDistance

snowManWorld :: IO [Sphere]
snowManWorld = do
  return $
    [ Sphere (Vec3 0 (-1000) 0) 1000 (Lambertian (Vec3 0.9 0.9 1)),
      Sphere (Vec3 0 1 (-1)) 1 (Metal (Vec3 0.9 0.9 1) 0.05),
      Sphere (Vec3 0 2.3 (-1)) 0.75 (Metal (Vec3 0.9 0.9 1) 0.05),
      Sphere (Vec3 0 3.4 (-1)) 0.5 (Metal (Vec3 0.9 0.9 1) 0.05),
      -- Balls
      Sphere (Vec3 (-1) 0.25 (-0.2)) 0.25 (Lambertian (rgb 255 30 30)), -- red
      Sphere (Vec3 2.5 1.2 0) 1.2 (Metal (rgb 94 45 138) 0.4), -- purple
      Sphere (Vec3 2 2.5 (-5)) 2.5 (Lambertian (rgb 255 202 0)), -- yellow
      Sphere (Vec3 4 2.5 4) 2.5 (Lambertian (rgb 117 117 117)), -- offscreen dark
      Sphere (Vec3 (-3.5) 2.5 4) 2.5 (Lambertian (rgb 230 14 255)) -- offscreen pink
    ]

snowManCamera :: Float -> Float -> Camera
snowManCamera nx ny =
  let lookFrom = (Vec3 0 4 4)
      lookAt = (Vec3 0 2.2 (-1))
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
   in newCamera lookFrom lookAt vup 50 (nx / ny) 0.07 focusDistance