module Worlds where

import Camera (Camera, newCamera)
import Control ((|>))
import Control.Monad (join)
import Hitable (SomeHitable (..))
import Material
  ( Material (Dielectric, DiffuseLight, Lambertian, Metal),
  )
import Random (getRandomItem)
import Rectangle
  ( Rectangle (XYRectangle, XZRectangle, YZRectangle),
    createBox,
  )
import Sphere (Sphere (Sphere))
import System.Random (randomIO)
import Texture (Texture (CheckedTexture, ConstantTexture))
import Transform (flipNormal, rotateY, translate)
import Triangle (Triangle (XYTriangle))
import Vec3 (Vec3 (..), map, vec3, vecLength)

blueSky = Vec3 0.5 0.7 1

rgb x y z = Vec3.map (/ 255) (Vec3 x y z)

white = vec3 1

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

tutoWorld :: IO [Sphere]
tutoWorld = do
  lambColor <- getRandomItem sphereColors
  metalColor <- getRandomItem sphereColors
  let floorSphere =
        Sphere
          (Vec3 0 (-1000) 0)
          1000
          (Lambertian (CheckedTexture (ConstantTexture white) (ConstantTexture (Vec3 0.1 0.1 0.1))))
  let spheres =
        [ floorSphere,
          Sphere (Vec3 0 1 0) 1 (Dielectric 1.5),
          Sphere (Vec3 (-3) 1 0) 1 (Lambertian (ConstantTexture lambColor)),
          Sphere (Vec3 3 1 0) 1 (Metal (ConstantTexture metalColor) 0)
        ]

  spheresList <- sequence do
    a <- [(-11), (-9) .. 10]
    b <- [(-11), (-9) .. 10]
    pure do
      randX <- randomIO
      randZ <- randomIO
      let center = Vec3 (a + 0.9 * randX) 0.2 (b + 0.9 * randZ)
      if vecLength (center - (Vec3 4 0.2 0)) <= 0.0
        then pure []
        else
          (randomIO @Float) >>= \case
            n | n < 0.8 -> do
              rgb <- getRandomItem sphereColors
              pure [Sphere center 0.2 (Lambertian (ConstantTexture rgb))]
            n | n < 0.95 -> do
              randVec <- getRandomItem sphereColors
              metalness <- randomIO :: IO Float
              let texture = ConstantTexture (vec3 0.5 * (vec3 1 + randVec))
                  metal = (Metal texture (0.5 * metalness))
                  sphere = Sphere center 0.2 metal
              pure [sphere]
            _ ->
              pure [Sphere center 0.2 (Dielectric 1.5)]

  pure (spheres ++ join spheresList)

tutoCamera :: Float -> Float -> Camera
tutoCamera nx ny =
  let lookFrom = (Vec3 8 1.7 5)
      lookAt = (Vec3 0 0.5 (-1))
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
   in newCamera lookFrom lookAt vup 30 (nx / ny) 0.02 focusDistance

snowManWorld :: IO [Sphere]
snowManWorld = do
  pure
    [ Sphere (Vec3 0 (-1000) 0) 1000 (Lambertian (ConstantTexture (Vec3 0.9 0.9 1))),
      Sphere (Vec3 0 1 (-1)) 1 (Metal (ConstantTexture (Vec3 0.9 0.9 1)) 0.05),
      Sphere (Vec3 0 2.3 (-1)) 0.75 (Metal (ConstantTexture (Vec3 0.9 0.9 1)) 0.05),
      Sphere (Vec3 0 3.4 (-1)) 0.5 (Metal (ConstantTexture (Vec3 0.9 0.9 1)) 0.05),
      -- Balls
      Sphere (Vec3 (-1) 0.25 (-0.2)) 0.25 (Lambertian (ConstantTexture (rgb 255 30 30))), -- red
      Sphere (Vec3 2.5 1.2 0) 1.2 (Metal (ConstantTexture (rgb 94 45 138)) 0.4), -- purple
      Sphere (Vec3 2 2.5 (-5)) 2.5 (Lambertian (ConstantTexture (rgb 255 202 0))), -- yellow
      Sphere (Vec3 4 2.5 4) 2.5 (Lambertian (ConstantTexture (rgb 117 117 117))), -- offscreen dark
      Sphere (Vec3 (-3.5) 2.5 4) 2.5 (Lambertian (ConstantTexture (rgb 230 14 255))) -- offscreen pink
    ]

snowManCamera :: Float -> Float -> Camera
snowManCamera nx ny =
  let lookFrom = (Vec3 0 4 4)
      lookAt = (Vec3 0 2.2 (-1))
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
   in newCamera lookFrom lookAt vup 50 (nx / ny) 0.07 focusDistance

pandaWorld :: IO [Sphere]
pandaWorld = do
  let white = Vec3 0.95 0.95 0.95
      black = Vec3 0.06 0.06 0.06
      nose position =
        [ Sphere (position + (Vec3 0 1.8 (0.5))) 0.3 (Lambertian (ConstantTexture white)),
          Sphere (position + (Vec3 0 1.8 (0.8))) 0.1 (Lambertian (ConstantTexture black)),
          Sphere (position + (Vec3 (-0.05) 1.8 (0.8))) 0.07 (Lambertian (ConstantTexture black)),
          Sphere (position + (Vec3 0.05 1.8 (0.8))) 0.07 (Lambertian (ConstantTexture black))
        ]
      eyes position =
        [ -- eyes fur
          Sphere (position + (Vec3 (-0.21) 2 (0.55))) 0.13 (Lambertian (ConstantTexture black)),
          Sphere (position + (Vec3 0.21 2 (0.55))) 0.13 (Lambertian (ConstantTexture black)),
          -- pupil
          Sphere (position + (Vec3 (-0.21) 2 (0.68))) 0.045 (Metal (ConstantTexture (vec3 0.06)) 0.05),
          Sphere (position + (Vec3 0.21 2 (0.68))) 0.045 (Metal (ConstantTexture (vec3 0.06)) 0.05),
          -- black fur
          Sphere (position + (Vec3 (-0.27) 1.93 (0.55))) 0.1 (Lambertian (ConstantTexture black)),
          Sphere (position + (Vec3 0.27 1.93 (0.55))) 0.1 (Lambertian (ConstantTexture black))
        ]
      ears =
        [ Sphere (Vec3 (-0.7) 2.75 (-0.3)) 0.3 (Lambertian (ConstantTexture black)),
          Sphere (Vec3 0.7 2.75 (-0.3)) 0.3 (Lambertian (ConstantTexture black))
        ]
  pure $
    [ Sphere (Vec3 0 (-1000) 0) 1000 (Lambertian (ConstantTexture (Vec3 0.9 0.9 0.95))),
      -- offscreen ball
      Sphere (Vec3 4 1.5 (3)) 2.5 (Lambertian (ConstantTexture (vec3 0.5))),
      -- body
      Sphere (Vec3 0 0.3 (-1)) 1.5 (Metal (ConstantTexture black) 0.1),
      -- head
      Sphere (Vec3 0 2 (-0.3)) 0.9 (Lambertian (ConstantTexture white)),
      -- shoulders
      Sphere (Vec3 (-1.05) 0.6 (-0.3)) 0.5 (Lambertian (ConstantTexture black)),
      Sphere (Vec3 1.05 0.6 (-0.3)) 0.5 (Lambertian (ConstantTexture black))
    ]
      ++ ears
      ++ eyes (vec3 0)
      ++ nose (Vec3 0 (-0.1) 0)

pandaCamera :: Float -> Float -> Camera
pandaCamera nx ny =
  let lookFrom = (Vec3 0 2 3)
      lookAt = (Vec3 0 1.8 (0.5))
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
   in newCamera lookFrom lookAt vup 45 (nx / ny) 0.07 focusDistance

lightWorld :: IO [SomeHitable]
lightWorld =
  pure
    [ SomeHitable (Sphere (Vec3 0 (-1000) 0) 1000 (Lambertian (ConstantTexture (Vec3 0.9 0.9 0.95)))),
      SomeHitable (Sphere (Vec3 0 (2) 0) 2 (Lambertian (ConstantTexture (Vec3 0.3 0.9 0.95)))),
      -- S $ Sphere (Vec3 0 (7) 0) 1 (DiffuseLight $ ConstantTexture (vec3 4)),
      SomeHitable (XYRectangle (3, 5) (1, 3) (-2) (DiffuseLight (ConstantTexture (vec3 4))))
    ]

lightCamera :: Float -> Float -> Camera
lightCamera nx ny =
  let lookFrom = (Vec3 8 1.7 3)
      lookAt = (Vec3 0 2 0)
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
   in newCamera lookFrom lookAt vup 50 (nx / ny) 0 focusDistance

cornellBoxWorld :: IO [SomeHitable]
cornellBoxWorld = do
  let red = Lambertian (ConstantTexture (Vec3 0.65 0.05 0.05))
      white = Lambertian (ConstantTexture (vec3 0.73))
      green = Lambertian (ConstantTexture (Vec3 0.12 0.45 0.15))
      light = DiffuseLight (ConstantTexture (vec3 15))
  pure
    [ SomeHitable
        ( YZRectangle (0, 555) (0, 555) 555 green
            |> flipNormal
        ),
      SomeHitable
        (YZRectangle (0, 555) (0, 555) 0 red),
      SomeHitable
        (XZRectangle (213, 343) (227, 332) 554 light),
      SomeHitable
        ( XZRectangle (0, 555) (0, 555) 555 white
            |> flipNormal
        ),
      SomeHitable
        (XZRectangle (0, 555) (0, 555) 0 white),
      SomeHitable
        ( XYRectangle (0, 555) (0, 555) 555 white
            |> flipNormal
        ),
      SomeHitable
        ( createBox (Vec3 0 0 0) (Vec3 165 165 165) white
            |> rotateY (-18)
            |> translate (Vec3 130 0 65)
        ),
      SomeHitable
        ( Sphere (Vec3 0 50 0) 50 (Dielectric 1.5)
            |> translate (Vec3 195 165 65)
        ),
      SomeHitable
        ( createBox (Vec3 0 0 0) (Vec3 165 330 165) white
            |> rotateY (15)
            |> translate (Vec3 265 0 295)
        )
    ]

cornellBoxCamera :: Float -> Float -> Camera
cornellBoxCamera nx ny =
  let lookFrom = Vec3 278 278 (-800)
      lookAt = Vec3 278 278 0
      focusDistance = 10
      aperture = 0
      fov = 40
      vup = Vec3 0 1 0
   in newCamera lookFrom lookAt vup fov (nx / ny) aperture focusDistance

triangleWorld :: IO [SomeHitable]
triangleWorld =
  pure
    [ SomeHitable (Sphere (Vec3 0 (-1000) 0) 1000 (Lambertian (ConstantTexture (Vec3 0.9 0.9 1)))),
      --
      SomeHitable (XYTriangle (-1, 1) (0, 3) (1, 1) (-1.5) (Metal (ConstantTexture (Vec3 0.9 0.9 1)) 0.05)),
      -- Balls
      SomeHitable (Sphere (Vec3 (-1) 0.25 (-0.2)) 0.25 (Lambertian (ConstantTexture (rgb 255 30 30)))), -- red
      SomeHitable (Sphere (Vec3 2.5 1.2 0) 1.2 (Metal (ConstantTexture (rgb 94 45 138)) 0.4)), -- purple
      SomeHitable (Sphere (Vec3 2 2.5 (-5)) 2.5 (Lambertian (ConstantTexture (rgb 255 202 0)))), -- yellow
      SomeHitable (Sphere (Vec3 4 2.5 4) 2.5 (Lambertian (ConstantTexture (rgb 117 117 117)))), -- offscreen dark
      SomeHitable (Sphere (Vec3 (-3.5) 2.5 4) 2.5 (Lambertian (ConstantTexture (rgb 230 14 255)))) -- offscreen pink
    ]

triangleCamera :: Float -> Float -> Camera
triangleCamera nx ny =
  let lookFrom = (Vec3 0 4 4)
      lookAt = (Vec3 0 2.2 (-1))
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
   in newCamera lookFrom lookAt vup 50 (nx / ny) 0.07 focusDistance