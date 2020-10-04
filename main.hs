{-# LANGUAGE NamedFieldPuns #-}

import Camera
import Control.Monad
import Data.List
import GHC.Float
import Hitable
import Material
import Random
import Ray
import Sphere
import Vec3

blueSky :: Vec3
blueSky = Vec3 0.5 0.7 1

white :: Vec3
white = vec3 1

color' :: Hitable a => Ray -> a -> Float -> IO Vec3
color' ray@(Ray {direction}) hitable depth =
  case hit ray 0.001 1000000 hitable of
    Just (HitRecord {point, normal, material}) -> do
      maybeRec <- scatter ray point normal material
      case maybeRec of
        Just (ScatterRecord {scattered, attenuation})
          | depth < 50 -> do
            c <- color' scattered world (depth + 1)
            return $ c * attenuation
        _ -> return $ vec3 0
    Nothing ->
      let unitDirection = unitVector direction
          t = 0.5 * getY unitDirection + 1
       in return $ lerp white blueSky t

color :: Hitable a => Ray -> a -> IO Vec3
color ray hitable =
  color' ray hitable 0

vecToLine :: Vec3 -> String
vecToLine (Vec3 r g b) =
  intercalate " " . fmap (show . floor . (* 255.99)) $ [r, g, b]

world :: [Sphere]
world =
  [ Sphere (Vec3 0 0 (-1.5)) 0.5 (Lambertian (Vec3 0.8 0.3 0.3)),
    Sphere (Vec3 0 (-100.5) (-1)) 100 (Lambertian (Vec3 0.5 0.5 0.5)),
    Sphere (Vec3 1 0 (-2)) 0.5 (Metal (Vec3 0.8 0.2 0.2) 0.05),
    Sphere (Vec3 (-0.75) 0 (-1)) 0.5 (Dielectric 1.5)
  ]

camera =
  Camera
    (Vec3 0 0 0)
    (Vec3 (-2) (-1) (-1))
    (Vec3 4 0 0)
    (Vec3 0 2 0)

average :: Fractional a => [a] -> a
average xs = (sum xs) / genericLength xs

gammaCorrection :: Vec3 -> Vec3
gammaCorrection vec =
  Vec3.map sqrt vec

allLines :: Float -> Float -> IO [String]
allLines nx ny =
  let getAvgColor x y =
        fmap average $
          sequence $ do
            subPixelX <- [0.1, 0.3 .. 0.9]
            subPixelY <- [0.1, 0.3 .. 0.9]
            let u = (x + subPixelX) / nx
            let v = (y + subPixelY) / ny
            let ray = getRay u v camera
            [fmap gammaCorrection $ (color ray world)]
   in sequence $ do
        y <- reverse [0 .. ny]
        x <- [0 .. (nx -1)]
        [fmap vecToLine $ getAvgColor x y]

(+++) :: String -> String -> String
(+++) x y = x ++ "\n" ++ y

writeImage :: Int -> Int -> IO ()
writeImage nx ny = do
  colorLines <- allLines (int2Float nx) (int2Float ny)
  let image =
        "P3"
          +++ (show nx ++ " " ++ show ny)
          +++ "255"
          +++ (intercalate "\n" colorLines)
  writeFile "./img.ppm" image

main :: IO ()
main =
  writeImage 200 100