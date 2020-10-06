{-# LANGUAGE NamedFieldPuns #-}

import Camera
import Control.Concurrent.ParallelIO.Global (parallel)
import Data.List
import GHC.Float
import Hitable
import Material
import Random (getRandomColor, getRandomVector)
import Ray
import Sphere
import System.Random (randomIO)
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
            c <- color' scattered hitable (depth + 1)
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

flattenListOfMaybes :: [Maybe a] -> [a]
flattenListOfMaybes xs =
  foldr flatten [] xs
  where
    flatten (Just v) acc = v : acc
    flatten Nothing acc = acc

getWorld :: IO [Sphere]
getWorld = do
  let spheres =
        [ Sphere (Vec3 0 (-1000) 0) 1000 (Lambertian (Vec3 0.5 0.5 0.5)),
          Sphere (Vec3 0 1 0) 1 (Dielectric 1.5),
          Sphere (Vec3 (-3) 1 0) 1 (Lambertian (Vec3 0.4 0.2 0.1)),
          Sphere (Vec3 3 1 0) 1 (Metal (Vec3 0.7 0.6 0.5) 0)
        ]

  fmap ((spheres ++) . flattenListOfMaybes) . sequence $ do
    a <- [(-11), (-8) .. 10]
    b <- [(-11), (-8) .. 10]
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
                color <- getRandomColor
                return . Just $ Sphere center 0.2 (Lambertian color)
            _
              | randMat < 0.95 -> do
                randVec <- getRandomVector
                metalness <- randomIO :: IO Float
                return . Just $ Sphere center 0.2 (Metal (vec3 0.5 * (vec3 1 + randVec)) (0.5 * metalness))
            _ -> do
              return . Just $ Sphere center 0.2 (Dielectric 1.5)

average :: Fractional a => [a] -> a
average xs = (sum xs) / genericLength xs

gammaCorrection :: Vec3 -> Vec3
gammaCorrection vec =
  Vec3.map sqrt vec

colorForPixel :: Hitable a => Float -> Float -> Float -> Float -> Camera -> a -> IO Vec3
colorForPixel nx ny x y camera world =
  fmap (gammaCorrection . average) . parallel $ do
    subPixelX <- [0, 0.1 .. 1]
    subPixelY <- [0, 0.2 .. 1]
    let u = (x + subPixelX) / nx
        v = (y + subPixelY) / ny
    return $ do
      ray <- getRay u v camera
      color ray world

pixels :: Hitable a => Float -> Float -> a -> [IO String]
pixels nx ny world =
  let lookFrom = (Vec3 8 1.7 5)
      lookAt = (Vec3 0 0.5 (-1))
      focusDistance = vecLength (lookFrom - lookAt)
      vup = (Vec3 0 1 0)
      camera =
        newCamera lookFrom lookAt vup 30 (nx / ny) 0.05 focusDistance
   in do
        y <- reverse [0 .. ny]
        x <- [0 .. (nx -1)]
        return . fmap (vecToLine) $ colorForPixel nx ny x y camera world

(+++) :: String -> String -> String
(+++) x y = x ++ "\n" ++ y

writeImage :: Int -> Int -> IO ()
writeImage nx ny = do
  putStrLn $ "P3" +++ (show nx ++ " " ++ show ny) +++ "255"
  world <- getWorld
  sequence_
    . fmap (>>= putStrLn)
    $ pixels (int2Float nx) (int2Float ny) world

main :: IO ()
main =
  writeImage 200 150