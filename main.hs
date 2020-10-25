{-# LANGUAGE NamedFieldPuns #-}

import BVH
import Camera
import Control.Concurrent.ParallelIO.Global (parallel)
import Data.List (genericLength, intercalate)
import GHC.Float (int2Float)
import Hitable
import Material
import Math
import Ray
import Vec3
import qualified Worlds

initialTRange :: (Float, Float)
initialTRange = (0.001, 1000000)

skyColor :: Vec3 -> Vec3
skyColor direction =
  let unitDirection = unitVector direction
      t = 0.5 * getY unitDirection + 1
   in lerp Worlds.white Worlds.blueSky t

rayColor :: Hitable a => Ray -> a -> IO Vec3
rayColor ray hitable =
  let colorRec ray@(Ray {direction}) hitable depth =
        case hit ray initialTRange hitable of
          Just (HitRecord {u, v, point, normal, material}) -> do
            maybeRec <- scatter ray point normal material
            let emitted = emit u v point material
            case maybeRec of
              Just (ScatterRecord {scattered, attenuation})
                | depth < 50 -> do
                  c <- colorRec scattered hitable (depth + 1)
                  return $ emitted + c * attenuation
              _ -> return $ emitted
          Nothing ->
            return $ vec3 0
   in -- return $ skyColor direction
      colorRec ray hitable 0

average :: Fractional a => [a] -> a
average xs = (sum xs) / genericLength xs

gammaCorrection :: Vec3 -> Vec3
gammaCorrection vec =
  vmap sqrt vec

colorForPixel :: Hitable a => Float -> Float -> Float -> Float -> Camera -> a -> IO Vec3
colorForPixel nx ny x y camera world =
  fmap (gammaCorrection . average) . parallel $ do
    subPixelX <- [0, 0.1 .. 1]
    subPixelY <- [0, 0.1 .. 1]
    let u = (x + subPixelX) / nx
        v = (y + subPixelY) / ny
    return $ do
      ray <- getRay u v camera
      rayColor ray world

vecToColorStr :: Vec3 -> String
vecToColorStr (Vec3 r g b) =
  intercalate " " . fmap (show . clamp 0 255 . floor . (* 255.99)) $ [r, g, b]

pixels :: Hitable a => Float -> Float -> a -> [IO String]
pixels nx ny world =
  let camera = Worlds.lightCamera nx ny
   in do
        y <- reverse [0 .. ny]
        x <- [0 .. (nx - 1)]
        return $ vecToColorStr <$> colorForPixel nx ny x y camera world

writeImage :: Int -> Int -> IO ()
writeImage nx ny = do
  putStrLn $ "P3" ++ "\n" ++ (show nx ++ " " ++ show ny) ++ "\n" ++ "255"
  world <- Worlds.lightWorld
  tree <- createTree initialTRange world
  sequence_
    . fmap (>>= putStrLn)
    $ pixels (int2Float nx) (int2Float ny) $ tree

main :: IO ()
main =
  writeImage 200 140