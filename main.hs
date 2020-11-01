{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

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
            return $ vec3 0 -- skyColor direction
   in colorRec ray hitable 0

average :: Fractional a => [a] -> a
average xs = (sum xs) / genericLength xs

gammaCorrection :: Vec3 -> Vec3
gammaCorrection vec =
  vmap sqrt vec

colorForPixel :: Hitable a => Float -> Float -> Float -> Float -> Camera -> a -> IO Vec3
colorForPixel nx ny x y camera world =
  fmap (gammaCorrection . average) . parallel $ do
    subPixelX <- subPixelXs
    subPixelY <- subPixelYs
    let u = (x + subPixelX) / nx
        v = (y + subPixelY) / ny
    return $ do
      ray <- getRay u v camera
      rayColor ray world

vecToColorStr :: Vec3 -> String
vecToColorStr (Vec3 r g b) =
  intercalate " " $
    show <$> clamp 0 255 <$> floor <$> (* 255.99) <$> [r, g, b]

pixels :: Hitable a => Float -> Float -> Camera -> a -> [IO String]
pixels nx ny camera world = do
  y <- reverse [0 .. ny]
  x <- [0 .. (nx - 1)]
  return $ vecToColorStr <$> colorForPixel nx ny x y camera world

writeImage :: Hitable a => Float -> Float -> Camera -> [a] -> IO ()
writeImage nx ny camera world = do
  putStrLn $ "P3\n" ++ show (floor nx) ++ " " ++ show (floor ny) ++ "\n255"
  tree <- createTree initialTRange world
  sequence_
    . fmap (>>= putStrLn)
    $ pixels nx ny camera tree

subPixelXs :: [Float]
subPixelXs = [0, 0.1 .. 1]

subPixelYs :: [Float]
subPixelYs = [0, 0.05 .. 1]

main :: IO ()
main = do
  let nx = 200
      ny = 200
      camera = Worlds.cornellBoxCamera nx ny
  world <- Worlds.cornellBoxWorld
  writeImage nx ny camera world