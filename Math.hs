module Math where

isBetween :: Float -> Float -> Float -> Bool
isBetween min max value = value > min && value < max

clamp :: Ord a => a -> a -> a -> a
clamp vmin vmax value =
  min (max vmin value) vmax