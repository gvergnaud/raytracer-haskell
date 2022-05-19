module BVH where

import AABB (AABB (minVec), hitAABB, suroundingBox)
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Hitable (HitRecord (HitRecord, t), Hitable (..))
import System.Random (randomRIO)
import Vec3 (Vec3 (x, y, z))

data Tree a
  = Node
      { box :: AABB,
        left :: Tree a,
        right :: Tree a
      }
  | Leaf a

instance Show a => Show (Tree a) where
  show (Node {box, left, right}) =
    let addTab = intercalate "\n" . fmap ("\t" ++) . splitOn "\n"
     in "Node (\n" ++ "\tbox = " ++ show box ++ "\n" ++ "\tleft = " ++ (addTab . show $ left) ++ "\n" ++ "\tright = " ++ (addTab . show $ right) ++ "\n" ++ ")"
  show (Leaf x) =
    "Leaf (" ++ show x ++ ")"

instance (Hitable a) => Hitable (Tree a) where
  boundingBox range (Node {box}) = box
  boundingBox range (Leaf a) = boundingBox range a

  hit ray range (Node {box, left, right}) =
    if hitAABB ray box
      then case (hit ray range left, hit ray range right) of
        (Just leftRec@(HitRecord {t = leftT}), Just rightRec@(HitRecord {t = rightT})) ->
          Just (if (leftT < rightT) then leftRec else rightRec)
        (Just record, _) -> Just record
        (_, Just record) -> Just record
        _ -> Nothing
      else Nothing
  hit ray range (Leaf x) =
    hit ray range x

createTree :: (Hitable a) => (Float, Float) -> [a] -> IO (Tree a)
createTree range (x : []) =
  return $ Leaf x
createTree range (x : y : []) =
  let aabb = suroundingBox (boundingBox range x) (boundingBox range y)
   in return $ Node aabb (Leaf x) (Leaf y)
createTree range list
  | length list > 2 = do
      rand <- randomRIO (0, 2) :: IO Int
      let getVecPart = case rand of
            0 -> (.x)
            1 -> (.y)
            2 -> (.z)

          (left, right) =
            splitAt (length list `div` 2) . sortBy sorter $ list
            where
              sorter a b =
                if getVecPart (boundingBox range a).minVec < getVecPart (boundingBox range b).minVec
                  then GT
                  else LT

          aabb = suroundingBox (boundingBox range left) (boundingBox range right)

      leftTree <- createTree range left
      rightTree <- createTree range right
      return $ Node aabb leftTree rightTree