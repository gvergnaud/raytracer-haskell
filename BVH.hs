{-# LANGUAGE NamedFieldPuns #-}

module BVH where

import AABB
import Hitable

data Node a b = Node
  { box :: AABB,
    left :: a,
    right :: b
  }

instance (Hitable a, Hitable b) => Hitable (Node a b) where
  boundingBox tMin tMax (Node {box}) =
    Just box

  hit ray tMin tMax (Node {box, left, right}) =
    if hitAABB ray tMin tMax box
      then case (hit ray tMin tMax left, hit ray tMin tMax right) of
        (Just leftRec@(HitRecord {t = leftT}), Just rightRec@(HitRecord {t = rightT})) ->
          Just $ if (leftT < rightT) then leftRec else rightRec
        (Just record, _) -> Just record
        (_, Just record) -> Just record
        _ -> Nothing
      else Nothing
