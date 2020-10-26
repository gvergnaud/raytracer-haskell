{-# LANGUAGE NamedFieldPuns #-}

module Transform where

import Hitable

newtype FlipNormal a = FlipNormal a

instance Hitable a => Hitable (FlipNormal a) where
  boundingBox range (FlipNormal hitable) =
    boundingBox range hitable

  hit ray range (FlipNormal hitable) = do
    (HitRecord {t, u, v, point, normal, material}) <- hit ray range hitable
    return $ HitRecord {normal = - normal, t, u, v, point, material}
