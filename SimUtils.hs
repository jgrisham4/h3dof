module SimUtils
( dot
, cross
, magnitude
, linspace
) where

import           Data.Monoid
import           Data.Vector as V

dot :: Num a => Vector a -> Vector a -> a
dot v1 v2 = V.sum $ V.zipWith (*) v1 v2

cross :: Num a => Vector a -> Vector a -> Vector a
cross a b = V.fromList [ihat, jhat, khat]
  where
    ihat =   a ! 1 * b ! 2 - a ! 2 * b ! 1
    jhat = -(a ! 0 * b ! 2 - a ! 2 * b ! 0)
    khat =   a ! 0 * b ! 1 - a ! 1 * b ! 0

magnitude :: Floating a => Vector a -> a
magnitude vec = sqrt $ V.sum $ V.map (^2) vec

linspace :: Floating a => a -> a -> Int -> [a]
linspace lb ub n = [fromIntegral i * dx + lb | i <- [0..(n-1)]]
  where
    dx = (ub - lb) / fromIntegral (n - 1)
