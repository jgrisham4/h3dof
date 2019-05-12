module SimUtils
( dot
, cross
, magnitude
, Position(..)
, Velocity(..)
, Acceleration(..)
, Force(..)
) where

import           Data.Monoid
import           Data.Vector as V
import           System.IO

-- Kinematic state types
newtype Position a = Position (Vector a) deriving (Show, Eq) -- TODO: Define instance of Num typeclass
newtype Velocity a = Velocity (Vector a) deriving (Show, Eq)
newtype Acceleration a = Acceleration (Vector a) deriving (Show, Eq)
data State a = State (Position a) (Velocity a) (Acceleration a)

newtype Force a = Force (Vector a) deriving (Show, Eq)

-- Monoid instance for the Force type so that we can sum forces easily
instance (Eq a, Num a) => Monoid (Force a) where
  mempty  = Force (V.fromList [0, 0, 0])
  mappend (Force v1) (Force v2) = Force (V.zipWith (+) v1 v2)

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
linspace lb ub n = [(fromIntegral i) * dx + lb | i <- [0..(n-1)]]
  where
    dx = (ub - lb) / (fromIntegral (n - 1))
