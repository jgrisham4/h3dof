module Kinematics
( llaToEcef
, Position(..)
, Velocity(..)
, Acceleration(..)
) where

import           Data.Monoid
import           Data.Vector as V

newtype Position a = Position {positionVector :: Vector a} deriving (Show, Eq)
newtype Velocity a = Velocity {velocityVector :: Vector a} deriving (Show, Eq)
newtype Acceleration a = Acceleration {accelerationVector :: Vector a} deriving (Show, Eq)

data State a = State {pos :: Position a, vel :: Velocity a, acc :: Acceleration a}

instance (Eq a, Num a) => Monoid (Position a) where
  mempty = Position (V.fromList [0, 0, 0])
  mappend (Position p1) (Position p2) = Position (V.zipWith (+) p1 p2)

instance (Eq a, Num a) => Monoid (Velocity a) where
  mempty = Velocity (V.fromList [0, 0, 0])
  mappend (Velocity v1) (Velocity v2) = Velocity (V.zipWith (+) v1 v2)

instance (Eq a, Num a) => Monoid (Acceleration a) where
  mempty = Acceleration (V.fromList [0, 0, 0])
  mappend (Acceleration a1) (Acceleration a2) = Acceleration (V.zipWith (+) a1 a2)

llaToEcef :: Floating a => a -> a -> a -> Position a
llaToEcef lat lon alt = Position $ V.fromList [x, y, z]
  where
    x = (n + alt) * cos lat * cos lon
    y = (n + alt) * cos lat * sin lon
    z = ((1 - e^2) * n + alt) * sin lat
    n = a / sqrt (1 - e^2 * sin (lat^2))
    a = 6378137
    f = 1 / 298.257223563
    b = a * (1 - f)
    e = sqrt ((a^2 - b^2) / a^2)
