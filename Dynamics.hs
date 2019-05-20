module Dynamics
( Force(..)
) where

import           Data.Monoid
import           Data.Vector as V

newtype Force a = Force (Vector a) deriving (Show, Eq)
newtype Mass a = Mass a deriving (Show, Eq)

-- Monoid instance for the Force type so that we can sum forces easily
instance (Eq a, Num a) => Monoid (Force a) where
  mempty  = Force (V.fromList [0, 0, 0])
  mappend (Force v1) (Force v2) = Force (V.zipWith (+) v1 v2)
