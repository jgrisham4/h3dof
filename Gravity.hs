module Gravity
( gravityAcceleration
) where

import qualified Data.Vector as V
import           Kinematics
import           SimUtils

prodGM :: Floating a => a
prodGM = 3.986005e14 -- m^3/s^2

omega_EI_E :: Floating a => V.Vector a
omega_EI_E = V.fromList [0.0, 0.0, 7.292115e-5]

gravityAcceleration :: (Floating a) => Position a -> Acceleration a
gravityAcceleration s_CE_E = Acceleration $ V.zipWith (-) newton angular
  where
    vecMag = magnitude $ positionVector s_CE_E
    newton =  V.map (* (-prodGM / vecMag^3)) (positionVector s_CE_E)
    angular = cross omega_EI_E (cross omega_EI_E (positionVector s_CE_E))
