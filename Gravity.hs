module Gravity
( gravityAcceleration
) where

import SimUtils
import qualified Data.Vector as V

prodGM = 3.986005e14 -- m^3/s^2
omega_EI_E = V.fromList [0.0, 0.0, 7.292115e-5]

gravityAcceleration :: (Fractional a) => Position a -> V.Vector a
gravityAcceleration s_CE_E = (-prodGM) * s_CE_E
