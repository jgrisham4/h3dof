module Quaternion
( Quaternion(..)
, quatMultiply
, quatAngleAxis
, quatFromVec
, vecFromQuat
, normalizeQuat
) where

import           Data.Vector

{-
This module contains functions for use with quaternions.
-}

data Quaternion a = Quaternion a a a a deriving (Eq,Show)

quatMultiply :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
quatMultiply (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion a3 b3 c3 d3
  where
    a3 = a1*a2 - b1*b2 - c1*c2 - d1*d2
    b3 = a1*b2 + b1*a2 + c1*d2 - d1*c2
    c3 = a1*c2 - b1*d2 + c1*a2 + d1*b2
    d3 = a1*d2 + b1*c2 - c1*b2 + d1*a2

quatAngleAxis :: (Floating a) => a -> Vector a -> Quaternion a
quatAngleAxis angle axis = Quaternion q1 q2 q3 q4
  where
    sinangle = sin(angle/2.0)
    cosangle = cos(angle/2.0)
    q1 = cos(angle/2.0)
    q2 = axis ! 0 * sinangle
    q3 = axis ! 1 * sinangle
    q4 = axis ! 2 * sinangle

quatConjugate :: (Num a) => Quaternion a -> Quaternion a
quatConjugate (Quaternion q1 q2 q3 q4) = Quaternion q1 (-q2) (-q3) (-q4)

quatFromVec :: (Num a) => Vector a -> Quaternion a
quatFromVec v = Quaternion 0 (v!0) (v!1) (v!2)

vecFromQuat :: (Num a) => Quaternion a -> Vector a
vecFromQuat (Quaternion q1 q2 q3 q4) = fromList [q2, q3, q4]

quatNorm :: (Floating a) => Quaternion a -> a
quatNorm (Quaternion q1 q2 q3 q4) = sqrt $ q1^2 + q2^2 + q3^2 + q4^2

normalizeQuat :: (Floating a) => Quaternion a -> Quaternion a
normalizeQuat (Quaternion q1 q2 q3 q4) = Quaternion (q1/qnorm) (q2/qnorm) (q3/qnorm) (q4/qnorm)
  where
    qnorm = quatNorm (Quaternion q1 q2 q3 q4)

transformCoordinates :: (Num a) => Quaternion a -> Vector a -> Vector a
transformCoordinates q_ba v_a = vecFromQuat $ quatMultiply (quatMultiply q_ba q_a) (quatConjugate q_ba)
  where
    q_a = quatFromVec v_a

rotateVector :: (Num a) => Quaternion a -> Vector a -> Vector a
rotateVector q v = vecFromQuat $ quatMultiply (quatMultiply (quatConjugate q) q_v) q
  where
    q_v = quatFromVec v
