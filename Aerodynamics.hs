module Aerodynamics
( aerodynamicForce
) where

aerodynamicForce :: Floating a => Velocity a -> a -> a -> Force a
aerodynamicForce (Velocity vRelAir) rho sref = Force (V.map (*drag) direction)
  where
    vinf = magnitude vRelAir
    q = 0.5 * rho * vinf^2
    cd = 1.5
    drag = q * sref * cd
    direction = V.map (*(-1)) vRelAir
