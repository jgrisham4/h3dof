module Aerodynamics
( aerodynamicForce
) where

aerodynamicForce :: Floating a => Velocity a -> a -> a -> Force a
aerodynamicForce (Velocity v_BA_B) rho sref = Force (V.map (*drag) direction)
  where
    vinf = magnitude v_BA_B
    q = 0.5 * rho * vinf^2
    cd = 1.5
    drag = q * sref * cd
    direction = V.map (*(-1)) v_BA_B
