module Environment
( gasConstantAir
, specificHeatRatio
, temperature
, pressure
) where
--, density
--, speedOfSound

-- R_air = 287.0 J / (kg * K)
gasConstantAir = 287 :: (RealFloat a => a)

-- gamma = cp / cv
specificHeatRatio = 1.4 :: (RealFloat a => a)

-- Returns temperature in Kelvin when provided with altitude in meters.
temperature :: RealFloat a => a -> a
temperature h
  | h <= 11000 = 288.15 - 0.0065 * h
  | otherwise  = 216.0

-- Returns pressure in Pascals when provided with altitude in meters.
pressure :: RealFloat a => a -> a
pressure h
  | h <= 11000 = 101325 * (temperature h / 288.15) ** (5.2559)
  | otherwise  = 22630 * (exp (-0.00015769 * (h - 11000)))

-- Returns density in kg/m^3 when provided with altitude in meters.
density :: RealFloat a => a -> a
density h = (pressure h) / (gasConstantAir * (temperature h))

-- Returns speed of sound in m/s when provided with altitude in meters.
--speedOfSound :: RealFloat a => a -> a
--speedOfSound h = sqrt (specificHeatRatio * gasConstantAir * (temperature h))
