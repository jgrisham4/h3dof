import           Aerodynamics
import           Control.Applicative
import qualified Data.Map            as DM
import qualified Data.Vector         as V
import           Dynamics
import           FileUtils
import           Gravity
import           Interp
import           Kinematics
import           SimUtils

{-
B -> Body
A -> Air
E -> ECEF
U -> UEN
-}

stepBackwardEuler :: Floating a => a -> a -> a -> a
stepBackwardEuler dt rhs y0 = y0 + dt * rhs


-- The advance function advances the solution forward one step in time using
-- the forward Euler method.
advance :: (Floating a) => State a -> [Force a] -> Mass a -> State a
advance initialState forces = newState
  where
    newAcceleration = []
    newVelocity = []
    newPosition = []
    newState = State (Position newPosition Velocity newVelocity Acceleration newAcceleration)


main = do
  atmosphereData <- readDataFile "standard_atmosphere.dat" :: IO (DM.Map String (V.Vector Double))
  putStrLn "Enter an altitude in meters: "
  altLine <- getLine
  let alt = read altLine
  pressure <- return $ liftA2 (interp alt) (DM.lookup "Alt" atmosphereData) (DM.lookup "Press" atmosphereData)
  putStrLn ("Pressure at " ++ show alt ++ " meters is " ++ show pressure ++ " Pa.")
