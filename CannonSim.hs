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

stepForwardEuler :: Floating a => a -> a -> a -> a
stepForwardEuler dt rhs y0 = y0 + dt * rhs


-- The advance function advances the solution forward one step in time using
-- the forward Euler method.
advance :: (Floating a) => State a -> [Force a] -> MassProperties a -> a -> State a
advance initialState forces massProps dt = newState
  where
    m = getMass massProps
    initialVelocity = velocityVector $ vel initialState
    initialPosition = positionVector $ pos initialState
    newAcceleration = V.map (1/m*) $ mconcat forces -- Data.Vector
    newVelocity = V.zipWith (stepForwardEuler dt) newAcceleration initialVelocity
    newPosition = V.zipWith (stepForwardEuler dt) newVelocity initialPosition
    newState = State (Position newPosition Velocity newVelocity Acceleration newAcceleration)

findTrajectory :: (Floating a) => State a -> a -> a -> a -> [State a]
findTrajectory initialState timeStep currentTime finalTime
  | currentTime == finalTime = [advance initialState forces massProperties timeStep]
  | otherwise = initialState : findTrajectory (advance initialState forces massProperties timeStep) timeStep (currentTime+timeStep) finalTime
    where
      velRelEarth = []
      forceAero = aerodynamicForce velRelEarth
      forceGrav = []
      massProperties = []
      forces = [forceAero, forceGrav]

main = do
  atmosphereData <- readDataFile "standard_atmosphere.dat" :: IO (DM.Map String (V.Vector Double))
  putStrLn "Enter an altitude in meters: "
  altLine <- getLine
  let alt = read altLine
  pressure <- return $ liftA2 (interp alt) (DM.lookup "Alt" atmosphereData) (DM.lookup "Press" atmosphereData)
  putStrLn ("Pressure at " ++ show alt ++ " meters is " ++ show pressure ++ " Pa.")
