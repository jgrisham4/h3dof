import           Control.Applicative
import qualified Data.Map            as DM
import qualified Data.Vector         as V
import           FileUtils
import           Interp
import           SimUtils

{-
B -> Body
A -> Air
E -> ECEF
U -> UEN
-}

-- Now need to define a function which steps the solution forward in time.

main = do
  atmosphereData <- readDataFile "standard_atmosphere.dat" :: IO (DM.Map String (V.Vector Double))
  putStrLn "Enter an altitude in meters: "
  altLine <- getLine
  let alt = read altLine
  pressure <- return $ liftA2 (interp alt) (DM.lookup "Alt" atmosphereData) (DM.lookup "Press" atmosphereData)
  putStrLn ("Pressure at " ++ show alt ++ " meters is " ++ show pressure ++ " Pa.")
