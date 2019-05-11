import qualified Data.Vector as V
import qualified Data.Map as DM
import Control.Applicative
import SimUtils
import FileUtils
import Interp

main = do
  atmosphereData <- readDataFile "standard_atmosphere.dat" :: IO (DM.Map String (V.Vector Double))
  putStrLn "Enter an altitude in meters: "
  altLine <- getLine
  let alt = read altLine
  pressure <- return $ liftA2 (interp alt) (DM.lookup "Alt" atmosphereData) (DM.lookup "Press" atmosphereData)
  putStrLn ("Pressure at " ++ show alt ++ " meters is " ++ show pressure ++ " Pa.")
