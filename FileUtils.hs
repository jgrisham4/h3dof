module FileUtils
( readDataFile
) where

import qualified Data.Map    as DM
import qualified Data.Matrix as M
import qualified Data.Text   as T
import qualified Data.Vector as V
import qualified Text.Regex  as R

splitStringAt :: String -> String -> [String]
splitStringAt char str = Prelude.map T.unpack $ T.splitOn (T.pack char) (T.pack str)

rmUnits :: String -> String
rmUnits headerLine = R.subRegex (R.mkRegex "\\(.{0,6}\\)") headerLine ""

getMapKeys :: [String] -> [String]
getMapKeys fileLines = splitStringAt "," headerLine
  where
    headerLine = filter (/= ' ') $ rmUnits (Prelude.head fileLines)

convertTextToNum :: (Num a, Read a) => [[String]] -> [[a]]
convertTextToNum splitLines = (fmap . fmap) read (tail splitLines)

getMapData :: (Num a, Read a) => [[String]] -> [V.Vector a]
getMapData splitLines = [M.getCol i dataMat | i <- [1..(length (head splitLines))]]
  where
    dataMat = M.fromLists $ convertTextToNum splitLines

csvToMap :: (Num a, Read a) => String -> DM.Map String (V.Vector a)
csvToMap fileContents = DM.fromList $ zip (getMapKeys fileLines) (getMapData splitLines)
  where
    fileLines = lines fileContents
    splitLines = fmap (splitStringAt ",") fileLines

-- This function returns a Data.Map whose keys are the column titles and
-- whose values are Data.Vectors which contain the numeric data in the columns.
-- The resulting map can be queried as follows:
-- > datamap = readDataFile "standard_atmosphere.dat"
-- > fmap (DM.lookup "Alt") datamap :: IO (Maybe (V.Vector Double))
readDataFile :: (Num a, Read a) => FilePath -> IO (DM.Map String (V.Vector a))
readDataFile path = fmap csvToMap (readFile path)
