module FileUtils
( rmUnits
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

csvToMap :: (Num a, Read a) => String -> DM.Map String (V.Vector a)
csvToMap fileContents = DM.fromList $ zip keys values
  where
    fileLines = lines fileContents
    splitLines = fmap (splitStringAt ",") fileLines
    headerLine = filter (/= ' ') $ rmUnits (Prelude.head fileLines)
    keys = splitStringAt "," headerLine
    dataMat = M.fromLists ((fmap . fmap) read (tail splitLines) :: [[a]])
    values = [M.getCol i dataMat | i <- [0..(length headerLine - 1)]]

--readDataFile :: (Num a) => FilePath -> IO (Map String a)
--readDataFile path = []
--  where
--    fileLines = fmap lines (readFile path)
--    splitLines = fileLines >>= return . fmap (splitStringAt ",")
--    firstLine = head splitLines
